/**
    Performs semantic analysis and evaluates source modules.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.parser.SemFile;

import Path = tango.io.Path;

import tango.io.Stdout;
import tango.io.device.File;
import tango.net.Uri;
import tango.sys.Environment;
import tango.text.Util : trimr;

import tango.core.tools.TraceExceptions;

import Ast      = ouro.ast.Nodes;
import Eval     = ouro.sem.Eval;
import InvokeFn = ouro.sem.InvokeFn;
import Lexer    = ouro.lexer.Lexer;
import Parser   = ouro.parser.Parser;
import Sem      = ouro.sem.Semantic;
import SemCtx   = ouro.sem.Context;
import UnfixedScan = ouro.sem.UnfixedScan;
import Sit      = ouro.sit.Nodes;

import ouro.Error : CompilerException;
import ouro.sem.ModulePool : ModulePool;
import ouro.util.StructuredOutput;

// Pull in all the builtin modules.
import ouro.sem.builtins.All;

import AstRepr = ouro.ast.ReprVisitor;
import SitRepr = ouro.sit.ReprVisitor;

int main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    bool throwExc = false;
    bool showAst = false;
    bool showSem = false;
    bool doRuntime = true;
    char[] lang = "ouro";

    scope eval = new Eval.EvalVisitor;
    scope unfixScan = new UnfixedScan.UnfixedScanVisitor;

    {
        bool nextIsLang = false;
        foreach( arg ; args )
        {
            if( nextIsLang )
            {
                lang = arg;
                nextIsLang = false;
            }
            else
            {
                switch( arg )
                {
                    case "--lang":  nextIsLang = true; break;
                    default:
                }
            }
        }
    }

    ModulePool mp;
    mp.lang = lang;
    {
        Path.PathParser pp;
        pp.parse(Path.standard(exec));
        mp.addFileImportRoot(Path.normalize(Path.join(
                        pp.parent, "../src.ouro/stdlib/")));
    }
    mp.addFileImportRoot(".");

    {
        // Import and compile language support.
        auto langMod = mp.load("/"~lang~"/lang", /*includeLang*/false);
        if( langMod is null )
            assert( false, "could not load /"~lang~"/lang" );
        mp.compileStmts();

        // Scan for unfixed values
        foreach( entry ; mp.entries )
            unfixScan.visitBase(entry.sit);
    }

    Sit.Module mainModule;
    char[] mainModulePath;
    bool nextIsImport = false;
    bool argTail = false;
    bool skipNext = false;
    char[][] mainArgs;

    foreach( path ; args )
    {
        if( skipNext )
            skipNext = false;

        else if( argTail )
            mainArgs ~= path;

        else if( nextIsImport )
        {
            nextIsImport = false;
            mp.addFileImportRoot(path);
        }
        else if( path == "--throw" )
            throwExc = true;

        else if( path == "--show-ast" )
            showAst = true;

        else if( path == "--show-sem" )
            showSem = true;

        else if( path == "--no-runtime" )
            doRuntime = false;

        else if( path == "--import" )
            nextIsImport = true;

        else if( path == "--lang" )
            skipNext = true;

        else if( path == "--" )
            argTail = true;

        else
        {
            try
            {
                auto stdPath = Path.standard(path);
                Uri modUri;

                if( stdPath.length >= 8 && stdPath[0..8] == "ouromod:" )
                {
                    // Module path; use it directly.
                    auto modPath = stdPath[8..$];
                    if( modPath[0] != '/' )
                        // Assume lazy absolute
                        modPath = '/' ~ modPath;

                    modUri = new Uri("ouromod:" ~ modPath);
                }
                else
                {
                    Uri uri;

                    // Check for a Windows absolute file path before parsing
                    // as an URI.
                    // TODO: better test; what about COMx?  LPTx?
                    version( Windows )
                        if( stdPath.length >= 2 && stdPath[1] == ':' )
                        {
                            // Probably a Windows absolute path.
                            uri = new Uri("file:///"
                                    ~ Path.normalize(stdPath));
                        }

                    if( uri is null )
                        // Try to parse as URI
                        uri = new Uri(stdPath);

                    // If there's no scheme, assume filesystem.
                    if( uri.scheme == "" )
                    {
                        auto uriPath = uri.path;
                        auto pp = Path.PathParser(uriPath);
                        if( ! pp.isAbsolute )
                        {
                            uriPath = Environment.toAbsolute(uriPath);
                            version( Windows )
                                uriPath = '/' ~ uriPath;
                        }

                        uri = new Uri("file", "", uriPath);
                    }

                    modUri = uri;
                }

                auto mod = mp.load(modUri);
                if( mod is null )
                    assert( false, "could not import "~path );
                mp.compileStmts();

                if( mainModule is null )
                {
                    mainModule = mod;
                    mainModulePath = path; // NB: As provided by user
                }

                if( showAst )
                {
                    scope repr = new AstRepr.ReprVisitor(
                            StructuredOutput.forStdout);

                    foreach( entry ; mp.entries )
                    {
                        Stdout("module ")(entry.sit.path)(" ast: ");
                        repr.visitBase(entry.ast);
                        Stdout.newline;
                    }
                }

                if( showSem )
                {
                    scope repr = new SitRepr.ReprVisitor(
                            StructuredOutput.forStdout);

                    foreach( entry ; mp.entries )
                    {
                        Stdout("module ")(entry.sit.path)(" sit: ");
                        repr.visitBase(entry.sit, true);
                        Stdout.newline;
                    }
                }
            }
            catch( CompilerException e )
            {
                Stderr(e.toString).newline().flush();

                if( throwExc )
                    throw e;

                else
                    return 1;
            }
        }
    }

    // Scan for unfixed values
    foreach( entry ; mp.entries )
        unfixScan.visitBase(entry.sit);

    if( doRuntime )
    {
        foreach( entry ; mp.entries )
            eval.evalModule(entry.sit);

        if( "main" in mainModule.exportScop.entries )
        {
            auto mainValue = mainModule.exportScop.entries["main"];
            auto mainFn = cast(Sit.CallableValue) mainValue;

            auto argValues = new Sit.Value[mainArgs.length+1];
            argValues[0] = new Sit.StringValue(null, mainModulePath);

            foreach( i,arg ; mainArgs )
                argValues[i+1] = new Sit.StringValue(null, arg);

            auto result = InvokeFn.invoke(mainFn,
                [cast(Sit.Value) new Sit.ListValue(null, argValues)]);
        }
    }

    return 0;
}

