/**
    Performs semantic analysis and evaluates source modules.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.parser.SemFile;

import Path = tango.io.Path;

import tango.io.Stdout;
import tango.io.device.File;
import tango.text.Util : trimr;

import tango.core.tools.TraceExceptions;

import Ast      = ouro.ast.Nodes;
import Builtins = ouro.sem.Builtins;
import Eval     = ouro.sem.Eval;
import Lexer    = ouro.lexer.Lexer;
import Parser   = ouro.parser.Parser;
import Sem      = ouro.sem.Semantic;
import SemCtx   = ouro.sem.Context;
import Sit      = ouro.sit.Nodes;

import ouro.Error : CompilerException;
import ouro.sem.ModulePool : ModulePool;
import ouro.util.StructuredOutput;

import AstRepr = ouro.ast.ReprVisitor;
import SitRepr = ouro.sit.ReprVisitor;

int main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    bool throwExc = false;
    bool showAst = false;
    bool showSem = false;
    bool doEval = true;

    scope eval = new Eval.EvalVisitor;

    Sit.Value builtin(char[] name)
    {
        return Builtins.lookupBuiltin(name);
    }

    ModulePool mp;
    {
        Path.PathParser pp;
        pp.parse(Path.standard(exec));
        mp.addFileImportRoot(Path.normalize(Path.join(
                        pp.parent, "../src.ouro/stdlib/")));
    }
    mp.addFileImportRoot(".");

    {
        // Import and compile language support.
        auto langMod = mp.load("/ouro/lang", /*includeLang*/false);
        if( langMod is null )
            assert( false, "could not load /ouro/lang" );
        mp.compileStmts();
    }

    foreach( path ; args )
    {
        if( path == "--throw" )
            throwExc = true;

        else if( path == "--show-ast" )
            showAst = true;

        else if( path == "--show-sem" )
            showSem = true;

        else if( path == "--no-eval" )
            doEval = false;

        else
        {
            try
            {
                auto mod = mp.load(Path.standard(path));
                if( mod is null )
                    assert( false, "could not import "~path );
                mp.compileStmts();

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

                if( doEval )
                    auto result = eval.evalModule(mod);
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

    return 0;
}

