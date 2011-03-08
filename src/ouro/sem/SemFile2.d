module ouro.parser.SemFile2;

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
import ouro.Source;
import ouro.sem.ModulePool : ModulePool;
import ouro.sit.ReprVisitor;
import ouro.util.StructuredOutput;
import ouro.util.TokenStream;

int main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    bool throwExc = false;
    bool doEval = true;

    scope eval = new Eval.EvalVisitor;
    auto repr = ReprVisitor.forStdout;
    auto errRepr = ReprVisitor.forStderr;

    Sit.Value builtin(char[] name)
    {
        return Builtins.lookupBuiltin(name);
    }

    ModulePool mp;
    {
        Path.PathParser pp;
        pp.parse(Path.standard(exec));
        mp.addFileImportRoot(Path.join(pp.parent, "import/"));
    }
    mp.addFileImportRoot(".");

    {
        // Import and compile language support
        auto langMod = mp.load("/ouro/lang");
        if( langMod is null )
            assert( false, "could not load /ouro/lang" );
        mp.compileStmts();
    }

    foreach( path ; args )
    {
        if( path == "--throw" )
            throwExc = true;

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

