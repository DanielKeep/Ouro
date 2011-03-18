/**
    Read, Eval, Print Loop.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Repl;

import tango.io.Console;
import tango.io.Stdout;
import tango.net.Uri;

import Path = tango.io.Path;

import ouro.Source : Source;
import ouro.sem.Eval : EvalVisitor;
import ouro.sem.ModulePool : ModulePool;
import ouro.sem.UnfixedScan : UnfixedScanVisitor;

import Sit = ouro.sit.Nodes;

// Pull in all the builtin modules & uri handlers
import ouro.sem.builtins.All;
import ouro.util.uriSchemes.All;

void repl(char[] exec, char[][] args)
{
    void emit(char[] s) { Stdout(s); }

    ModulePool mp;
    {
        Path.PathParser pp;
        pp.parse(Path.standard(exec));
        mp.addFileImportRoot(Path.normalize(Path.join(
                        pp.parent, "../src.ouro/stdlib/")));
    }
    mp.addFileImportRoot(".");

    auto lang = "ouro";
    scope eval = new EvalVisitor;
    scope unfixScan = new UnfixedScanVisitor;

    {
        // Import and compile language support.
        auto langMod = mp.load("/"~lang~"/lang", /*includeLang*/false);
        if( langMod is null )
            assert( false, "could not load /"~lang~"/lang" );
        mp.compileStmts();

        // Scan for unfixed values
        foreach( entry ; mp.entries )
            unfixScan.visitBase(entry.sit);

        // Eval loaded modules.
        foreach( entry ; mp.entries )
            eval.evalModule(entry.sit);
    }

    // Load repl module
    mp.addImportRoot("empty:");
    auto replMod = mp.load(new Uri("empty:/repl"));
    mp.compileStmts;

    void prompt()
    {
        Stdout(">>> ").flush;
    }

    void help()
    {
        Stdout
            .formatln("Commands:")
            .formatln("")
            .formatln(" .h, .help   Shows this text.")
            .formatln(" .q, .quit   Exits the REPL.")
            .formatln(" .s, .scope  Dumps the contents of the REPL scope.")
            .newline()
        ;
    }

    size_t replLine = 0;

replLoop:
    while( true )
    {
        prompt;
        char[] line;
        if( ! Cin.readln(line) )
        {
            Stdout.newline;
            break replLoop;
        }

        line = line.dup;

        ++ replLine;

        switch( line )
        {
            case ".h": case ".help":
                help;
                continue replLoop;

            case ".q": case ".quit":
                break replLoop;

            case ".s": case ".scope":
                {
                    auto keys = replMod.scop.entries.keys;
                    keys.sort;
                    foreach( k ; keys )
                    {
                        auto v = replMod.scop.entries[k];
                        Stdout.format(" {,16} : ", k);
                        if( auto f = cast(Sit.Formatter) v )
                            f.format(&emit, 0, '>', " ", null, ["R"]);
                        else
                            Stdout(v.toString);
                        Stdout.newline;
                    }
                }
                continue replLoop;

            default:
        }

        // Load & compile new statement(s).
        Sit.Value result;
        try
        {
            ModulePool.ShadowPool sp;
            {
                auto src = new Source("repl", line, replLine, 1);
                mp.loadStmt(replMod, src, sp);
                mp.compileStmts;
            }

            // Scan for unfixed values.
            foreach( stmt ; sp.stmts )
                unfixScan.visitBase(stmt.value);

            // Eval the compiled statements.  Remember the last resulting
            // value.
            foreach( stmt ; sp.stmts )
                result = eval.evalExpr(stmt.value);
        }
        catch( Exception e )
        {
            Stdout.flush;
            Stderr(e.toString).newline().flush();
            mp.clearStmts;
        }

        // Display the result
        if( result !is null && cast(Sit.NilValue) result is null )
        {
            if( auto f = cast(Sit.Formatter) result )
            {
                f.format(&emit, 0, '>', " ", null, ["R"]);
                Stdout.newline;
            }
            else
                Stdout(result.toString).newline;
        }
    }
}

void main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];
    repl(exec, args);
}

