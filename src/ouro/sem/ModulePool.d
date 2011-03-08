module ouro.sem.ModulePool;

import tango.net.Uri;

import Path = tango.io.Path;

import ouro.sem.Abort;
import ouro.sem.Context : Context;
import ouro.sem.Semantic : SemInitialVisitor;
import ouro.util.ImportRoot : ImportRoot, Resource, ImportRootSchemes;
import ouro.util.TokenStream;
import ouro.util.uriSchemes.File : FileRoot;
import ouro.Source;

import Ast = ouro.ast.Nodes;
import Sem = ouro.sem.Semantic;
import Sit = ouro.sit.Nodes;
import Builtins = ouro.sem.Builtins;
import Lexer = ouro.lexer.Lexer;
import Parser = ouro.parser.Parser;

const char[][] FileExtensions = [".ouro"];

struct ModulePool
{
    struct Entry
    {
        Uri uri;
        Source src;
        Ast.Module ast;
        Sit.Module sit;
    }

    struct Stmt
    {
        Sit.Module mod;
        Sit.Stmt stmt;
        Object ex;
    }

    ImportRoot[] importRoots;
    Entry[] entries;
    size_t[char[]] pathToEntryIdx;
    Stmt[] stmts;

    Sit.Module currentModule = null;
    size_t currentStmt = size_t.max;

    void addImportRoot(char[] uri)
    {
        auto r = ImportRootSchemes.createRootFrom(uri);
        if( r is null )
            assert( false, "Could not create root for "~uri );

        importRoots = r ~ importRoots;
    }

    void addFileImportRoot(char[] path)
    {
        importRoots = FileRoot.createForPath(path) ~ importRoots;
    }

    Sit.Module loadRelativeToModule(Sit.Module mod, char[] path)
    {
        assert( path.length > 1 );

        if( path[0] == '/' )
            return load(path);

        else
        {
            auto newPath = Path.join(mod.path, path);
            assert( newPath[0] == '/' );
            return load(newPath);
        }
    }

    Sit.Module load(char[] path)
    {
        // Is it already there?
        if( auto idxptr = (path in pathToEntryIdx) )
            return entries[*idxptr].sit;

        // Find the resource
        Resource res;
        {
            foreach( root ; importRoots )
            {
                foreach( ext ; FileExtensions )
                {
                    res = root.resolve(path~ext);
                    if( res !is null )
                        break;
                }
                if( res !is null )
                    break;
            }
    
            if( res is null )
                return null;
        }

        Entry entry;
        entry.uri = res.uri;

        // Create source
        entry.src = new Source(path, res.data);

        // Parse
        {
            scope ts = new TokenStream(entry.src, &Lexer.lexNext);
            entry.ast = Parser.parseModule(ts);
        }

        // Create module object
        entry.sit = SemInitialVisitor.createEmptyModule(entry.ast, path);

        // Done making entry.
        entries ~= entry;
        pathToEntryIdx[entry.sit.path] = entries.length-1;

        // Add to the statement pool
        injectStmts(entry.sit, entry.ast.stmts);

        // Return module object
        return entry.sit;
    }

    void compileStmts()
    {
        assert( stmts.length > 0, "no statements to compile!" );

        scope sem = new Sem.SemInitialVisitor;
        
        auto moduleFn = new Sit.FunctionValue("ouro.module",
                [Sit.Argument("path", false)], &loadModule,
                Sit.FunctionValue.Host.EvalContext.Compile);

        Context ctx;
        ctx.builtinFn = (char[] name)
        {
            if( name == "ouro.module" )
                return cast(Sit.Value) moduleFn;
            else
                return Builtins.lookupBuiltin(name);
        };

        bool failedStmt = false;
        bool successStmt = false;

        do
        {
            failedStmt = false;
            successStmt = false;

processStmt:
            // Use a for because we might mutate stmts on the way.
            for( size_t i=0; i<stmts.length; i++ )
            {
                auto stmt = &stmts[i];
                currentStmt = i;
                currentModule = stmt.mod;
                scope(exit)
                {
                    currentStmt = size_t.max;
                    currentModule = null;
                }

                if( stmt.stmt.value !is null )
                    // Already done this one!  YAY!
                    continue processStmt;

                // Try to process
                if( stmt.stmt.expr is null )
                {
                    auto subCtx = ctx.dup;
                    auto astStmt = stmt.stmt.astNode;

                    subCtx.stmt = &stmt.stmt;
                    subCtx.scop = stmt.mod.scop;

                    try
                    {
                        stmt.stmt.expr = sem.visitExpr(astStmt, &subCtx);
                    }
                    catch( NonFatalAbort nf )
                    {
                        stmt.ex = nf;
                        failedStmt = true;
                    }
                }

                // Semantic failed
                if( stmt.stmt.expr is null )
                    continue processStmt;

                // Attempt to fold
                {
                    Sit.Expr foldedExpr;
                    Sit.Value foldedValue;

                    try
                    {
                        foldedExpr = sem.foldExpr(stmt.stmt.expr);
                    }
                    catch( Object ex )
                    {
                        stmt.ex = ex;
                        failedStmt = true;
                    }

                    if( foldedExpr is null )
                        continue processStmt;

                    successStmt = true;
                    foldedValue = cast(Sit.Value) foldedExpr;

                    // If we couldn't fold, create a RuntimeValue.
                    if( foldedValue is null )
                        foldedValue = new Sit.RuntimeValue(
                                stmt.stmt.expr.astNode, foldedExpr);

                    // Handle bindings, etc.
                    if( stmt.stmt.bind )
                    {
                        stmt.mod.scop.bind(stmt.stmt.bindIdent, foldedValue);
                    }

                    if( stmt.stmt.xport )
                    {
                        assert( false, "nyi" );
                    }

                    if( stmt.stmt.mergeAll )
                    {
                        if( cast(Sit.RuntimeValue) foldedValue !is null )
                            assert( false, "cannot import from runtime value" );

                        assert( false, "nyi" );
                    }
                    else if( stmt.stmt.mergeList.length != 0 )
                    {
                        if( cast(Sit.RuntimeValue) foldedValue !is null )
                            assert( false, "cannot import from runtime value" );

                        assert( false, "nyi" );
                    }

                    stmt.stmt.value = foldedValue;
                }
            }

            if( ! successStmt )
            {
                foreach( stmt ; stmts )
                    if( stmt.ex !is null )
                        throw stmt.ex;

                assert( false, "could not complete semantic analysis" );
            }
        }
        while( failedStmt )

        // Dump statements into their respective modules
        foreach( stmt ; stmts )
            stmt.mod.stmts ~= stmt.stmt;

        // Clear list of statements to process
        stmts = null;
    }

    void injectStmts(Sit.Module mod, Ast.Statement[] stmts)
    {
        typeof(this.stmts) newStmts, injSlice;
        if( currentStmt == size_t.max )
        {
            newStmts = new Stmt[stmts.length];
            injSlice = newStmts;
        }
        else
        {
            newStmts = new Stmt[this.stmts.length + stmts.length];
            newStmts[0..currentStmt+1] = this.stmts[0..currentStmt+1];
            newStmts[currentStmt+1+stmts.length..$] = this.stmts[currentStmt+1..$];

            injSlice = newStmts[currentStmt+1..currentStmt+1+stmts.length];
        }

        foreach( i, ref stmt ; injSlice )
        {
            stmt.mod = mod;
            stmt.stmt.astNode = stmts[i];
        }

        this.stmts = newStmts;
    }

    Sit.Value loadModule(Sit.Value[] vs)
    {
        if( vs.length != 1 )
            assert( false, "expected one argument for module" );

        auto sv = cast(Sit.StringValue) vs[0];
        if( sv is null )
            assert( false, "expected string for module" );

        assert( currentModule !is null );

        auto path = sv.value;
        return new Sit.ModuleValue(null,
                loadRelativeToModule(currentModule, path));
    }
}

