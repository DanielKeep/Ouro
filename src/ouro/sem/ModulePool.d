/**
    Collects modules and uncompiled statements.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.ModulePool;

debug(TraceModulePool) import tango.io.Stdout;
import tango.net.Uri;

import Path = tango.io.Path;

import ouro.sem.Abort;
import ouro.sem.Context : Context;
import ouro.sem.Semantic : SemInitialVisitor;
import ouro.util.ImportRoot : ImportRoot, Resource, ImportRootSchemes;
import ouro.util.TokenStream;
import ouro.util.uriSchemes.File : FileRoot;
import ouro.Location;
import ouro.Source;

import Ast = ouro.ast.Nodes;
import Sem = ouro.sem.Semantic;
import Sit = ouro.sit.Nodes;
import Builtins = ouro.sem.builtins.Builtins;
import Lexer = ouro.lexer.Lexer;
import Parser = ouro.parser.Parser;

debug(TraceStmtSem)     debug=TraceStmt;
debug(TraceStmtFold)    debug=TraceStmt;

debug(TraceStmt)
{
    import SitRepr = ouro.sit.ReprVisitor;
    import ouro.util.StructuredOutput;
}

const char[][] FileExtensions = [".ouro"];

struct ModulePool
{
    struct Entry
    {
        Uri uri;
        Source src;
        size_t* stmtsNotDone;
        Ast.Module ast;
        Sit.Module sit;
    }

    struct Stmt
    {
        bool done = false;
        Sit.Module mod;
        Sit.Stmt stmt;
        size_t* stmtsNotDone;
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

    Sit.Module load(Uri uri, bool includeLang = true)
    {
        // ouromod: URLs are easy.
        if( uri.scheme == "ouromod" )
            return load(uri.path, includeLang);

        // We'll try each import root, looking for one which both 'owns' this
        // uri and is willing to load it.
        foreach( root ; importRoots )
        {
            auto path = root.pathFrom(uri);
            if( path == "" )
                continue;
            auto res = root.resolve(path);
            if( res is null )
                return null;
            return load(path, res, includeLang);
        }

        return null;
    }

    Sit.Module load(char[] path, bool includeLang = true)
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

        return load(path, res, includeLang);
    }

    Sit.Module load(char[] path, Resource res, bool includeLang = true)
    {
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

        // Create counter for un-processed statements
        entry.stmtsNotDone = new size_t;

        // Done making entry.
        entries ~= entry;
        pathToEntryIdx[entry.sit.path] = entries.length-1;

        // Add to the statement pool
        if( includeLang )
            // Inject: import "/ouro/lang" : *
            injectStmts(entry.sit, [new Ast.ImportStmt(Location.init,
                        /*xport*/false, "/ouro/lang", /*ident*/null,
                        /*all*/true, /*symbols*/null)]);

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
                Sit.EvalContext.Compile);

        Context ctx;
        ctx.builtinFn = (char[] name)
        {
            if( name == "ouro.module" )
                return cast(Sit.Value) moduleFn;
            else
                return Builtins.lookup(name);
        };

        ctx.bindFn = (char[] ident, Sit.Value value)
        {
            assert( currentModule !is null );
            currentModule.scop.bind(ident, value);
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
                ctx.curModule = currentModule = stmt.mod;
                scope(exit)
                {
                    currentStmt = size_t.max;
                    ctx.curModule = currentModule = null;
                }

                void resetStmtPtr() { stmt = &stmts[i]; }

                if( stmt.done )
                    // Already done this one!  YAY!
                    continue processStmt;

                debug(TraceModulePool)
                {
                    Stderr.formatln("Compiling {} #{} - {}",
                            stmt.mod.path, currentStmt,
                            stmt.stmt.astNode.loc.toString);
                }

                // Try to process
                if( stmt.stmt.expr is null )
                {
                    auto subCtx = ctx.dup;
                    auto astStmt = stmt.stmt.astNode;

                    subCtx.stmt = &stmt.stmt;
                    subCtx.scop = stmt.mod.scop;

                    // Note: stmts may be reallocated due to additional
                    // modules being loaded.  We need to update the stmt
                    // pointer after the visit just in case.

                    try
                    {
                        stmt.stmt.expr = sem.visitExpr(astStmt, &subCtx);
                    }
                    catch( NonFatalAbort nf )
                    {
                        resetStmtPtr;
                        stmt.ex = nf;
                        failedStmt = true;
                    }
                    resetStmtPtr;

                    debug(TraceStmtSem) if( stmt.stmt.expr !is null )
                    {
                        scope repr = new SitRepr.ReprVisitor(
                                StructuredOutput.forStdout);

                        Stdout.format("{} SEM: ", stmt.stmt.astNode.loc);
                        repr.visitBase(stmt.stmt.expr, true);
                        Stdout.newline;
                    }
                }

                // Semantic failed
                if( stmt.stmt.expr is null )
                    continue processStmt;

                // Attempt to fold
                {
                    Sit.Expr foldedExpr;
                    Sit.Value foldedValue = stmt.stmt.value;

                    if( foldedValue is null )
                    {
                        try
                        {
                            foldedExpr = sem.foldExpr(stmt.stmt.expr);
                        }
                        catch( Object ex )
                        {
                            resetStmtPtr;
                            stmt.ex = ex;
                            failedStmt = true;
                        }
                        resetStmtPtr;

                        if( foldedExpr is null )
                            continue processStmt;
                        foldedValue = cast(Sit.Value) foldedExpr;

                        // If we couldn't fold, create a RuntimeValue.
                        if( foldedValue is null )
                            foldedValue = new Sit.RuntimeValue(
                                    stmt.stmt.expr.astNode, foldedExpr);

                        stmt.stmt.value = foldedValue;
                    }

                    debug(TraceStmtFold) if( stmt.stmt.value !is null )
                    {
                        scope repr = new SitRepr.ReprVisitor(
                                StructuredOutput.forStdout);

                        Stdout.format("{} FLD: ", stmt.stmt.astNode.loc);
                        repr.visitBase(stmt.stmt.value, true);
                        Stdout.newline;
                    }
                }

                // Handle bindings, etc.
                {
                    if( stmt.stmt.bind )
                    {
                        stmt.mod.scop.bind(stmt.stmt.bindIdent, stmt.stmt.value);

                        if( stmt.stmt.xport )
                            stmt.mod.exportScop.bind(stmt.stmt.bindIdent,
                                    stmt.stmt.value);
                    }

                    if( stmt.stmt.mergeAll )
                    {
                        if( cast(Sit.RuntimeValue) stmt.stmt.value !is null )
                            assert( false, "cannot import from runtime value" );

                        if( auto mv = cast(Sit.ModuleValue) stmt.stmt.value )
                        {
                            if( ! mv.modul.complete )
                            {
                                stmt.ex = new Exception("cannot import all "
                                    "symbols from module "~mv.modul.path~
                                    " before it has been compiled.  Try a "
                                    "selective import instead.");
                                failedStmt = true;
                                continue processStmt;
                            }

                            foreach( k,v ; mv.modul.exportScop.entries )
                                stmt.mod.scop.bind(k, v);

                            if( stmt.stmt.xport )
                                foreach( k,v ; mv.modul.exportScop.entries )
                                    stmt.mod.exportScop.bind(k, v);
                        }
                        else
                            assert( false, "cannot import from "
                                    ~stmt.stmt.value.toString );
                    }
                    else if( stmt.stmt.mergeList.length != 0 )
                    {
                        if( cast(Sit.RuntimeValue) stmt.stmt.value !is null )
                            assert( false, "cannot import from runtime value" );

                        if( auto mv = cast(Sit.ModuleValue) stmt.stmt.value )
                        {
                            foreach( k ; stmt.stmt.mergeList )
                                stmt.mod.scop.bind(k,
                                    mv.modul.exportScop.lookup(null, k));

                            if( stmt.stmt.xport )
                                foreach( k ; stmt.stmt.mergeList )
                                    stmt.mod.scop.bind(k,
                                        mv.modul.exportScop.lookup(null, k));
                        }
                        else
                            assert( false, "cannot import from "
                                    ~stmt.stmt.value.toString );
                    }

                    if( stmt.stmt.expr is null )
                        assert( false );
                    if( stmt.stmt.value is null )
                        assert( false );

                    successStmt = true;
                    stmt.done = true;
                    (*stmt.stmtsNotDone)--;

                    if( *stmt.stmtsNotDone == 0 )
                        modDone(stmt.mod);
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

        // Double-check that all statements are done *and* the associated
        // modules are done.
        foreach( i,stmt ; stmts )
        {
            assert( stmt.done );
            assert( stmt.mod.complete );
        }

        // Clear list of statements to process
        stmts = null;
    }

    void modDone(Sit.Module mod)
    {
        debug
        {
            Entry* entry;
            // TODO: Better way of doing this
            foreach( ref e ; entries )
                if( e.sit is mod )
                    entry = &e;
            assert( entry !is null );
            assert( *entry.stmtsNotDone == 0 );
        }

        assert( ! mod.complete );

        foreach( stmt ; stmts )
        {
            if( stmt.mod !is mod )
                continue;

            assert( stmt.done );

            stmt.mod.stmts ~= stmt.stmt;
        }

        mod.complete = true;
    }

    void injectStmts(Sit.Module mod, Ast.Statement[] stmts)
    {
        typeof(this.stmts) newStmts, injSlice;
        if( currentStmt == size_t.max )
        {
            newStmts = new Stmt[this.stmts.length+stmts.length];
            newStmts[0..this.stmts.length] = this.stmts[];
            injSlice = newStmts[this.stmts.length..$];
        }
        else
        {
            newStmts = new Stmt[this.stmts.length + stmts.length];
            newStmts[0..currentStmt+1] = this.stmts[0..currentStmt+1];
            newStmts[currentStmt+1+stmts.length..$] = this.stmts[currentStmt+1..$];

            injSlice = newStmts[currentStmt+1..currentStmt+1+stmts.length];
        }

        // TODO: Better way of doing this
        size_t* stmtsNotDone;
        foreach( entry ; entries )
            if( entry.sit is mod )
                stmtsNotDone = entry.stmtsNotDone;

        assert( stmtsNotDone !is null );

        foreach( i, ref stmt ; injSlice )
        {
            stmt.mod = mod;
            stmt.stmt.astNode = stmts[i];
            stmt.stmtsNotDone = stmtsNotDone;
            (*stmtsNotDone)++;
        }

        this.stmts = newStmts;
    }

    Sit.Value loadModule(Sit.EvalContext ectx, Sit.Value[] vs)
    {
        if( vs.length != 1 )
            assert( false, "expected one argument for module" );

        auto sv = cast(Sit.StringValue) vs[0];
        if( sv is null )
            assert( false, "expected string for module" );

        assert( currentModule !is null );

        auto path = sv.value;
        auto mod = loadRelativeToModule(currentModule, path);
        if( mod is null )
            return Sit.NilValue.instance;
        else
            return new Sit.ModuleValue(null, mod);
    }
}

