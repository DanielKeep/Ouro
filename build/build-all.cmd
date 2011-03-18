@ECHO OFF
CALL dmdenv 1.057-tango-0.99.9
set ARGS=-g -debug -unittest "-version=Unittest" +xtango -I../src -J../src/import

echo Compiling LexFile
xfbuild %ARGS% +D.xf/LexFile.deps +O.xf/LexFile.objs +o..\bin\LexFile %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/lexer/LexFile.d
if %errorlevel% neq 0 goto fail

echo Compiling ParseFile
xfbuild %ARGS% +D.xf/ParseFile.deps +O.xf/ParseFile.objs +o..\bin\ParseFile %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/parser/ParseFile.d
if %errorlevel% neq 0 goto fail

echo Compiling SemFile
xfbuild %ARGS% +D.xf/SemFile.deps +O.xf/SemFile.objs +o..\bin\SemFile %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/sem/SemFile.d
if errorlevel 1 goto fail

echo Compiling Repl
xfbuild %ARGS% +D.xf/Repl.deps +O.xf/Repl.objs +o..\bin\Repl %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/sem/Repl.d
if errorlevel 1 goto fail

goto eof

:fail
exit /b 1

:eof
