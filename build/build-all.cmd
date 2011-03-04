@ECHO OFF
CALL dmdenv 1.057-tango-0.99.9
xfbuild -g -debug -unittest -version=Unittest +xtango +D.xf/LexFile.deps +O.xf/LexFile.objs +o..\bin\LexFile -I../src -J../src/import %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/lexer/LexFile.d
if %errorlevel% neq 0 goto fail
xfbuild -g -debug -unittest -version=Unittest +xtango +D.xf/ParseFile.deps +O.xf/ParseFile.objs +o..\bin\ParseFile -I../src -J../src/import %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/parser/ParseFile.d
if %errorlevel% neq 0 goto fail
xfbuild -g -debug -unittest -version=Unittest +xtango +D.xf/SemFile.deps +O.xf/SemFile.objs +o..\bin\SemFile -I../src -J../src/import %1 %2 %3 %4 %5 %6 %7 %8 %9 ../src/ouro/sem/SemFile.d
if %errorlevel% neq 0 goto fail
goto eof

:fail
exit /b 1

:eof
