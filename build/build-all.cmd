@ECHO OFF
CALL dmdenv 1.057-tango-0.99.9
xfbuild -g -debug -unittest -version=Unittest +xtango +D.xf/LexFile.deps +O.xf/LexFile.objs +o..\bin\LexFile -I../src -J../src/import ../src/meme/lexer/LexFile.d
xfbuild -g -debug -unittest -version=Unittest +xtango +D.xf/ParseFile.deps +O.xf/ParseFile.objs +o..\bin\ParseFile -I../src -J../src/import ../src/meme/parser/ParseFile.d
