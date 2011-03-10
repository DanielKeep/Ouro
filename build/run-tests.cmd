@ECHO OFF

if exist tests.log del tests.log
if exist tests.out del tests.out
if exist tests.err del tests.err

for %%F in (..\doc\tests\*.pass.ouro) do (call :passtest "%%F")

echo.
type tests.log
goto :eof

:passtest
    echo.>>tests.out
    echo.>>tests.err
    echo.TEST %1>>tests.out
    echo.TEST %1>>tests.err
    ..\bin\SemFile --import ../doc/tests %1 1>>tests.out 2>>tests.err
    if errorlevel 1 goto :fail
    goto :pass

:pass
    echo  PASS - %1>>tests.log
    goto :eof

:fail
    echo  FAIL - %1>>tests.log
    goto :eof

