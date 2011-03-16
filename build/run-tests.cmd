@ECHO OFF

if exist tests.log del tests.log
if exist tests.out del tests.out
if exist tests.err del tests.err

echo.

for %%F in (..\doc\tests\*.pass.ouro) do (call :passtest "%%F")
for %%F in (..\doc\tests\*.fail.ouro) do (call :failtest "%%F")

goto :eof

:passtest
    echo.>>tests.out
    echo.>>tests.err
    echo.TEST %1>>tests.out
    echo.TEST %1>>tests.err
    ..\bin\SemFile --import ../doc/tests %1 1>>tests.out 2>>tests.err
    if errorlevel 1 goto :xfail
    goto :pass

:failtest
    echo.>>tests.out
    echo.>>tests.err
    echo.TEST %1>>tests.out
    echo.TEST %1>>tests.err
    ..\bin\SemFile --import ../doc/tests %1 1>>tests.out 2>>tests.err
    if errorlevel 1 goto :fail
    goto :xpass

:pass
    echo  PASS - %1>>tests.log
    echo  PASS - %1
    goto :eof

:fail
    echo  FAIL - %1>>tests.log
    echo  FAIL - %1
    goto :eof

:xpass
    echo XPASS - %1>>tests.log
    echo XPASS - %1
    goto :eof

:xfail
    echo XFAIL - %1>>tests.log
    echo XFAIL - %1
    goto :eof

