@ECHO OFF

:passtest
    echo.TEST %1
    ..\bin\SemFile --import ../doc/tests %1
    if errorlevel 1 goto :fail
    goto :pass

:pass
    echo  PASS - %1
    goto :eof

:fail
    echo  FAIL - %1
    goto :eof

