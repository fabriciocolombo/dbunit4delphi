@echo off
set WARNING_MESSAGES=-UNIT_PLATFORM
set SEARCH_PATH=..\lib\FastMM;..\lib\DUnit\src;..\lib\dunit-extension\src;..\dcu
set RES_PATH=C:\Desenvolvimento\third\FastMM
set CONDITIONAL_DEFINES=ASCONSOLE

if "%DELPHI_HOME%" == "" goto :delphihomenotfound 

goto :compile

:delphihomenotfound
echo The DELPHI_HOME environment variable is not defined correctly
echo This environment variable is needed to run this program
echo For example:
echo    Delphi 7   -  DELPHI_HOME=C:\Program Files\Borland\Delphi7
echo    Delphi XE2 -  DELPHI_HOME=C:\Program Files\Embarcadero\RAD Studio\9.0
goto :end	

:compile
cd src
"%DELPHI_HOME%\bin\dcc32.exe" -B ..\packages\dbunit4delphiR7.dpk -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH% -LE"%DELPHI_HOME%\Projects\Bpl"

cd ..\Generator
"%DELPHI_HOME%\bin\dcc32.exe" -B GenaratorDataset.dpr -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%

cd ..\unittest
"%DELPHI_HOME%\bin\dcc32.exe" -B dbunit4delphitest.dpr -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%

cd ..

:end