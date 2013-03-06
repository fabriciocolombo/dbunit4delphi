@echo off
set WARNING_MESSAGES=-UNIT_PLATFORM
set SEARCH_PATH=..\lib\FastMM;..\lib\DUnit\src;..\lib\dunit-extension\src;..\dcu
set RES_PATH=C:\Desenvolvimento\third\FastMM
set CONDITIONAL_DEFINES=ASCONSOLE

cd src
%DELPHI_HOME%\bin\dcc32.exe -B ..\packages\dbunit4delphiR7.dpk -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH% -LE%DELPHI_HOME%\Projects\Bpl

cd ..\Generator
%DELPHI_HOME%\bin\dcc32.exe -B GenaratorDataset.dpr -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%

cd ..\unittest
%DELPHI_HOME%\bin\dcc32.exe -B dbunit4delphitest.dpr -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%

cd ..

