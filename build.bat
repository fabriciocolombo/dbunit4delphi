@echo off
set DIR_DELPHI=c:\Arquiv~1\Borland\Delphi7\Bin
set WARNING_MESSAGES=-UNIT_PLATFORM
set SEARCH_PATH=C:\Desenvolvimento\third\FastMM;C:\Desenvolvimento\third\DUnit\src;..\dcu
set RES_PATH=C:\Desenvolvimento\third\FastMM
set CONDITIONAL_DEFINES=ASCONSOLE

rem Vai para o drive D:
d:

cd D:\Desenvolvimento\OpenSource\dbunit4delphi\src
c:\Arquiv~1\Borland\Delphi7\Bin\dcc32.exe -B ..\packages\dbunit4delphiR7.dpk -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%

cd ..\Generator
c:\Arquiv~1\Borland\Delphi7\Bin\dcc32.exe -B GenaratorDataset.dpr -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%

cd ..\unittest
c:\Arquiv~1\Borland\Delphi7\Bin\dcc32.exe -B dbunit4delphitest.dpr -Q -W%WARNING_MESSAGES% -D%CONDITIONAL_DEFINES% -U%SEARCH_PATH%


