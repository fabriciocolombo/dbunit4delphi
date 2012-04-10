@echo off
d:
cd d:\Desenvolvimento\OpenSource\dbunit4delphi\unittest
del reports\coverage\*.* /S /Q 
CodeCoverage.exe -e ..\bin\dbunit4delphitest.exe -m ..\bin\dbunit4delphitest.map -od reports\coverage -uf dbunit4delphitest.dpr -a ^/testcase ^TSampleDBTestCase
call reports\coverage\CodeCoverage_summary.html