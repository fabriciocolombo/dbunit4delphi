@echo off
d:
cd D:\Desenvolvimento\OpenSource\dbunit4delphi\unittest
rmdir reports\coverage /S /Q
mkdir reports\coverage
CodeCoverage.exe -e ..\bin\dbunit4delphitest.exe -m ..\bin\dbunit4delphitest.map -od reports\coverage -uf dbunit4delphitest.dpr