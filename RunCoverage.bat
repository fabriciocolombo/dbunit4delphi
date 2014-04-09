rmdir target\CoverageReports /S /Q
mkdir target\CoverageReports

dir src\*.pas /S /B > Arquivos.txt

cd bin

cmd /C CodeCoverage -e dbunit4delphitest.exe -uf ..\Arquivos.txt -od ..\target\CoverageReports -a /text -lt ..\coverage.log -sp ..\src ..\src\database ..\src\database\dbx ..\src\database\operation  ..\src\statement ..\src\xml -xml -html

cd ..