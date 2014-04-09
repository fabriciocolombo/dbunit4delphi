call "rsvars.bat"
msbuild.exe /target:Build /p:config=Release /p:Platform=Win32 packages\XE2\DBUnit4Delphi.dproj
msbuild.exe /target:Build /p:config=Debug   /p:Platform=Win32 packages\XE2\DBUnit4Delphi.dproj
msbuild.exe /target:Build /p:config=Release /p:Platform=Win32 Generator\GenaratorDataset.dproj
msbuild.exe /target:Build /p:config=Debug   /p:Platform=Win32 unittest\dbunit4delphitest.dproj