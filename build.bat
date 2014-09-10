call "rsvars.bat"
msbuild.exe /target:Build /p:config=Release /p:Platform=Win32 %~dp0\packages\XE2\DBUnit4Delphi.dproj
msbuild.exe /target:Build /p:config=Debug   /p:Platform=Win32 %~dp0\packages\XE2\DBUnit4Delphi.dproj
msbuild.exe /target:Build /p:config=Release /p:Platform=Win32 %~dp0\Generator\GenaratorDataset.dproj
msbuild.exe /target:Build /p:config=Debug   /p:Platform=Win32 %~dp0\unittest\dbunit4delphitest.dproj