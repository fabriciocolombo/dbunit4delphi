(*
 •VER80 - Delphi 1
 •VER90 - Delphi 2
 •VER100 - Delphi 3
 •VER120 - Delphi 4
 •VER130 - Delphi 5
 •VER140 - Delphi 6
 •VER150 - Delphi 7
 •VER160 - Delphi 8
 •VER170 - Delphi 2005
 •VER180 - Delphi 2006
 •VER180 - Delphi 2007
 •VER185 - Delphi 2007
 •VER200 - Delphi 2009
 •VER210 - Delphi 2010
 •VER220 - Delphi XE
 •VER230 - Delphi XE2
 •WIN32 - Indicates that the operating environment is the Win32 API.
 •LINUX - Indicates that the operating environment is Linux
 •MSWINDOWS - Indicates that the operating environment is the MS Windows/li] 
•CONSOLE - Indicates that an application is being compiled as a console application
*)

{$IFDEF VER150}
   {$DEFINE D7}
{$ENDIF}
{$IFDEF VER200}
  {$DEFINE D2009UP}
{$ENDIF}
{$IFDEF VER210}
  {$DEFINE D2009UP}
  {$DEFINE D2010UP}
{$ENDIF}
{$IFDEF VER220}
  {$DEFINE D2009UP}
  {$DEFINE D2010UP}
{$ENDIF}