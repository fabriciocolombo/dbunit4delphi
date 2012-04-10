unit LoggerFactory;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, Logger;

type
  ILoggerFactory = interface(IInterface)
    function getLogger(name: String): ILogger; overload;
    function getLogger(clazz: TClass): ILogger; overload; 
  end;

  function GetLoggerFactory: ILoggerFactory;

implementation

var
  _LoggerFactory: ILoggerFactory = nil;

function GetLoggerFactory: ILoggerFactory;
begin
  Result := _LoggerFactory;
end;

initialization

finalization

end.
