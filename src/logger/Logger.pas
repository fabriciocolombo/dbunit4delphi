unit Logger;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs;

type
  ILogger = interface(IInterface)
    procedure debug(message: string);
    procedure error(message: string);
    procedure info(message: string);
    function IsDebugEnabled: Boolean;
    procedure warning(message: string);
  end;


implementation


end.
