unit Table;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs;

type
  ITable = interface
  ['{9C404D47-A816-41CA-866D-15887456EB2C}']
    function GetTableName: Integer;
    procedure SetTableName(Value: Integer);
    
    property TableName: Integer read GetTableName write SetTableName;
  end;


implementation


end.
