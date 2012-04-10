unit uTableForExport;

interface

type
  TTableForExport = class
  private
    FTableName: String;
    FWhereClause: String;
  public
    property TableName: String read FTableName;
    property WhereClause: String read FWhereClause write FWhereClause;

    function GetQuery: String;

    constructor Create(ATableName: String);
  end;

  
implementation

uses SysUtils;

{ TTableForExport }

constructor TTableForExport.Create(ATableName: String);
begin
  FTableName := ATableName;
end;

function TTableForExport.GetQuery: String;
begin
  Result := Format('Select * from %s %s', [FTableName, FWhereClause]);
end;

end.
