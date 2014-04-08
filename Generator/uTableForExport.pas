unit uTableForExport;

interface

uses Classes;

type
  TTableForExport = class
  private
    FTableName: String;
    FWhereClause: String;
    FFields: TStringList;
  public
    property TableName: String read FTableName;
    property WhereClause: String read FWhereClause write FWhereClause;
    property Fields: TStringList read FFields;

    function GetQuery: String;

    constructor Create(ATableName: String);
    destructor Destroy; override;   
  end;

  
implementation

uses SysUtils;

{ TTableForExport }

constructor TTableForExport.Create(ATableName: String);
begin
  FTableName := ATableName;
  FFields := TStringList.Create;
end;

destructor TTableForExport.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TTableForExport.GetQuery: String;
var
  vFields: String;
begin
  if FFields.Count = 0 then
  begin
    vFields := '*'
  end
  else
  begin
    vFields := StringReplace(Trim(FFields.Text), sLineBreak, ',', [rfReplaceAll]);
  end;
  
  Result := Format('Select %s from %s %s', [vFields, FTableName, FWhereClause]);
end;

end.
