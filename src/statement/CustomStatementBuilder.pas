unit CustomStatementBuilder;

interface

uses
  Classes, DB, SysUtils, Statement, StatementBuilderIntf;

type
  TCustomStatementBuilder = class(TInterfacedObject, IStatementBuilder)
  private
    FFields: TStringList;
    FFieldsUpdate: TStringList;
    FFieldsInsert: TStringList;
    FValuesInsert: TStringList;
    FWhereFields: TStringList;
    FTableName: String;

  protected

    function addField(fieldName: String): TCustomStatementBuilder;virtual;

    function addFieldInsert(fieldName, fieldValue: String): TCustomStatementBuilder;virtual;

    function addFieldUpdate(fieldName, fieldValue: String): TCustomStatementBuilder;virtual;

    function addWhere(fieldName, fieldValue: String): TCustomStatementBuilder;virtual;

    function getFields: String;
    function getWhere: String;

    function getInsertFields: String;
    function getInsertValues: String;

    function getUpdateFields: String;

    function buildCommand: String;virtual;
    function buildParams: TParams;virtual;
  public
    property TableName: String read FTableName;

    function build: IStatement;virtual;

    constructor Create(tableName: String);virtual;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;


implementation



{ TCustomStatementBuilder }

function TCustomStatementBuilder.addField(fieldName: String): TCustomStatementBuilder;
begin
  FFields.Add(fieldName);

  Result := Self;
end;

function TCustomStatementBuilder.addFieldInsert(fieldName,fieldValue: String): TCustomStatementBuilder;
begin
  FFieldsInsert.Add(fieldName);
  FValuesInsert.Add(fieldValue);

  Result := Self;
end;

function TCustomStatementBuilder.addFieldUpdate(fieldName,fieldValue: String): TCustomStatementBuilder;
begin
  FFieldsUpdate.Values[fieldName] := fieldValue;

  Result := Self;
end;

function TCustomStatementBuilder.addWhere(fieldName,fieldValue: String): TCustomStatementBuilder;
begin
  FWhereFields.Values[fieldName] := fieldValue;

  Result := Self;
end;

procedure TCustomStatementBuilder.AfterConstruction;
begin
  inherited;
  FFields := TStringList.Create;
  FFieldsInsert := TStringList.Create;
  FValuesInsert := TStringList.Create;
  FFieldsUpdate := TStringList.Create;
  FWhereFields := TStringList.Create;
end;

destructor TCustomStatementBuilder.Destroy;
begin
  FFields.Free;
  FFieldsInsert.Free;
  FValuesInsert.Free;
  FFieldsUpdate.Free;
  FWhereFields.Free;
  inherited;
end;

function TCustomStatementBuilder.build: IStatement;
var
  vParams: TParams;
begin
  Result := TStatement.Create(buildCommand) as IStatement;
  vParams := buildParams;
  try
    Result.Params := vParams;
  finally
    vParams.Free;
  end;
end;

function TCustomStatementBuilder.buildCommand: String;
begin
  Result := EmptyStr;
end;

function TCustomStatementBuilder.buildParams: TParams;
begin
  Result := TParams.Create(nil);
end;

constructor TCustomStatementBuilder.Create(tableName: String);
begin
  FTableName := tableName;
end;

function TCustomStatementBuilder.getFields: String;
const
  sSeparator = ', ';
begin
  if (FFields.Count = 0) then
  begin
    Result := '*'
  end
  else
  begin
    Result := StringReplace(FFields.Text, sLineBreak, sSeparator, [rfReplaceAll]);
    Delete(Result, Length(Result)-1, 2);
  end;
end;

function TCustomStatementBuilder.getInsertFields: String;
const
  sSeparator = ', ';
begin
  if (FFieldsInsert.Count = 0) then
  begin
    raise Exception.Create('nenhum campo adicionado');
  end
  else
  begin
    Result := StringReplace(FFieldsInsert.Text, sLineBreak, sSeparator, [rfReplaceAll]);
    Result := '(' + Copy(Result, 1, Length(Result)-2) + ')';
  end;
end;

function TCustomStatementBuilder.getInsertValues: String;
const
  sSeparator = ', ';
begin
  if (FValuesInsert.Count = 0) then
  begin
    raise Exception.Create('nenhum valor adicionado');
  end
  else
  begin
    Result := StringReplace(FValuesInsert.Text, sLineBreak, sSeparator, [rfReplaceAll]);
    Result := '(' + Copy(Result, 1, Length(Result)-2) + ')';
  end;
end;

function TCustomStatementBuilder.getUpdateFields: String;
const
  sSeparator = ', ';
begin
  Result := EmptyStr;
  if (FFieldsUpdate.Count > 0) then
  begin
    Result := StringReplace(FFieldsUpdate.Text, sLineBreak, sSeparator, [rfReplaceAll]);
    Delete(Result, Length(Result)-1, 2);
  end;

end;

function TCustomStatementBuilder.getWhere: String;
const
  sSeparator = ' and ';
begin
  Result := EmptyStr;
  if (FWhereFields.Count > 0) then
  begin
    Result := ' where ' + StringReplace(FWhereFields.Text, sLineBreak, sSeparator, [rfReplaceAll]);
    Delete(Result, Length(Result)-4, 5);
  end;
end;

end.
