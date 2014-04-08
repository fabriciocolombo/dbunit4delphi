unit DatabaseConnectionDBX;

interface

uses DatabaseConnection, SqlExpr, DatabaseConfig, Query, DatabaseConfigDBX,
     QueryDBX, Exceptions, Statement, DB, DataSet, Classes, DBXCommon;

type
  TDatabaseConnectionDBX = class(TInterfacedObject, IDatabaseConnection)
  private
    FSqlConnection: TSQLConnection;
    FTD: TDBXTransaction;

    function TableExists(ATableName: String): Boolean;
  public
    procedure CommitTransaction;
    procedure RollbackTransaction;
    procedure StartTransaction;
    function inTransaction: Boolean;

    procedure open;
    procedure close;

    procedure configure(const config: IDatabaseConfig);

    function connected: Boolean;

    function createQuery: IQuery;

    procedure AfterConstruction; override;
    destructor Destroy; override;

    function realConnection: TObject;

    function Execute(const AStatement: IStatement): Integer;
    function ExecuteWithParams(const AStatement: IStatement): Integer;

    //TODO - Carregar informação de que o campo é PK
    function getFields(ATableName: String): TFieldListMetadata;
    procedure getTableNames(AList: TStrings);
  end;


implementation

uses SysUtils;

{ TDatabaseConnectionDBX }

procedure TDatabaseConnectionDBX.AfterConstruction;
begin
  inherited;
  FSqlConnection := TSQLConnection.Create(nil);
  FSqlConnection.LoginPrompt := False;
end;

procedure TDatabaseConnectionDBX.close;
begin
  FSqlConnection.Close;
end;

procedure TDatabaseConnectionDBX.CommitTransaction;
begin
  FSqlConnection.CommitFreeAndNil(FTD);
end;

procedure TDatabaseConnectionDBX.configure(const config: IDatabaseConfig);
var
  vConfigDBX: IDatabaseConfigDBX;
begin
  if not Supports(config, IDatabaseConfigDBX, vConfigDBX) then
    raise EInvalidDatabaseConfigClass.Create('TDatabaseConfigDBX', config.getName);

  with vConfigDBX do
  begin
    FSqlConnection.Close;
    FSqlConnection.DriverName := DriverName;
    FSqlConnection.GetDriverFunc := GetDriverFunc;
    FSqlConnection.LibraryName := LibraryName;
    FSqlConnection.VendorLib := VendorLib;
    FSqlConnection.Params.Text := Params.Text;
    FSqlConnection.Params.Values['Database'] := Database;
    FSqlConnection.Params.Values['User_Name'] := UserName;
    FSqlConnection.Params.Values['Password'] := Password;
  end;
end;

function TDatabaseConnectionDBX.connected: Boolean;
begin
  Result := FSqlConnection.Connected;
end;

function TDatabaseConnectionDBX.createQuery: IQuery;
begin
  Result := TQueryDBX.Create(FSqlConnection);
end;

destructor TDatabaseConnectionDBX.Destroy;
begin
  FSqlConnection.Free;
  inherited;
end;

function TDatabaseConnectionDBX.Execute(const AStatement: IStatement): Integer;
begin
  if (AStatement.Params.Count = 0) then
  begin
    Result := FSqlConnection.ExecuteDirect(AStatement.Command);
  end
  else
  begin
    Result := ExecuteWithParams(AStatement);
  end;
end;

function TDatabaseConnectionDBX.ExecuteWithParams(const AStatement: IStatement): Integer;
begin
  Result := FSqlConnection.Execute(AStatement.Command, AStatement.Params);
end;

function TDatabaseConnectionDBX.getFields(ATableName: String): TFieldListMetadata;
var
  qry: TSQLQuery;
begin
  if not TableExists(ATableName) then
    raise ETableNotExists.Create(ATableName);

  qry := TSQLQuery.Create(nil);
  try
    qry.SQLConnection := FSqlConnection;

    qry.SQL.Text := Format('select * from %s where 1 = 0 ', [ATableName]);

    qry.Open;

    Result := TFieldListMetadata.Create;
    Result.PopulateFromFieldList(qry.Fields);
  finally
    qry.Free;
  end;
end;

procedure TDatabaseConnectionDBX.getTableNames(AList: TStrings);
begin
  FSqlConnection.GetTableNames(AList);
end;

function TDatabaseConnectionDBX.inTransaction: Boolean;
begin
  Result := FSqlConnection.InTransaction;
end;

procedure TDatabaseConnectionDBX.open;
begin
  FSqlConnection.Open;
end;

function TDatabaseConnectionDBX.realConnection: TObject;
begin
  Result := FSqlConnection;
end;

procedure TDatabaseConnectionDBX.RollbackTransaction;
begin
  FSqlConnection.RollbackFreeAndNil(FTD);
end;

procedure TDatabaseConnectionDBX.StartTransaction;
begin
  FTD := FSqlConnection.BeginTransaction;
end;

function TDatabaseConnectionDBX.TableExists(ATableName: String): Boolean;
var
  vTables: TStrings;
begin
  vTables := TStringList.Create;
  try
    FSqlConnection.GetTableNames(vTables);

    Result := (vTables.IndexOf(ATableName) >= 0);
  finally
    vTables.Free;
  end;
end;

end.
