unit DatabaseConnectionDBX;

interface

uses DatabaseConnection, SqlExpr, DatabaseConfig, Query,
     QueryDBX, Exceptions, Statement, DB, DataSet, Classes, DBXCommon;

type
  TDatabaseConnectionDBX = class(TAbstractDatabaseConnection, IDatabaseConnection)
  private
    FSqlConnection: TSQLConnection;
    FTD: TDBXTransaction;

    function TableExists(ATableName: String): Boolean;
  public
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
    procedure StartTransaction;override;
    function inTransaction: Boolean;override;

    procedure open;override;
    procedure close;override;

    procedure configure(const config: IDatabaseConfig);override;

    function connected: Boolean;override;

    function createQuery: IQuery;override;

    procedure AfterConstruction; override;
    destructor Destroy; override;

    function realConnection: TObject;override;

    function Execute(const AStatement: IStatement): Integer;override;
    function ExecuteWithParams(const AStatement: IStatement): Integer;

    //TODO - Carregar informação de que o campo é PK
    function getFields(ATableName: String): IFieldListMetadata;override;
    procedure getTableNames(AList: TStrings);override;
  end;


implementation

uses SysUtils, DatabaseConnectionRegistry, DatabaseConnectionType;

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
begin
  FSqlConnection.Close;
  FSqlConnection.DriverName := config.Params.Values['DriverName'];
  FSqlConnection.GetDriverFunc := config.Params.Values['GetDriverFunc'];
  FSqlConnection.LibraryName := config.Params.Values['LibraryName'];
  FSqlConnection.VendorLib := config.Params.Values['VendorLib'];
  FSqlConnection.Params.Text := config.Params.Text;
  FSqlConnection.Params.Values['Database'] := config.Database;
  FSqlConnection.Params.Values['User_Name'] := config.UserName;
  FSqlConnection.Params.Values['Password'] := config.Password;
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

function TDatabaseConnectionDBX.getFields(ATableName: String): IFieldListMetadata;
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

initialization
  TDatabaseConnectionRegistry.RegisterConnection(dctDBX, TDatabaseConnectionDBX);

end.
