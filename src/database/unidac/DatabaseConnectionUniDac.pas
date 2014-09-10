unit DatabaseConnectionUniDac;

interface

uses
  DatabaseConnection, Statement, DatabaseConfig, Query, DataSet, Classes, Uni,
  InterBaseUniProvider, OracleUniProvider, UniProvider, PostgreSQLUniProvider, SQLiteUniProvider,
  SQLServerUniProvider, MySQLUniProvider;

type
  TDatabaseConnectionUniDac = class(TAbstractDatabaseConnection, IDatabaseConnection)
  private
    FConnection: TUniConnection;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
    procedure StartTransaction;override;
    function inTransaction: Boolean;override;

    procedure open;override;
    procedure close;override;

    procedure configure(const config: IDatabaseConfig);override;

    function connected: Boolean;override;

    function createQuery: IQuery;override;

    function realConnection: TObject;override;

    function Execute(const AStatement: IStatement): Integer;override;
    function ExecuteWithParams(const AStatement: IStatement): Integer;

    function getFields(ATableName: String): IFieldListMetadata;override;
    procedure getTableNames(AList: TStrings);override;
  end;

implementation

uses
  SysUtils, QueryUniDac, DatabaseConnectionRegistry, DatabaseConnectionType;

{ TDatabaseConnectionUniDac }

procedure TDatabaseConnectionUniDac.close;
begin
  FConnection.Close;
end;

procedure TDatabaseConnectionUniDac.CommitTransaction;
begin
  FConnection.Commit;
end;

procedure TDatabaseConnectionUniDac.configure(const config: IDatabaseConfig);
begin
  FConnection.Server := config.HostName;
  FConnection.Database := config.Database;
  FConnection.Username := config.UserName;
  FConnection.Password := config.Password;
  FConnection.ProviderName := config.Params.Values['DBProvider'];
end;

function TDatabaseConnectionUniDac.connected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TDatabaseConnectionUniDac.AfterConstruction;
begin
  FConnection := TUniConnection.Create(nil);
end;

function TDatabaseConnectionUniDac.createQuery: IQuery;
begin
  Result := TQueryUniDac.Create(FConnection);
end;

destructor TDatabaseConnectionUniDac.Destroy;
begin
  FConnection.Free;
  inherited;
end;

function TDatabaseConnectionUniDac.Execute(const AStatement: IStatement): Integer;
begin
  if (AStatement.Params.Count = 0) then
  begin
    Result := FConnection.ExecSQL(AStatement.Command);
  end
  else
  begin
    Result := ExecuteWithParams(AStatement);
  end;
end;

function TDatabaseConnectionUniDac.ExecuteWithParams(const AStatement: IStatement): Integer;
var
  qry: TUniQuery;
begin
  qry := TUniQuery.Create(nil);
  try
    qry.Connection := FConnection;
    qry.SQL.Text := AStatement.Command;
    qry.Params.Assign(AStatement.Params);
    qry.ExecSQL;

    Result := qry.RowsAffected;
  finally
    qry.Free;
  end;
end;

function TDatabaseConnectionUniDac.getFields(ATableName: String): IFieldListMetadata;
var
  qry: TUniQuery;
begin
  qry := TUniQuery.Create(nil);
  try
    qry.Connection := FConnection;
    qry.SQL.Text := Format('select * from %s where 1 = 0 ', [ATableName]);
    qry.Open;

    Result := TFieldListMetadata.Create;
    Result.PopulateFromFieldList(qry.Fields);
  finally
    qry.Free;
  end;
end;

procedure TDatabaseConnectionUniDac.getTableNames(AList: TStrings);
begin
  FConnection.GetTableNames(AList);
end;

function TDatabaseConnectionUniDac.inTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

procedure TDatabaseConnectionUniDac.open;
begin
  FConnection.Open;
end;

function TDatabaseConnectionUniDac.realConnection: TObject;
begin
  Result := FConnection;
end;

procedure TDatabaseConnectionUniDac.RollbackTransaction;
begin
  FConnection.Rollback;
end;

procedure TDatabaseConnectionUniDac.StartTransaction;
begin
  FConnection.StartTransaction;
end;

initialization
  TDatabaseConnectionRegistry.RegisterConnection(dctUniDac, TDatabaseConnectionUniDac);

end.
