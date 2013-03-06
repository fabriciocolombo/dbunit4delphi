unit DBTestCase;

interface

uses TestCaseExtension, DatabaseOperation, DatabaseConnection, DataSet, DatabaseConnectionType,
     DatabaseConnectionFactory, DatabaseConfig;

type
  TDBTestCase = class(TTestCaseExtension)
  private
    FConnection: IDatabaseConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function getConnection: IDatabaseConnection;virtual;

    function setUpOperation: IDatabaseOperation;virtual;
    function tearDownOperation: IDatabaseOperation;virtual;

    function getDataSet: IDataSet;virtual;abstract;
    function getDatabaseConfig: IDatabaseConfig;virtual;abstract;
  public
  end;


implementation

uses NoneOperation;


{ TDBTestCase }

function TDBTestCase.getConnection: IDatabaseConnection;
begin
  if not Assigned(FConnection) then
    FConnection := ConnectionFactory.newConnection(getDatabaseConfig);

  Result := FConnection;
end;

procedure TDBTestCase.SetUp;
begin
  inherited;
  FConnection := getConnection;

  if Assigned(FConnection) then
  begin
    FConnection.StartTransaction;
  end;
  setUpOperation.execute(FConnection, getDataSet);
end;

function TDBTestCase.setUpOperation: IDatabaseOperation;
begin
  Result := TDatabaseOperation.CLEAN_INSERT;
end;

procedure TDBTestCase.TearDown;
begin
  tearDownOperation.execute(FConnection, getDataSet);
  if Assigned(FConnection) then
  begin
    FConnection.RollbackTransaction;
  end;
  inherited;
end;

function TDBTestCase.tearDownOperation: IDatabaseOperation;
begin
  Result := TDatabaseOperation.DELETE;
end;

end.
