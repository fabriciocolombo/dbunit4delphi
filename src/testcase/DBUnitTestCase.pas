unit DBUnitTestCase;

interface

uses TestCaseExtension, DatabaseOperation, DatabaseConnection, DataSet, DatabaseConnectionType,
     DatabaseConnectionFactory, DatabaseConfig;

type
  TDBUnitTestCase = class(TTestCaseExtension)
  private
    FConnection: IDatabaseConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function getConnection: IDatabaseConnection;virtual;

    function setUpOperation: IDatabaseOperation;virtual;
    function tearDownOperation: IDatabaseOperation;virtual;

    function getDataSet: IDataSetReadOnly;virtual;abstract;
    function getDatabaseConfig: IDatabaseConfig;virtual;abstract;
  public
  end;


implementation

uses NoneOperation;


{ TDBUnitTestCase }

function TDBUnitTestCase.getConnection: IDatabaseConnection;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := ConnectionFactory.newConnection(getDatabaseConfig);
  end;

  Result := FConnection;
end;

procedure TDBUnitTestCase.SetUp;
begin
  inherited;
  FConnection := getConnection;

  if Assigned(FConnection) then
  begin
    FConnection.StartTransaction;
  end;
  setUpOperation.execute(FConnection, getDataSet);
end;

function TDBUnitTestCase.setUpOperation: IDatabaseOperation;
begin
  Result := TDatabaseOperation.CLEAN_INSERT;
end;

procedure TDBUnitTestCase.TearDown;
begin
  try
    tearDownOperation.execute(FConnection, getDataSet);
  finally
    if Assigned(FConnection) then
    begin
      FConnection.RollbackTransaction;
    end;
  end;
  inherited;
end;

function TDBUnitTestCase.tearDownOperation: IDatabaseOperation;
begin
  Result := TDatabaseOperation.NONE;
end;

end.
