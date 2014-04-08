unit SampleDBTestCase;

interface

uses TestCaseExtension,  DBTestCase, DatabaseConnection, DataSet, DataSetDecorator,
  DataSetListBuilder, XmlClientDataSet, XmlDatasetReader, XmlValidator,
  DatabaseConfig, MockDatabaseConnection, DatabaseOperation, Windows;

type
  TSampleDBTestCase = class(TDBTestCase)
  private
    FConnection: Boolean;
    FDataSet: Boolean;
    FSetUpOperation: Boolean;
    FTearDownOperation: Boolean;
  protected
    function getDataSet: IDataSetReadOnly;override;

    function getConnection: IDatabaseConnection; override;

    function tearDownOperation: IDatabaseOperation; override;
    function setUpOperation: IDatabaseOperation; override;

    function getDatabaseConfig: IDatabaseConfig;override;
  public
  published
    procedure CalledGetDataSet;
    procedure CalledGetConnection;
    procedure CalledSetUpOperation;
  end;

implementation

uses DatabaseConfigDBX, StubDatabaseConfig,
  StubDatabaseConfigDBX, DatabaseConnectionType, SysUtils, StdConvs, TestUtils;

{ TSampleDBTestCase }

procedure TSampleDBTestCase.CalledGetConnection;
begin
  CheckTrue(FConnection);
end;

procedure TSampleDBTestCase.CalledGetDataSet;
begin
  CheckTrue(FDataSet);
end;

procedure TSampleDBTestCase.CalledSetUpOperation;
begin
  CheckTrue(FSetUpOperation);
end;

function TSampleDBTestCase.getConnection: IDatabaseConnection;
begin
  inherited getConnection;

  Result := nil;
  FConnection := True;
end;

function TSampleDBTestCase.getDatabaseConfig: IDatabaseConfig;
begin
  Result := TTestUtils.DATABASECONFIGDBX;
end;

function TSampleDBTestCase.getDataSet: IDataSetReadOnly;
begin
  Result := nil;
  FDataSet := True;
end;

function TSampleDBTestCase.setUpOperation: IDatabaseOperation;
begin
  inherited setUpOperation;
  Result := TDatabaseOperation.NONE;
  FSetUpOperation := True;
end;

function TSampleDBTestCase.tearDownOperation: IDatabaseOperation;
begin
  inherited tearDownOperation;
  FTearDownOperation := True;
  Result := TDatabaseOperation.NONE;
end;

initialization
  TSampleDBTestCase.RegisterTest('SampleDBTestCase');

end.
