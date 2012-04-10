unit TestInsertOperation;

interface

uses BaseTestCase, DatabaseConnection, DataSet, DataSetDecorator,
  DataSetListBuilder, XmlClientDataSet, XmlDatasetReader, XmlValidator,
  DatabaseConfig, MockDatabaseConnection, DatabaseOperation, Windows,
  TestResources;

type
  TTestInsertOperation = class(TBaseTestCase)
  private
  protected

  public
  published
    procedure insertSample;
    procedure insertPerson;
    procedure insertPersonReal;
  end;

implementation

uses SysUtils, DatabaseConnectionFactory, TestUtils;

{ TTestInsertOperation }

procedure TTestInsertOperation.insertPerson;
const
  firstInsert  = 'INSERT INTO PERSON (ID, NAME, BIRTHDATE, SALARY) VALUES (1, ''FABRICIO'', ''1983-04-15'', 2153.47)';
  secondInsert = 'INSERT INTO PERSON (ID, NAME, BIRTHDATE, SALARY) VALUES (2, ''PEDRO'', ''1986-07-23'', 1376.54)';
var
  vDataSet: IDataSetReadOnly;
  vConnection: IMockDatabaseConnection;
begin
  vDataSet := TXmlDataSetBuilder.newFromFile(PersonXMLPath).build;

  vConnection := TMockDatabaseConnection.Create;
  vConnection.addExpectedStatement(firstInsert);
  vConnection.addExpectedStatement(secondInsert);

  TDatabaseOperation.INSERT.execute(vConnection, vDataSet);

  vConnection.checkCalls;
end;

procedure TTestInsertOperation.insertPersonReal;
var
  vDataSet: IDataSetReadOnly;
  vConnection: IDatabaseConnection;
begin
  vDataSet := TXmlDataSetBuilder.newFromFile(PersonXMLPath).build;

  vConnection := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);

  vConnection.StartTransaction;
  try
    TDatabaseOperation.DELETE_ALL.execute(vConnection, vDataSet);

    TDatabaseOperation.INSERT.execute(vConnection, vDataSet);

    //TODO - Checar se os registros foram realmente inseridos

    TDatabaseOperation.DELETE_ALL.execute(vConnection, vDataSet);
  finally
    vConnection.RollbackTransaction;
  end;
end;

procedure TTestInsertOperation.insertSample;
const
  firstInsert  = 'insert into TEST_TABLE (col0, col1, col2) values (''row 0 col 0'', ''row 0 col 1'', ''row 0 col 2'')';
  secondInsert = 'insert into TEST_TABLE (col0, col1, col2) values (''row 1 col 0'', ''row 1 col 1'', null)';
  thirdInsert  = 'insert into SECOND_TABLE (col0, col1) values (''row 0 col 0'', ''row 0 col 1'')';
var
  vDataSet: IDataSetReadOnly;
  vConnection: IMockDatabaseConnection;
begin
  vDataSet := TXmlDataSetBuilder.newFromFile(SampleXMLPath).build;

  vConnection := TMockDatabaseConnection.Create;
  vConnection.addExpectedStatement(firstInsert);
  vConnection.addExpectedStatement(secondInsert);
  vConnection.addExpectedStatement(thirdInsert);

  TDatabaseOperation.INSERT.execute(vConnection, vDataSet);

  vConnection.checkCalls;
end;

initialization
  TTestInsertOperation.RegisterTest('InsertOperation');

end.
