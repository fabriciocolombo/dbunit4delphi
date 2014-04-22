unit TestRefreshOperation;

interface

uses TestCaseExtension,  DatabaseConnection, MockDatabaseOperation, DataSet,
  DatabaseOperation;

type
  TTestRefreshOperation = class(TTestCaseExtension)
  private
  published
    procedure RefreshPerson;
  end;

implementation

uses MockDatabaseConnection, XMLDomParseError, XmlDatasetReader,
  TestResources;

{ TTestRefreshOperation }

procedure TTestRefreshOperation.RefreshPerson;
const
  firstInsert  = 'INSERT INTO PERSON (ID, NAME, BIRTHDATE, SALARY) VALUES (1, ''FABRICIO'', ''1983-04-15'', 2153.47)';
  firstUpdate  = 'UPDATE PERSON SET NAME=''FABRICIO'', BIRTHDATE=''1983-04-15'', SALARY=2153.47 WHERE ID=1';
  secondInsert = 'INSERT INTO PERSON (ID, NAME, BIRTHDATE, SALARY) VALUES (2, ''PEDRO'', ''1986-07-23'', 1376.54)';
  secondUpdate = 'UPDATE PERSON SET NAME=''PEDRO'', BIRTHDATE=''1986-07-23'', SALARY=1376.54 WHERE ID=2';
var
  vDataSet: IDataSetReadOnly;
  vConnection: IMockDatabaseConnection;
begin
  vConnection := TMockDatabaseConnection.Create;
  vConnection.addExpectedStatement(firstUpdate);
  vConnection.addExpectedStatement(firstInsert);
  vConnection.addExpectedStatement(secondUpdate);
  vConnection.addExpectedStatement(secondInsert);

  vDataSet := TXmlDataSetBuilder.newFromFile(PersonXMLPath, vConnection).build;

  TDatabaseOperation.REFRESH.execute(vConnection, vDataSet);

  vConnection.checkCalls;
end;

initialization
  TTestRefreshOperation.RegisterTest('RefreshOperation');

end.
