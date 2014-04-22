unit TestDeleteOperation;

interface

uses TestCaseExtension;

type
  TTestDeleteOperation = class(TTestCaseExtension)
  private
  protected
  public
  published
    procedure deleteAllPerson;
    procedure deletePerson;
  end;

implementation

uses TestFramework, DataSet, MockDatabaseConnection, XmlDatasetReader,
  TestResources, DatabaseOperation;

{ TTestDeleteOperation }

{ TTestDeleteOperation }

procedure TTestDeleteOperation.deleteAllPerson;
var
  vDataSet: IDataSetReadOnly;
  vConnection: IMockDatabaseConnection;
begin
  vConnection := TMockDatabaseConnection.Create;
  vConnection.addExpectedStatement('DELETE FROM PERSON');

  vDataSet := TXmlDataSetBuilder.newFromFile(PersonXMLPath, vConnection).build;

  TDatabaseOperation.DELETE_ALL.execute(vConnection, vDataSet);

  vConnection.checkCalls;
end;

procedure TTestDeleteOperation.deletePerson;
const
  firstDelete  = 'DELETE FROM PERSON WHERE ID=1';
  secondDelete = 'DELETE FROM PERSON WHERE ID=2';
var
  vDataSet: IDataSetReadOnly;
  vConnection: IMockDatabaseConnection;
begin
  vConnection := TMockDatabaseConnection.Create;
  vConnection.addExpectedStatement(firstDelete);
  vConnection.addExpectedStatement(secondDelete);

  vDataSet := TXmlDataSetBuilder.newFromFile(PersonXMLPath, vConnection).build;

  TDatabaseOperation.DELETE.execute(vConnection, vDataSet);

  vConnection.checkCalls;
end;

initialization
  TTestDeleteOperation.RegisterTest('DeleteOperation');

end.
