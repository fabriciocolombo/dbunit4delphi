unit TestUpdateOperation;

interface

uses TestCaseExtension;

type
  TTestUpdateOperation = class(TTestCaseExtension)
  published
    procedure UpdatePerson;
  end;


implementation

uses DataSet, MockDatabaseConnection, XmlDatasetReader, TestResources,
  DatabaseOperation;

{ TTestUpdateOperation }


{ TTestUpdateOperation }

procedure TTestUpdateOperation.UpdatePerson;
const
  firstUpdate  = 'UPDATE PERSON SET NAME=''FABRICIO'', BIRTHDATE=''1983-04-15'', SALARY=2153.47 WHERE ID=1';
  secondUpdate = 'UPDATE PERSON SET NAME=''PEDRO'', BIRTHDATE=''1986-07-23'', SALARY=1376.54 WHERE ID=2';
var
  vDataSet: IDataSetReadOnly;
  vConnection: IMockDatabaseConnection;
begin
  vDataSet := TXmlDataSetBuilder.newFromFile(PersonXMLPath).build;

  vConnection := TMockDatabaseConnection.Create;
  vConnection.addExpectedStatement(firstUpdate);
  vConnection.addExpectedStatement(secondUpdate);

  TDatabaseOperation.UPDATE.execute(vConnection, vDataSet);

  vConnection.checkCalls;
end;

initialization
  TTestUpdateOperation.RegisterTest('UpdateOperation');

end.
