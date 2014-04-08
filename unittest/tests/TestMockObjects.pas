unit TestMockObjects;

interface

uses TestCaseExtension;

type
  TTestMockObjects = class(TTestCaseExtension)
  private
  public
  published
    procedure checkMockXmlValidator;
    procedure checkMockDatabaseOperation;
    procedure checkMockDatabaseConnection;
    procedure checkMockDatabaseConnectionStatement;
  end;

implementation

uses MockDatabaseConnection, MockDatabaseOperation, MockXmlValidator,
  Exceptions, TestFramework, Statement;

{ TTestMockObjects }

procedure TTestMockObjects.checkMockDatabaseConnection;
begin
  ExpectedException := EExpectationNotSatisfied;
  with TMockDatabaseConnection.Create as IMockDatabaseConnection do
  begin
    addExpectedStatement('select * from dual');
    checkCalls;
  end;
end;

procedure TTestMockObjects.checkMockDatabaseConnectionStatement;
begin
  ExpectedException := EMockException;
  with TMockDatabaseConnection.Create as IMockDatabaseConnection do
  begin
    addExpectedStatement('select * from dual');
    Execute(TStatement.Create('insert into dual') as IStatement);
    checkCalls;
  end;
end;

procedure TTestMockObjects.checkMockDatabaseOperation;
begin
  ExpectedException := EExpectationNotSatisfied;
  with TMockDatabaseOperation.Create as IMockDatabaseOperation do
  begin
    setExpectationCalls(1);
    checkCalls;
  end;
end;

procedure TTestMockObjects.checkMockXmlValidator;
begin
  ExpectedException := EExpectationNotSatisfied;
  with TMockXmlValidator.Create as IMockXmlValidator do
  begin
    setExpectationCalls(1);
    checkCalls;
  end;
end;

initialization
  TTestMockObjects.RegisterTest('MockObjects');

end.
