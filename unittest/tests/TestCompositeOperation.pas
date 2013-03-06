unit TestCompositeOperation;

interface

uses TestCaseExtension,  DatabaseOperation, MockDatabaseConnection;

type
  TTestCompositeOperation = class(TTestCaseExtension)
  private
  public
  published
    procedure None_CompositeOperation;
  end;

implementation

uses MockDatabaseOperation;

{ TTestCompositeOperation }

procedure TTestCompositeOperation.None_CompositeOperation;
var
  vMock: IMockDatabaseOperation;
begin
  vMock := TMockDatabaseOperation.Create;
  vMock.setExpectationCalls(3);

  TDatabaseOperation.COMPOSITE([vMock, vMock, vMock]).execute(nil, nil);

  vMock.checkCalls;
end;

initialization
  TTestCompositeOperation.RegisterTest('CompositeOperation');

end.
