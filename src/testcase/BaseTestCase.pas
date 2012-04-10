unit BaseTestCase;

interface

uses TestFramework, Classes;

type
  TBaseTestCase = class(TTestCase, ITest)
  private
  public
    procedure CheckEqualsAnsiString(expected: String; actual: String; msg: String = '');

    class function Suite: ITestSuite; override;

    class procedure RegisterTest(SuitePath: string);
  end;

  TBaseTestCaseClass = class of TBaseTestCase;

implementation

uses SysUtils, StrUtils, AppUtils;

type
  TTestCaseToTest = class
  private
    FList: TStringList;

    procedure LoadTestCasesEntry;
  public
    function hasTestCases: Boolean;

    function matchClass(AClassName: TClass): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

var
  _TestCasesEntry: TTestCaseToTest = nil;

{ TBaseTestCase }

procedure TBaseTestCase.CheckEqualsAnsiString(expected, actual, msg: String);
begin
  CheckEqualsString(AnsiUpperCase(expected), AnsiUpperCase(actual), msg);
end;

class procedure TBaseTestCase.RegisterTest(SuitePath: string);
begin
  if (not _TestCasesEntry.hasTestCases) or
     ((_TestCasesEntry.hasTestCases) and (_TestCasesEntry.matchClass(Self))) then
  TestFramework.RegisterTest(SuitePath, Self.Suite);
end;

class function TBaseTestCase.Suite: ITestSuite;
begin
  Result := TTestSuite.Create(Self);
end;

{ TTestCaseToTest }

constructor TTestCaseToTest.Create;
begin
  FList := TStringList.Create;

  LoadTestCasesEntry;
end;

destructor TTestCaseToTest.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTestCaseToTest.hasTestCases: Boolean;
begin
  Result := (FList.Count > 0); 
end;

procedure TTestCaseToTest.LoadTestCasesEntry;
var
  vClasses: String;
begin
  if TAppUtils.hasAppParam('TestCase', vClasses) then
  begin
    FList.Text := StringReplace(vClasses, ' ', sLineBreak, [rfReplaceAll]);
  end;
end;

function TTestCaseToTest.matchClass(AClassName: TClass): Boolean;
begin
  Result := (FList.IndexOf(AClassName.ClassName) >= 0);
end;

initialization
  _TestCasesEntry := TTestCaseToTest.Create; 

finalization
  FreeAndNil(_TestCasesEntry);

end.
