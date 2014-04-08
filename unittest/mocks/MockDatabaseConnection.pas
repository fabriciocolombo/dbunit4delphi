unit MockDatabaseConnection;

interface

uses TestFramework, DatabaseConnection, DatabaseConfig, Statement, Query, Classes, DataSet,
     DB;

type
  IMockDatabaseConnection = interface(IDatabaseConnection)
  ['{5B713EEC-39E4-4267-8066-0EA2FC108318}']
    procedure addExpectedStatement(AStatements: String);

    procedure checkCalls;
  end;

  TMockDatabaseConnection = class(TInterfacedObject, IDatabaseConnection, IMockDatabaseConnection)
  private
    FExpectedStatements: TStrings;
    FStatements: TStrings;
  public
    function Execute(const AStatement: IStatement): Integer;
    function ExecuteWithParams(const AStatement: IStatement): Integer;

    procedure AfterConstruction; override;
    destructor Destroy; override;

    function getFields(ATableName: String): TFieldListMetadata;

    procedure addExpectedStatement(AStatements: String);
    procedure checkCalls;

    procedure CommitTransaction;

    procedure RollbackTransaction;
    procedure StartTransaction;
    procedure close;
    function connected: Boolean;
    function createQuery: IQuery;
    function inTransaction: Boolean;
    procedure open;
    function realConnection: TObject;
    procedure configure(const config: IDatabaseConfig);
    procedure getTableNames(AList: TStrings);
  end;

implementation

uses SysUtils, Exceptions;


{ TMockDatabaseConnection }

procedure TMockDatabaseConnection.addExpectedStatement(AStatements: String);
begin
  FExpectedStatements.Add(AStatements);
end;

procedure TMockDatabaseConnection.AfterConstruction;
begin
  inherited;
  FStatements := TStringList.Create;
  FExpectedStatements := TStringList.Create;
end;

destructor TMockDatabaseConnection.Destroy;
begin
  FExpectedStatements.Free;
  FStatements.Free;
  inherited;
end;

function TMockDatabaseConnection.Execute(const AStatement: IStatement): Integer;
begin
  Result := ExecuteWithParams(AStatement);
end;

function TMockDatabaseConnection.getFields(ATableName: String): TFieldListMetadata;
begin
  Result := TFieldListMetadata.Create;

  if SameText(ATableName, 'PERSON') then
  begin
    with Result.AddField('id', ftInteger) do
    begin
      Required := True;
      ProviderFlags := [pfInUpdate, pfInWhere, pfInKey];
    end;
    with Result.AddField('Name', ftString) do
    begin
      Size := 60;
    end;
    with Result.AddField('BirthDate', ftDate) do
    begin
    end;
    with Result.AddField('Salary', ftFloat) do
    begin

    end;
  end;
end;

function TMockDatabaseConnection.inTransaction: Boolean;
begin
  Result := False;
end;

procedure TMockDatabaseConnection.open;
begin

end;

function TMockDatabaseConnection.realConnection: TObject;
begin
  Result := Self;
end;

procedure TMockDatabaseConnection.RollbackTransaction;
begin

end;

procedure TMockDatabaseConnection.StartTransaction;
begin

end;

procedure TMockDatabaseConnection.checkCalls;
begin
  if (FExpectedStatements.Count <> FStatements.Count) then
    raise EExpectationNotSatisfied.Create(FExpectedStatements.Count, FStatements.Count, 'Execute');

  if AnsiCompareText(FExpectedStatements.Text, FStatements.Text) <> 0 then
    raise EMockException.CreateFmt('Expected Statements "%s" but was "%s"',[FExpectedStatements.Text, FStatements.Text]);
end;

procedure TMockDatabaseConnection.close;
begin

end;

procedure TMockDatabaseConnection.CommitTransaction;
begin

end;

procedure TMockDatabaseConnection.configure(const config: IDatabaseConfig);
begin

end;

function TMockDatabaseConnection.connected: Boolean;
begin
  Result := False;
end;

function TMockDatabaseConnection.createQuery: IQuery;
begin
  Result :=  nil;
end;

function TMockDatabaseConnection.ExecuteWithParams(const AStatement: IStatement): Integer;
begin
  FStatements.Add(AStatement.Command);

  Result := -1;
end;

procedure TMockDatabaseConnection.getTableNames(AList: TStrings);
begin

end;

end.
