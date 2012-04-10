unit MockDatabaseOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet;

type
  IMockDatabaseOperation = interface(IDatabaseOperation)
  ['{8D21151E-63B3-47F3-BDF5-54AD0B5A4EB7}']
    procedure setExpectationCalls(AQuantity: Integer);
    procedure checkCalls;
 
  end;

  TMockDatabaseOperation = class(TInterfacedObject, IDatabaseOperation, IMockDatabaseOperation)
  private
    FExpectedCalls: Integer;
    FCalls: Integer;
  public
    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);

    procedure checkCalls;
    procedure setExpectationCalls(AQuantity: Integer);
  end;

implementation

uses Exceptions;

{ TMockDatabaseOperation }

procedure TMockDatabaseOperation.checkCalls;
begin
  if (FExpectedCalls <> FCalls) then
    raise EExpectationNotSatisfacted.Create(FExpectedCalls, FCalls, 'execute');
end;

procedure TMockDatabaseOperation.execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
begin
  Inc(FCalls);
end;

procedure TMockDatabaseOperation.setExpectationCalls(AQuantity: Integer);
begin
  FExpectedCalls := AQuantity;
end;

end.
