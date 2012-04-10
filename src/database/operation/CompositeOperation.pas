unit CompositeOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet, Classes;
type
  TCompositeOperation = class(TInterfacedObject, IDatabaseOperation)
  private
    FOperations: IInterfaceList;
  public
    constructor Create(Operations: array of IDatabaseOperation);
    destructor Destroy; override;

    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
  end;

implementation

{ TCompositeOperation }

constructor TCompositeOperation.Create(Operations: array of IDatabaseOperation);
var
  i: Integer;
  vOperation: IDatabaseOperation;
begin
  FOperations := TInterfaceList.Create;
  
  for i := Low(Operations) to High(Operations) do
  begin
    vOperation := Operations[i];
    
    FOperations.Add(vOperation);
  end;
end;

destructor TCompositeOperation.Destroy;
begin
  FOperations := nil;
  inherited;
end;

procedure TCompositeOperation.execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
var
  i: Integer;
  vOperation: IDatabaseOperation;
begin
  for i := 0 to FOperations.Count-1 do
  begin
    vOperation := (FOperations.Items[i] as IDatabaseOperation);

    vOperation.execute(AConnection, ADataSet);
  end;
end;

end.
