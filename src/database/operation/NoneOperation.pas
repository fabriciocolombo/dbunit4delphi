unit NoneOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet;

type
  TNoneOperation = class(TInterfacedObject, IDatabaseOperation)
  public
    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
  end;


implementation



{ TNoneOperation }

procedure TNoneOperation.execute(const AConnection: IDatabaseConnection;const ADataSet: IDataSetReadOnly);
begin
  inherited;
  //Nothing to do
end;

end.
