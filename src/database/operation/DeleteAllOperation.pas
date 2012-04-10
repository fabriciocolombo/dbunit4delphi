unit DeleteAllOperation;

interface

uses DatabaseOperation, DataSet, DatabaseConnection, Statement;

type
  TDeleteAllOperation = class(TInterfacedObject, IDatabaseOperation)
  public
    procedure execute(const AConnection: IDatabaseConnection;const ADataSet: IDataSetReadOnly);
  end;


implementation

uses StatementBuilder, StatementBuilderIntf;



{ TDeleteAllOperation }

procedure TDeleteAllOperation.execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
begin
  inherited;
   AConnection.Execute(TStatementBuilder.newDelete(ADataSet.getTableName).build);
end;

end.
