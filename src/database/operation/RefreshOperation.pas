unit RefreshOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet;

type
  TRefreshOperation = class(TInterfacedObject, IDatabaseOperation)
  public
    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
  end;


implementation

uses Statement, InsertOperation, UpdateOperation;



{ TRefreshOperation }

procedure TRefreshOperation.execute(const AConnection: IDatabaseConnection;const ADataSet: IDataSetReadOnly);
var
  vMetadata: TFieldListMetadata;
  vStatement: IStatement;
begin
  inherited;

  vMetadata := AConnection.getFields(ADataSet.getTableName);
  try
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      vStatement := TUpdateOperation.BuildUpdateStatement(ADataSet, vMetadata);

      if (AConnection.Execute(vStatement) <= 0) then
      begin
        vStatement := TInsertOperation.BuildInsertStatement(ADataSet, vMetadata);

        AConnection.Execute(vStatement);
      end;

      ADataSet.Next;
    end;
  finally
    vMetadata.Free;
  end;
end;

end.
