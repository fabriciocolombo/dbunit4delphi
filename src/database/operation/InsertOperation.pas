unit InsertOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet, StatementBuilder, Statement,
     StatementInsertBuilder, DB;

type
  TInsertOperation = class(TInterfacedObject, IDatabaseOperation)
  private
  public
    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);

    class function BuildInsertStatement(const ADataSet: IDataSetReadOnly;AMetadata: TFieldListMetadata): IStatement;
  end;


implementation

uses Formatter;

{ TInsertOperation }

class function TInsertOperation.BuildInsertStatement(const ADataSet: IDataSetReadOnly; AMetadata: TFieldListMetadata): IStatement;
var
  vInsert: IStatementInsertBuilder;
  int_field: Integer;
  vField, vFieldMetadata: TField;
  vFieldType: TFieldType;
begin
  vInsert := TStatementBuilder.newInsert(ADataSet.getTableName);

  for int_field := 0 to ADataSet.getFieldCount-1 do
  begin
    vField := ADataSet.getField(int_field);

    vFieldMetadata := AMetadata.FindField(vField.FieldName);

    if Assigned(vFieldMetadata) then
      vFieldType := vFieldMetadata.DataType
    else
      vFieldType := vField.DataType;

    vInsert.addFieldInsert(vField.FieldName, TFormatter.Format_Field(vField, vFieldType));
  end;

  Result := vInsert.build;
end;

procedure TInsertOperation.execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
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
      vStatement := BuildInsertStatement(ADataSet, vMetadata);

      AConnection.Execute(vStatement);

      ADataSet.Next;
    end;
  finally
    vMetadata.Free;
  end;
end;

end.
