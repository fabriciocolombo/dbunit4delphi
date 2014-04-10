unit DeleteOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet, DB;

type
  TDeleteOperation = class(TInterfacedObject, IDatabaseOperation)
  public
    procedure execute(const AConnection: IDatabaseConnection;const ADataSet: IDataSetReadOnly);
  end;


implementation

uses StatementBuilder, StatementDeleteBuilder, Formatter;



{ TDeleteOperation }

procedure TDeleteOperation.execute(const AConnection: IDatabaseConnection;const ADataSet: IDataSetReadOnly);
var
  vDelete: IStatementDeleteBuilder;
  int_field: Integer;
  vField, vFieldMetadata: TField;
  vMetadata: IFieldListMetadata;
  vFieldType: TFieldType;
begin
  inherited;

  vMetadata := AConnection.getFields(ADataSet.getTableName);

  ADataSet.First;
  while not ADataSet.Eof do
  begin
    vDelete := TStatementBuilder.newDelete(ADataSet.getTableName);

    for int_field := 0 to ADataSet.getFieldCount-1 do
    begin
      vField := ADataSet.getField(int_field);

      vFieldMetadata := vMetadata.FindField(vField.FieldName);

      if Assigned(vFieldMetadata) then
        vFieldType := vFieldMetadata.DataType
      else
        vFieldType := vField.DataType;

        if vMetadata.IsPrimaryKeyField(vFieldMetadata) then
          vDelete.addWhere(vField.FieldName, TFormatter.Format_Field(vField, vFieldType));
    end;

    AConnection.Execute(vDelete.build);

    ADataSet.Next;
  end;
end;

end.
