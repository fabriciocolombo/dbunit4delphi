unit UpdateOperation;

interface

uses DatabaseOperation, DatabaseConnection, DataSet, DB, Statement;

type
  TUpdateOperation = class(TInterfacedObject, IDatabaseOperation)
  public
    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);

    class function BuildUpdateStatement(const ADataSet: IDataSetReadOnly;const AMetadata: IFieldListMetadata): IStatement;
  end;


implementation

uses Formatter, StatementBuilder, StatementUpdateBuilder;



{ TUpdateOperation }

class function TUpdateOperation.BuildUpdateStatement(const ADataSet: IDataSetReadOnly;const AMetadata: IFieldListMetadata): IStatement;
var
  vUpdate: IStatementUpdateBuilder;
  int_field: Integer;
  vField, vFieldMetadata: TField;
  vFieldType: TFieldType;
begin
  vUpdate := TStatementBuilder.newUpdate(ADataSet.getTableName);

  for int_field := 0 to ADataSet.getFieldCount-1 do
  begin
    vField := ADataSet.getField(int_field);
    vFieldMetadata := AMetadata.FindField(vField.FieldName);

    if Assigned(vFieldMetadata) then
    begin
      vFieldType := vFieldMetadata.DataType;

      if AMetadata.IsPrimaryKeyField(vFieldMetadata) then
        vUpdate.addWhere(vField.FieldName, TFormatter.Format_Field(vField, vFieldType));
    end
    else
      vFieldType := vField.DataType;

    if not AMetadata.IsPrimaryKeyField(vFieldMetadata) then
      vUpdate.addFieldUpdate(vField.FieldName, TFormatter.Format_Field(vField, vFieldType));
  end;

  Result := vUpdate.build;
end;

procedure TUpdateOperation.execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
var
  vMetadata: IFieldListMetadata;
  vStatement: IStatement;
begin
  inherited;

  vMetadata := AConnection.getFields(ADataSet.getTableName);
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    vStatement := BuildUpdateStatement(ADataSet, vMetadata);

    AConnection.Execute(vStatement);

    ADataSet.Next;
  end;
end;

end.
