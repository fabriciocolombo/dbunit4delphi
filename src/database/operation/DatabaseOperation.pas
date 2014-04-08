unit DatabaseOperation;

interface

uses
  DatabaseConnection, DataSet;

type
  IDatabaseOperation = interface
  ['{40FF6F9D-5001-4CDE-925C-32FB42A4AC8C}']
    procedure execute(const AConnection: IDatabaseConnection; const ADataSet: IDataSetReadOnly);
  end;

  TDatabaseOperation = class
  protected
  public
    class function CLEAN_INSERT: IDatabaseOperation;
    class function DELETE: IDatabaseOperation;
    class function DELETE_ALL: IDatabaseOperation;
    class function INSERT: IDatabaseOperation;
    class function NONE: IDatabaseOperation;
    class function REFRESH: IDatabaseOperation;
    class function UPDATE: IDatabaseOperation;
    class function COMPOSITE(Operations: array of IDatabaseOperation):IDatabaseOperation;
    //class function TRUNCATE_TABLE: IDatabaseOperation;
    //class function ORACLE_SQL_LOADER:IDatabaseOperation;
    //class function CLOSE_CONNECTION(Operation: TDatabaseOperation):IDatabaseOperation;
    //class function TRANSACTION(Operation: TDatabaseOperation):IDatabaseOperation;
  end;


implementation

uses InsertOperation, NoneOperation, DeleteAllOperation,
  CompositeOperation, DeleteOperation, RefreshOperation, UpdateOperation;

{
****************************** TDatabaseOperation ******************************
}
class function TDatabaseOperation.CLEAN_INSERT: IDatabaseOperation;
begin
  Result := TCompositeOperation.Create([DELETE_ALL, INSERT]);
end;

class function TDatabaseOperation.COMPOSITE(Operations: array of IDatabaseOperation): IDatabaseOperation;
begin
  Result := TCompositeOperation.Create(Operations);
end;

class function TDatabaseOperation.DELETE: IDatabaseOperation;
begin
  Result := TDeleteOperation.Create;
end;

class function TDatabaseOperation.DELETE_ALL: IDatabaseOperation;
begin
  Result := TDeleteAllOperation.Create;
end;

class function TDatabaseOperation.INSERT: IDatabaseOperation;
begin
  Result := TInsertOperation.Create;
end;

class function TDatabaseOperation.NONE: IDatabaseOperation;
begin
  Result := TNoneOperation.Create;
end;

class function TDatabaseOperation.REFRESH: IDatabaseOperation;
begin
  Result := TRefreshOperation.Create;
end;

class function TDatabaseOperation.UPDATE: IDatabaseOperation;
begin
  Result := TUpdateOperation.Create;
end;



end.
