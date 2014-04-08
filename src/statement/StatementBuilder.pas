unit StatementBuilder;

interface

uses
  StatementDeleteBuilder, StatementSelectBuilder, StatementUpdateBuilder,
  StatementInsertBuilder;

type
  TStatementBuilder = class(TObject)
  public
    class function newDelete(tableName: String): IStatementDeleteBuilder;
    class function newInsert(tableName: String): IStatementInsertBuilder;
    class function newSelect(tableName: String): IStatementSelectBuilder;
    class function newUpdate(tableName: String): IStatementUpdateBuilder;
  end;

implementation

{
****************************** TStatementBuilder *******************************
}
class function TStatementBuilder.newDelete(tableName: String): IStatementDeleteBuilder;
begin
  Result := TStatementDeleteBuilder.Create(tableName);
end;

class function TStatementBuilder.newInsert(tableName: String): IStatementInsertBuilder;
begin
  Result := TStatementInsertBuilder.Create(tableName);
end;

class function TStatementBuilder.newSelect(tableName: String): IStatementSelectBuilder;
begin
  Result := TStatementSelectBuilder.Create(tableName);
end;

class function TStatementBuilder.newUpdate(tableName: String): IStatementUpdateBuilder;
begin
  Result := TStatementUpdateBuilder.Create(tableName);
end;

end.
