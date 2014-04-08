unit StatementSelectBuilder;

interface

uses
  SysUtils, Statement, CustomStatementBuilder, StatementBuilderIntf;

type
  IStatementSelectBuilder = interface(IStatementBuilder)
  ['{09ED8ED0-331E-48F4-A9F7-1BABE167E84B}']
    function addField(fieldName: String): IStatementSelectBuilder;
    function addWhere(fieldName: String;fieldValue: String): IStatementSelectBuilder; 
  end;

  TStatementSelectBuilder = class(TCustomStatementBuilder, IStatementSelectBuilder)
  private
  protected
    function buildCommand: String; override;
  public
    function addField(fieldName: String): IStatementSelectBuilder; reintroduce;
    function addWhere(fieldName: String;fieldValue: String): IStatementSelectBuilder; reintroduce;
  end;


implementation

{ TStatementSelectBuilder }

function TStatementSelectBuilder.addField(fieldName: String): IStatementSelectBuilder;
begin
  Result := inherited addField(fieldName) as IStatementSelectBuilder;
end;

function TStatementSelectBuilder.addWhere(fieldName,fieldValue: String): IStatementSelectBuilder;
begin
  Result := inherited addWhere(fieldName, fieldValue) as IStatementSelectBuilder;
end;

function TStatementSelectBuilder.buildCommand: String;
begin
  Result := Format('select %s from %s%s', [getFields, TableName, getWhere]);
end;

end.
