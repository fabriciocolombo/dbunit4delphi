unit StatementUpdateBuilder;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  CustomStatementBuilder, StatementBuilderIntf;

type
  IStatementUpdateBuilder = interface(IStatementBuilder)
  ['{28D3EB15-79FF-49A9-9FE0-080CC4C81431}']
    function addFieldUpdate(fieldName: String;fieldValue: String): IStatementUpdateBuilder;
    function addWhere(fieldName: String;fieldValue: String): IStatementUpdateBuilder;
  end;

  TStatementUpdateBuilder = class(TCustomStatementBuilder, IStatementUpdateBuilder)
  private
  protected
    function buildCommand: String; override;
  public
    function addFieldUpdate(fieldName: String;fieldValue: String): IStatementUpdateBuilder; reintroduce;
    function addWhere(fieldName: String;fieldValue: String): IStatementUpdateBuilder; reintroduce;
  end;


implementation



{ TStatementUpdateBuilder }

function TStatementUpdateBuilder.addFieldUpdate(fieldName,fieldValue: String): IStatementUpdateBuilder;
begin
  Result := inherited addFieldUpdate(fieldName, fieldValue) as IStatementUpdateBuilder;
end;

function TStatementUpdateBuilder.addWhere(fieldName,fieldValue: String): IStatementUpdateBuilder;
begin
  Result := inherited addWhere(fieldName, fieldValue) as IStatementUpdateBuilder;
end;

function TStatementUpdateBuilder.buildCommand: String;
begin
  Result := Format('update %s set %s%s',[TableName, getUpdateFields, getWhere]);
end;

end.
