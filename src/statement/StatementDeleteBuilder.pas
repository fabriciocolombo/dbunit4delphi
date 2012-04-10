unit StatementDeleteBuilder;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  CustomStatementBuilder, StatementBuilderIntf;

type
  IStatementDeleteBuilder = interface(IStatementBuilder)
  ['{A3E3AF1B-5717-4D9D-8F44-EE414521CEE3}']
    function addWhere(fieldName: String;fieldValue: String): IStatementDeleteBuilder; 
  end;

  TStatementDeleteBuilder = class(TCustomStatementBuilder, IStatementDeleteBuilder)
  private
  protected
    function buildCommand: String; override;
  public
    function addWhere(fieldName: String;fieldValue: String): IStatementDeleteBuilder; reintroduce;
  end;


implementation



{ TStatementDeleteBuilder }

function TStatementDeleteBuilder.addWhere(fieldName,fieldValue: String): IStatementDeleteBuilder;
begin
  Result := inherited addWhere(fieldName, fieldValue) as IStatementDeleteBuilder;
end;

function TStatementDeleteBuilder.buildCommand: String;
begin
  Result := Format('delete from %s%s', [TableName, getWhere]);
end;

end.
