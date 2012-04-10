unit StatementInsertBuilder;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  CustomStatementBuilder, StatementBuilderIntf;

type
  IStatementInsertBuilder = interface(IStatementBuilder)
  ['{75A85C6D-D7D6-49B6-8BB3-7F78142969BC}']
    function addFieldInsert(fieldName: String;fieldValue: String): IStatementInsertBuilder; 
  end;

  TStatementInsertBuilder = class(TCustomStatementBuilder, IStatementInsertBuilder)
  private
  protected
    function buildCommand: String; override;
  public
    function addFieldInsert(fieldName: String;fieldValue: String): IStatementInsertBuilder; reintroduce;
  end;

implementation

{ TStatementInsertBuilder }

function TStatementInsertBuilder.addFieldInsert(fieldName,fieldValue: String): IStatementInsertBuilder;
begin
  Result := inherited addFieldInsert(fieldName, fieldValue) as IStatementInsertBuilder;
end;

function TStatementInsertBuilder.buildCommand: String;
begin
  Result := Format('insert into %s %s values %s', [TableName, getInsertFields, getInsertValues]);
end;

end.
