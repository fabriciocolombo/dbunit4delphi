unit StatementBuilderIntf;

interface

uses DB, Statement;

type
  IStatementBuilder = interface
  ['{4D1934C3-015C-4BBA-86A3-7F86BEBF85F2}']
    function build: IStatement;

    function buildCommand: String;
    function buildParams: TParams;
  end;
implementation

end.
