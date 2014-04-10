unit DatabaseConnection;

interface

uses
  SysUtils, Classes, DatabaseConfig, DataSet, Query, Statement, DB;

type
  IDatabaseConnection = interface
  ['{3110F532-BEBB-4D3A-9FA1-2EDD1DDF9D42}']
    procedure open;
    procedure close;

    procedure configure(const config: IDatabaseConfig);

    function connected: Boolean;

    function createQuery: IQuery;

    function inTransaction: Boolean;
    
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;

    function realConnection: TObject;

    function Execute(const AStatement: IStatement): Integer;

    function getFields(ATableName: String): IFieldListMetadata;
    procedure getTableNames(AList: TStrings); 
  end;

  IDatabaseConnectionProvider = interface
  ['{520470FC-0111-47EB-9806-BC12CA5FB319}']
  end;

implementation


end.
