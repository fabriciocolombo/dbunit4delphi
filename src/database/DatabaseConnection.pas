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

  TAbstractDatabaseConnection = class abstract(TPersistent, IDatabaseConnection)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure open;virtual;abstract;
    procedure close;virtual;abstract;
    procedure configure(const config: IDatabaseConfig);virtual;abstract;
    function connected: Boolean;virtual;abstract;
    function createQuery: IQuery;virtual;abstract;
    function inTransaction: Boolean;virtual;abstract;
    procedure StartTransaction;virtual;abstract;
    procedure CommitTransaction;virtual;abstract;
    procedure RollbackTransaction;virtual;abstract;
    function realConnection: TObject;virtual;abstract;
    function Execute(const AStatement: IStatement): Integer;virtual;abstract;
    function getFields(ATableName: String): IFieldListMetadata;virtual;abstract;
    procedure getTableNames(AList: TStrings);virtual;abstract;
    property RefCount: Integer read FRefCount;
  public
  end;

  TAbstractDatabaseConnectionClass = class of TAbstractDatabaseConnection;

implementation

uses
  Windows;

{ TAbstractDatabaseConnection }

function TAbstractDatabaseConnection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TAbstractDatabaseConnection._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TAbstractDatabaseConnection._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

end.
