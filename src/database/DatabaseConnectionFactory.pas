unit DatabaseConnectionFactory;

interface

uses DatabaseConnection, DatabaseConfig, DatabaseConnectionType, Exceptions;

type
  IDatabaseConnectionFactory = interface
  ['{844D387C-FE80-41A0-85FA-477C189D69A2}']
    function newConnection(const Config: IDatabaseConfig; AOpened: Boolean = True): IDatabaseConnection;
  end;

  function ConnectionFactory: IDatabaseConnectionFactory;

implementation

uses DatabaseConnectionDBX,
     SysUtils, TypInfo, System.Classes, DatabaseConnectionRegistry;

var
  _DatabaseConnectionFactory: IDatabaseConnectionFactory = nil;

function ConnectionFactory: IDatabaseConnectionFactory;
begin
  Result := _DatabaseConnectionFactory;
end;

type
  TDatabaseConnectionFactory = class(TInterfacedObject, IDatabaseConnectionFactory)
  private
  public
    function newConnection(const Config: IDatabaseConfig; AOpened: Boolean = True): IDatabaseConnection;
  end;

{ TDatabaseConnectionFactory }

function TDatabaseConnectionFactory.newConnection(const Config: IDatabaseConfig; AOpened: Boolean): IDatabaseConnection;
var
  vClass: TClass;
begin
  if Config.DatabaseConnectionType = dctUndefined then
  begin
    raise EInvalidDatabaseConnectionType.Create(Config.DatabaseConnectionType);
  end;

  vClass := TDatabaseConnectionRegistry.Resolve(Config.DatabaseConnectionType);

  Result := TAbstractDatabaseConnectionClass(vClass).Create;

  if not Config.ConfigIsOk then
  begin
    raise EInvalidDatabaseConfiguration.CreateFmt('%s:%s: invalid configuration. Verify properties: %s%s',
                                                  [ClassName, Config.getName, sLineBreak, Config.toString]);
  end;

  Result.configure(Config);

  if AOpened then
  begin
    Result.open;
  end;
end;

initialization
  _DatabaseConnectionFactory := TDatabaseConnectionFactory.Create;

end.
