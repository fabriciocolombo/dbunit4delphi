unit DatabaseConnectionFactory;

interface

uses DatabaseConnection, DatabaseConfig, DatabaseConnectionType, Exceptions;

type
  IDatabaseConnectionFactory = interface
  ['{844D387C-FE80-41A0-85FA-477C189D69A2}']
    function newConnection(const Config: IDatabaseConfig): IDatabaseConnection;
  end;

  function ConnectionFactory: IDatabaseConnectionFactory;

implementation

uses DatabaseConnectionDBX, SysUtils, TypInfo;

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
    function newConnection(const Config: IDatabaseConfig): IDatabaseConnection;
  end;

{ TDatabaseConnectionFactory }

function TDatabaseConnectionFactory.newConnection(const Config: IDatabaseConfig): IDatabaseConnection;
begin
  case Config.DatabaseConnectionType of
    dctDBX: Result := TDatabaseConnectionDBX.Create;
  else
    raise EInvalidDatabaseConnectionType.Create(Config.DatabaseConnectionType);
  end;

  if not Config.ConfigIsOk then
  begin
    raise EInvalidDatabaseConfiguration.CreateFmt('%s:%s: invalid configuration. Verify properties: %s%s',
                                                  [ClassName, Config.getName, sLineBreak, Config.toString]);
  end;

  Result.configure(Config);
end;

initialization
  _DatabaseConnectionFactory := TDatabaseConnectionFactory.Create;

end.
