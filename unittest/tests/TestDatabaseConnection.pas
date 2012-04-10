unit TestDatabaseConnection;

interface

uses BaseTestCase, DatabaseConnection, Exceptions, DatabaseConnectionFactory,
     DatabaseConfig, DatabaseConnectionType, StubDatabaseConfig;

type
  TTestDatabaseConnection = class(TBaseTestCase)
  private
  public
  published
    procedure assignedFactory;
    procedure invalidDatabaseConnectionType;
    procedure invalidConfiguration;
  end;

implementation

{ TTestDatabaseConnection }

procedure TTestDatabaseConnection.assignedFactory;
begin
  CheckNotNull(ConnectionFactory);
end;

procedure TTestDatabaseConnection.invalidConfiguration;
begin
  ExpectedException := EInvalidDatabaseConfiguration;
  ConnectionFactory.newConnection(TDatabaseConfig.newConfig(dctDBX));
end;

procedure TTestDatabaseConnection.invalidDatabaseConnectionType;
begin
  ExpectedException := EInvalidDatabaseConnectionType;
  ConnectionFactory.newConnection(TDatabaseConfig.newConfig(TDatabaseConnectionType(-1)));
end;

initialization
  TTestDatabaseConnection.RegisterTest('DatabaseConnection');

end.
