unit TestDatabaseConfig;

interface

uses TestCaseExtension, DatabaseConfig;

type
  TTestDatabaseConfig = class(TTestCaseExtension)
  published
    procedure TestConfigUniDac;
    procedure TestConfigDBX;
  end;

implementation

uses
  SysUtils, DatabaseConnectionType;

{ TTestDatabaseConfig }

procedure TTestDatabaseConfig.TestConfigDBX;
var
  vConfig: IDatabaseConfig;
begin
  vConfig := TDatabaseConfigFactory.CreateFromFile(ExtractFilePath(ParamStr(0)) + 'resources\Sample-DBConfig-DBX.ini');
  CheckNotNull(vConfig);
  CheckEqualsEnum(dctDBX, vConfig.DatabaseConnectionType, TypeInfo(TDatabaseConnectionType));
  CheckEquals('ORAHOME', vConfig.Database);
  CheckEquals('LOCALHOST', vConfig.HostName);
  CheckEquals('scott', vConfig.UserName);
  CheckEquals('tiger', vConfig.Password);
  CheckEquals('Oracle', vConfig.Params.Values['DriverName']);
  CheckEquals('getSQLDriverORACLE', vConfig.Params.Values['GetDriverFunc']);
end;

procedure TTestDatabaseConfig.TestConfigUniDac;
var
  vConfig: IDatabaseConfig;
begin
  vConfig := TDatabaseConfigFactory.CreateFromFile(ExtractFilePath(ParamStr(0)) + 'resources\Sample-DBConfig-UniDac.ini');
  CheckNotNull(vConfig);
  CheckEqualsEnum(dctUniDac, vConfig.DatabaseConnectionType, TypeInfo(TDatabaseConnectionType));
  CheckEquals('ORAHOME', vConfig.Database);
  CheckEquals('LOCALHOST', vConfig.HostName);
  CheckEquals('scott', vConfig.UserName);
  CheckEquals('tiger', vConfig.Password);
  CheckEquals('PostgreSQL', vConfig.Params.Values['DBProvider']);
end;

initialization
  TTestDatabaseConfig.RegisterTest();

end.
