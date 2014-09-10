unit DatabaseConfig;

interface

uses
  SysUtils, DatabaseConnectionType, Classes;

const
  DB_CONFIG = 'DBCONFIG';
  DB_EXTRA_PARAMS = 'EXTRA_PARAMS';

type
  IDatabaseConfig = interface
  ['{83B617CF-A429-4E26-B502-3CDC8818DA49}']
    procedure SetDatabase(const Value: string);
    procedure SetDatabaseConnectionType(const Value: TDatabaseConnectionType);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetParams(const Value: TStrings);
    function GetDatabase: string;
    function GetDatabaseConnectionType: TDatabaseConnectionType;
    function GetPassword: string;
    function GetUserName: string;
    function GetHostName: string;
    function GetParams: TStrings;

    property Database: string read GetDatabase write SetDatabase;
    property Password: string read GetPassword write SetPassword;
    property UserName: string read GetUserName write SetUserName;
    property HostName: String read GetHostName write SetHostName;
    property Params: TStrings read GetParams write SetParams;
    property DatabaseConnectionType: TDatabaseConnectionType read GetDatabaseConnectionType write SetDatabaseConnectionType;

    function getName: String;

    function ConfigIsOk: Boolean;
    function toString: String;
  end;

  TDatabaseConfig = class(TInterfacedObject, IDatabaseConfig)
  private
    FDatabase: string;
    FPassword: string;
    FUserName: string;
    FHostName: String;
    FParams: TStrings;
    FDatabaseConnectionType: TDatabaseConnectionType;
    procedure SetParams(const Value: TStrings);
    procedure SetHostName(const Value: String);
    procedure SetDatabase(const Value: string);
    procedure SetDatabaseConnectionType(const Value: TDatabaseConnectionType);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    function GetDatabase: string;
    function GetDatabaseConnectionType: TDatabaseConnectionType;
    function GetPassword: string;
    function GetUserName: string;
    function GetHostName: String;
    function GetParams: TStrings;
  public
    property Database: string read GetDatabase write SetDatabase;
    property Password: string read GetPassword write SetPassword;
    property UserName: string read GetUserName write SetUserName;
    property HostName: String read GetHostName write SetHostName;
    property Params: TStrings read GetParams write SetParams;
    property DatabaseConnectionType: TDatabaseConnectionType read GetDatabaseConnectionType write SetDatabaseConnectionType;

    function ConfigIsOk: Boolean;virtual;
    function ToString: String;override;
    
    function getName: String;

    constructor Create;virtual;
    destructor Destroy; override;

    class function newConfig(AType: TDatabaseConnectionType): IDatabaseConfig;
  end;

  TDatabaseConfigFactory = class
  public
    class function CreateFromFile(const AFileName: string): IDatabaseConfig;static;
  end;

implementation

uses
  IniFiles, TypInfo;

{ TDatabaseConfig }

function TDatabaseConfig.ConfigIsOk: Boolean;
begin
  Result := (FDatabase <> EmptyStr) and
            (FPassword <> EmptyStr) and
            (FUserName <> EmptyStr);
end;

constructor TDatabaseConfig.Create;
begin
  inherited;
  FParams := TStringList.Create;
end;

destructor TDatabaseConfig.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TDatabaseConfig.GetDatabase: string;
begin
  Result := FDatabase;
end;

function TDatabaseConfig.GetDatabaseConnectionType: TDatabaseConnectionType;
begin
  Result := FDatabaseConnectionType;
end;

function TDatabaseConfig.GetHostName: String;
begin
  Result := FHostName;
end;

function TDatabaseConfig.getName: String;
begin
  Result := ClassName;
end;

function TDatabaseConfig.GetParams: TStrings;
begin
  Result := FParams;
end;

function TDatabaseConfig.GetPassword: string;
begin
  Result := FPassword;
end;

function TDatabaseConfig.GetUserName: string;
begin
  Result := FUserName;
end;

class function TDatabaseConfig.newConfig(AType: TDatabaseConnectionType): IDatabaseConfig;
begin
  Result := Self.Create;
  Result.DatabaseConnectionType := AType;
end;

procedure TDatabaseConfig.SetDatabase(const Value: string);
begin
  FDatabase := Trim(Value);
end;

procedure TDatabaseConfig.SetDatabaseConnectionType(const Value: TDatabaseConnectionType);
begin
  FDatabaseConnectionType := Value;
end;

procedure TDatabaseConfig.SetHostName(const Value: String);
begin
  FHostName := Value;
end;

procedure TDatabaseConfig.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TDatabaseConfig.SetPassword(const Value: string);
begin
  FPassword := Trim(Value);
end;

procedure TDatabaseConfig.SetUserName(const Value: string);
begin
  FUserName := Trim(Value);
end;

function TDatabaseConfig.toString: String;
begin
  Result := ClassName + ':' + sLineBreak +
            'database=' + FDatabase + sLineBreak +
            'username=' + FUserName + sLineBreak +
            'password=' + FPassword + sLineBreak +
            'hostname=' + FHostName;
end;

{ TDatabaseConfigFactory }

class function TDatabaseConfigFactory.CreateFromFile(const AFileName: string): IDatabaseConfig;
var
  vIniFile: TMemIniFile;
  vDBTypeName: string;
  vDBType: TDatabaseConnectionType;
begin
  inherited;

  Result := nil;

  vIniFile := TMemIniFile.Create(AFileName);
  try
    vDBTypeName := vIniFile.ReadString(DB_CONFIG, 'ConnectionType', EmptyStr);
    vDBType := TDatabaseConnectionType(GetEnumValue(TypeInfo(TDatabaseConnectionType), vDBTypeName));

    Result := TDatabaseConfig.newConfig(vDBType);
    Result.Database := vIniFile.ReadString(DB_CONFIG, 'Database', EmptyStr);
    Result.UserName := vIniFile.ReadString(DB_CONFIG, 'User_Name', EmptyStr);
    Result.Password := vIniFile.ReadString(DB_CONFIG, 'Password', EmptyStr);
    Result.HostName := vIniFile.ReadString(DB_CONFIG, 'HostName', EmptyStr);

    Result.Params.Clear;
    vIniFile.ReadSectionValues(DB_CONFIG, Result.Params);

    Result.Params.Values['DriverName'] := vIniFile.ReadString(DB_CONFIG, 'DriverName', EmptyStr);
    Result.Params.Values['GetDriverFunc'] := vIniFile.ReadString(DB_CONFIG, 'GetDriverFunc', EmptyStr);
    Result.Params.Values['LibraryName'] := vIniFile.ReadString(DB_CONFIG, 'LibraryName', EmptyStr);
    Result.Params.Values['VendorLib'] := vIniFile.ReadString(DB_CONFIG, 'VendorLib', EmptyStr);
  finally
    vIniFile.Free;
  end;
end;

end.
