unit DatabaseConfigDBX;

interface

uses DatabaseConfig, Classes, IniFiles;

type
  IDatabaseConfigDBX = interface(IDatabaseConfig)
  ['{96111300-F115-4ADC-B694-DCC21AAEA127}']
    procedure SetDriverName(const Value: String);
    procedure SetGetDriverFunc(const Value: String);
    procedure SetLibraryName(const Value: String);
    procedure SetVendorLib(const Value: String);
    procedure SetParams(const Value: TStrings);
    function GetDriverName: String;
    function GetGetDriverFunc: String;
    function GetLibraryName: String;
    function GetVendorLib: String;
    function GetParams: TStrings;
    
    property DriverName: String read GetDriverName write SetDriverName;
    property GetDriverFunc: String read GetGetDriverFunc write SetGetDriverFunc;
    property LibraryName: String read GetLibraryName write SetLibraryName;
    property VendorLib: String read GetVendorLib write SetVendorLib;
    property Params: TStrings read GetParams write SetParams;
  end;

  TDatabaseConfigDBX = class(TDatabaseConfig, IDatabaseConfigDBX)
  private
    FGetDriverFunc: String;
    FLibraryName: String;
    FVendorLib: String;
    FDriverName: String;
    FParams: TStrings;
    procedure SetDriverName(const Value: String);
    procedure SetGetDriverFunc(const Value: String);
    procedure SetLibraryName(const Value: String);
    procedure SetVendorLib(const Value: String);
    procedure SetParams(const Value: TStrings);
    function GetDriverName: String;
    function GetGetDriverFunc: String;
    function GetLibraryName: String;
    function GetVendorLib: String;
    function GetParams: TStrings;
  public
    property DriverName: String read GetDriverName write SetDriverName;
    property GetDriverFunc: String read GetGetDriverFunc write SetGetDriverFunc;
    property LibraryName: String read GetLibraryName write SetLibraryName;
    property VendorLib: String read GetVendorLib write SetVendorLib;
    property Params: TStrings read GetParams write SetParams;

    constructor Create;override;
    destructor Destroy; override;

    function ConfigIsOk: Boolean; override;
    function ToString: String; override;

    procedure LoadFromFile(const AFileName: String);override;
  end;


implementation

uses SysUtils, DatabaseConnectionType;

{ TDatabaseConfigDBX }

function TDatabaseConfigDBX.ConfigIsOk: Boolean;
begin
  Result := inherited ConfigIsOk and
            (FDriverName <> EmptyStr) and
            (FGetDriverFunc <> EmptyStr) and
            (FLibraryName <> EmptyStr) and
            (FVendorLib <> EmptyStr);
end;

constructor TDatabaseConfigDBX.Create;
begin
  FParams := TStringList.Create;
  DatabaseConnectionType := dctDBX;
end;

destructor TDatabaseConfigDBX.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TDatabaseConfigDBX.GetDriverName: String;
begin
  Result := FDriverName;
end;

function TDatabaseConfigDBX.GetGetDriverFunc: String;
begin
  Result := FGetDriverFunc;
end;

function TDatabaseConfigDBX.GetLibraryName: String;
begin
  Result := FLibraryName;
end;

function TDatabaseConfigDBX.GetParams: TStrings;
begin
  Result := FParams;
end;

function TDatabaseConfigDBX.GetVendorLib: String;
begin
  Result := FVendorLib;
end;

procedure TDatabaseConfigDBX.LoadFromFile(const AFileName: String);
var
  vIniFile: TMemIniFile;
begin
  inherited;
  vIniFile := TMemIniFile.Create(AFileName);
  try
    FDriverName := vIniFile.ReadString(DB_CONFIG, 'DriverName', EmptyStr);
    FGetDriverFunc := vIniFile.ReadString(DB_CONFIG, 'GetDriverFunc', EmptyStr);
    FLibraryName := vIniFile.ReadString(DB_CONFIG, 'LibraryName', EmptyStr);
    FVendorLib := vIniFile.ReadString(DB_CONFIG, 'VendorLib', EmptyStr);
    Database := vIniFile.ReadString(DB_CONFIG, 'Database', EmptyStr);
    UserName := vIniFile.ReadString(DB_CONFIG, 'User_Name', EmptyStr);
    Password := vIniFile.ReadString(DB_CONFIG, 'Password', EmptyStr);

    FParams.Clear;
    vIniFile.ReadSectionValues(DB_CONFIG, FParams);
  finally
    vIniFile.Free;
  end;
end;

procedure TDatabaseConfigDBX.SetDriverName(const Value: String);
begin
  FDriverName := Trim(Value);
end;

procedure TDatabaseConfigDBX.SetGetDriverFunc(const Value: String);
begin
  FGetDriverFunc := Trim(Value);
end;

procedure TDatabaseConfigDBX.SetLibraryName(const Value: String);
begin
  FLibraryName := Trim(Value);
end;

procedure TDatabaseConfigDBX.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TDatabaseConfigDBX.SetVendorLib(const Value: String);
begin
  FVendorLib := Trim(Value);
end;

function TDatabaseConfigDBX.ToString: String;
begin
  Result := inherited toString + sLineBreak +
            'drivername=' + FDriverName + sLineBreak +
            'getdriverfunc=' + FGetDriverFunc +sLineBreak +
            'libraryname=' + FLibraryName + sLineBreak +
            'vendorlib=' + FVendorLib;


end;

end.
