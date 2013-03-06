unit DatabaseConfig;

interface

{$I ..\dbunit4delphi.inc}
uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  DatabaseConnectionType;

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
    function GetDatabase: string;
    function GetDatabaseConnectionType: TDatabaseConnectionType;
    function GetPassword: string;
    function GetUserName: string;

    property Database: string read GetDatabase write SetDatabase;
    property Password: string read GetPassword write SetPassword;
    property UserName: string read GetUserName write SetUserName;
    property DatabaseConnectionType: TDatabaseConnectionType read GetDatabaseConnectionType write SetDatabaseConnectionType;

    function getName: String;

    function ConfigIsOk: Boolean;
    function toString: String;

    procedure LoadFromFile(const AFileName: String);    
  end;

  TDatabaseConfig = class(TInterfacedObject, IDatabaseConfig)
  private
    FDatabase: string;
    FPassword: string;
    FUserName: string;
    FDatabaseConnectionType: TDatabaseConnectionType;
  protected
    procedure SetDatabase(const Value: string);
    procedure SetDatabaseConnectionType(const Value: TDatabaseConnectionType);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    function GetDatabase: string;
    function GetDatabaseConnectionType: TDatabaseConnectionType;
    function GetPassword: string;
    function GetUserName: string;
  public
    property Database: string read GetDatabase write SetDatabase;
    property Password: string read GetPassword write SetPassword;
    property UserName: string read GetUserName write SetUserName;
    property DatabaseConnectionType: TDatabaseConnectionType read GetDatabaseConnectionType write SetDatabaseConnectionType;

    function ConfigIsOk: Boolean;virtual;
    function ToString: String;{$IFDEF D2009UP}override;{$ELSE}virtual;{$ENDIF}
    
    function getName: String;

    constructor Create;virtual;

    class function newConfig(AType: TDatabaseConnectionType): IDatabaseConfig;

    procedure LoadFromFile(const AFileName: String);virtual;abstract;
  end;

  //TODO - Configurar a conexão a partir de uma conexão pré-existente

implementation


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
end;

function TDatabaseConfig.GetDatabase: string;
begin
  Result := FDatabase;
end;

function TDatabaseConfig.GetDatabaseConnectionType: TDatabaseConnectionType;
begin
  Result := FDatabaseConnectionType;
end;

function TDatabaseConfig.getName: String;
begin
  Result := ClassName;
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
            'password=' + FPassword;
end;

end.
