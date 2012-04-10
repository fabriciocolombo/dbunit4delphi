unit uDMGenerator;

interface

uses
  SysUtils, Classes, Forms,
  DatabaseConnection, DatabaseConnectionFactory, DatabaseConfig, DatabaseConfigDBX;

type
  TDMGenerator = class(TDataModule)
  private
    FConnection: IDatabaseConnection;
  public
    property Connection: IDatabaseConnection read FConnection;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DMGenerator: TDMGenerator;

implementation

{$R *.dfm}

{ TDMGenerator }

constructor TDMGenerator.Create(AOwner: TComponent);
var
  DBConfig: IDatabaseConfigDBX;
begin
  inherited;
  DBConfig := TDatabaseConfigDBX.Create;
  DBConfig.DriverName := 'Interbase';
  DBConfig.GetDriverFunc := 'getSQLDriverINTERBASE';
  DBConfig.LibraryName := 'dbexpint.dll';
  DBConfig.VendorLib := 'fbembed.dll';

  DBConfig.Database := ExtractFilePath(Application.ExeName) + '..\unittest\db\SAMPLE.FDB';
  DBConfig.UserName := 'sysdba';
  DBConfig.Password := 'masterkey';
  DBConfig.Params.Values['SQLDialect'] := '3';

  FConnection := ConnectionFactory.newConnection(DBConfig);
end;

destructor TDMGenerator.Destroy;
begin

  inherited;
end;

end.
