unit uDMGenerator;

interface

{$I DBUnit.inc}

uses
  SysUtils, Classes, Forms,
  DatabaseConnection, DatabaseConnectionFactory, DatabaseConfig, DBXFirebird, DB,
  SqlExpr, DBXDb2, DBXInterBase, DBXMsSQL, DBXMySQL, DBXOdbc,
  {$IFDEF ENABLE_UNIDAC} DatabaseConnectionUniDac,{$ENDIF}
  DBXOracle;

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
  DBConfig: IDatabaseConfig;
begin
  inherited;
  DBConfig := TDatabaseConfigFactory.CreateFromFile('DBConfig.ini');

  FConnection := ConnectionFactory.newConnection(DBConfig);
  FConnection.open;
end;

destructor TDMGenerator.Destroy;
begin

  inherited;
end;

end.
