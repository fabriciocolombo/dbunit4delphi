unit uDMGenerator;

interface

uses
  SysUtils, Classes, Forms,
  DatabaseConnection, DatabaseConnectionFactory, DatabaseConfig, DatabaseConfigDBX, Data.DBXFirebird, Data.DB,
  Data.SqlExpr, Data.DBXDb2, Data.DBXInterBase, Data.DBXMsSQL, Data.DBXMySQL, Data.DBXOdbc, Data.DBXOracle;

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
  DBConfig.LoadFromFile('DBConfig.ini');

  FConnection := ConnectionFactory.newConnection(DBConfig);
  FConnection.open;
end;

destructor TDMGenerator.Destroy;
begin

  inherited;
end;

end.
