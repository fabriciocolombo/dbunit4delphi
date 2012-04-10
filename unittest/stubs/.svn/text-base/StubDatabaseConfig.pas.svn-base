unit StubDatabaseConfig;

interface

uses DatabaseConfig;

type
  TStubDatabaseConfig = class(TDatabaseConfig)
  private
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TStubDatabaseConfigStub }

procedure TStubDatabaseConfig.AfterConstruction;
begin
  inherited;
  Database := 'stubdatabase';
  UserName := 'stubuser';
  Password := 'stubpw';
end;

end.
