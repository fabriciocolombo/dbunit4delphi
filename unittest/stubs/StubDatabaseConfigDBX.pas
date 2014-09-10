unit StubDatabaseConfigDBX;

interface

uses DatabaseConfig;

type
  TStubDatabaseConfigDBX = class(TDatabaseConfig)
  private
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TStubDatabaseConfigDBX }

procedure TStubDatabaseConfigDBX.AfterConstruction;
begin
  inherited;
  Database := 'stubdatabase';
  UserName := 'stubusername';
  Password := 'stubpw';
end;

end.
