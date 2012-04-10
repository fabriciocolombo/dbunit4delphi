unit StubDatabaseConfigDBX;

interface

uses DatabaseConfigDBX;

{$I ..\..\src\dbunit4delphi.inc}

type
  TStubDatabaseConfigDBX = class(TDatabaseConfigDBX)
  private
  public
    procedure AfterConstruction; override;
  end;

implementation

uses DatabaseConfig;

{ TStubDatabaseConfigDBX }

procedure TStubDatabaseConfigDBX.AfterConstruction;
begin
  inherited;
  Database := 'stubdatabase';
  UserName := 'stubusername';
  Password := 'stubpw';
  DriverName := 'InterBase';
  GetDriverFunc := 'stubgetdriverfunc';
  LibraryName := 'stublibraryname';
  VendorLib := 'stubvendorlib';
end;

end.
