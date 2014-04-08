unit TestUtils;

interface

uses TestFramework, DatabaseConfig, DatabaseConfigDBX, Forms, SysUtils, Classes,
     (* Register DBX Driver*)
     Data.DBXFirebird, Data.DbxOracle;

type
  TTestUtils = class
  private
  public
    class function DATABASECONFIGDBX: TDatabaseConfigDBX;

    class procedure ChangeSystemDecimalSeparator(ANewSeparator: Char;var AOldSeparator: Char);

    class function EqualsFileContent(AFileName: TFileName; AContent: String): Boolean;
  end;

implementation

uses DatabaseConnectionType, StrUtils;

{ TTestUtils }

class procedure TTestUtils.ChangeSystemDecimalSeparator(ANewSeparator: Char;var AOldSeparator: Char);
begin
  AOldSeparator := FormatSettings.DecimalSeparator;

  FormatSettings.DecimalSeparator := ANewSeparator;
end;

class function TTestUtils.DATABASECONFIGDBX: TDatabaseConfigDBX;
begin
  Result := TDatabaseConfigDBX.Create;
  Result.DriverName := 'Firebird';
  Result.GetDriverFunc := 'getSQLDriverINTERBASE';
  Result.LibraryName := 'dbxfb.dll';
  Result.VendorLib := 'fbclient.dll';
  Result.Database := 'localhost:' + ExtractFilePath(Application.ExeName) + '..\unittest\db\SAMPLE.FDB';
  Result.UserName := 'sysdba';
  Result.Password := 'masterkey';
  Result.Params.Values['SQLDialect'] := '3';
end;

class function TTestUtils.EqualsFileContent(AFileName: TFileName;AContent: String): Boolean;
var
  vFileStream: TFileStream;
  vStringStream: TStringStream;
begin
  vFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    vFileStream.Seek(0, soFromBeginning);

    vStringStream := TStringStream.Create(EmptyStr);
    try
      vStringStream.CopyFrom(vFileStream, vFileStream.Size);
      vStringStream.Seek(0, soFromBeginning);

      Result := CompareStr(AContent, vStringStream.DataString) = 0;      
    finally
      vStringStream.Free;
    end;
  finally
    vFileStream.Free;
  end;
end;

end.
