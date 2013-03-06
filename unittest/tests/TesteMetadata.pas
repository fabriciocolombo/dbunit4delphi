unit TesteMetadata;

interface

uses TestCaseExtension,  DatabaseConnection, DatabaseConnectionFactory, DB,
  DatabaseConnectionType, DataSet, DatabaseConfig;

type
  TTesteMetadata = class(TTestCaseExtension)
  private
    FConnection: IDatabaseConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure TabelaDualDeveTerUmCampoDummyDoTipoChar1;
    procedure TabelaXYZNaoExiste;
  end;  

implementation

uses StubDatabaseConfigDBX, TestUtils, TestFramework, Exceptions;

{ TTesteMetadata }

procedure TTesteMetadata.SetUp;
begin
  inherited;
  FConnection := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);
end;

procedure TTesteMetadata.TabelaDualDeveTerUmCampoDummyDoTipoChar1;
var
  vMetaData: TFieldListMetadata;
  vField: TField;
begin
  vMetaData := FConnection.getFields('Dual');
  try
    CheckEquals(1, vMetaData.Count, 'Field count');
    vField := vMetaData.Fields[0];
    CheckEqualsText('Dummy', vField.FieldName, 'FieldName');
    CheckEquals(Ord(ftString), Ord(vField.DataType), 'DataType');
    CheckEquals(1, vField.Size, 'Size');
    CheckFalse(vField.Required, 'Required');
  finally
    vMetaData.Free;
  end;
end;

procedure TTesteMetadata.TabelaXYZNaoExiste;
begin
  ExpectedException := ETableNotExists;
  FConnection.getFields('XYZ');
end;

procedure TTesteMetadata.TearDown;
begin
  FConnection := nil;
  inherited;
end;

initialization
  TTesteMetadata.RegisterTest('Metadata');

end.
