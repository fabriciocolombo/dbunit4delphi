unit TestExportDataSet;

interface

uses TestCaseExtension,  Classes, DatabaseConnection;

type
  TTestExportDataSet = class(TTestCaseExtension)
  private
    FConnection : IDatabaseConnection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure ExportDualTable;
    procedure ExportDualAndPersonTable;
    procedure ExportDualAndPersonTableFromStringList;
    procedure ExportDualWithQuery;
    procedure ExportDualTableToFile;
  end;

implementation

uses TestUtils, DatabaseConnectionFactory,
  ExportDataSet, TestFramework, SysUtils;

const
  sDualFile = '%Temp%\Dual.xml';

  DataSetXml = '<DATASET>' + sLineBreak +
               '%s' + sLineBreak +
               '</DATASET>';

  DualXML = sIdentation + '<DUAL DUMMY="X"/>';

  PersonXml = sIdentation + '<PERSON ID="1" NAME="Fabricio" BIRTHDATE="1983-07-15" SALARY="1543,21"/>';

{ TTestExportDataSet }

procedure TTestExportDataSet.ExportDualAndPersonTable;
var
  vExpectedXml,
  vXmlDataSet: String;
begin
  vXmlDataSet := TDataExporter
                  .CreateWithConnection(FConnection)
                  .WithMultipleTables(['Dual', 'Person']).ExportAsXmlText;

  vExpectedXml := Format(DataSetXml, [DualXML + sLineBreak + PersonXml]);

  CheckEqualsString(vExpectedXml, vXmlDataSet);
end;

procedure TTestExportDataSet.ExportDualAndPersonTableFromStringList;
var
  vExpectedXml,
  vXmlDataSet: String;
  vList: TStrings;
begin
  vList := TStringList.Create;
  try
    vList.Add('Dual');
    vList.Add('Person');

    vXmlDataSet := TDataExporter
                    .CreateWithConnection(FConnection)
                    .WithMultipleTables(vList).ExportAsXmlText;

    vExpectedXml := Format(DataSetXml, [DualXML + sLineBreak + PersonXml]);

    CheckEqualsString(vExpectedXml, vXmlDataSet);
  finally
    vList.Free;
  end;
end;

procedure TTestExportDataSet.ExportDualTable;
var
  vExpectedXml,
  vXmlDataSet: String;
begin
  vXmlDataSet := TDataExporter
                  .CreateWithConnection(FConnection)
                  .WithTableName('Dual').ExportAsXmlText;

  vExpectedXml := Format(DataSetXml, [DualXML]);

  CheckEqualsString(vExpectedXml, vXmlDataSet);
end;

procedure TTestExportDataSet.ExportDualTableToFile;
var
  vExpectedXml,
  vFileName: String;
begin
  vFileName := ExtractFilePath(ParamStr(0)) + 'exportdata.xml';

  if FileExists(vFileName) then
  begin
    if not DeleteFile(vFileName) then
      Fail(Format('Can not delete de file "%s". Test aborted.',[vFileName]));
  end;

  vExpectedXml := Format(DataSetXml, [DualXML]);

  TDataExporter
    .CreateWithConnection(FConnection)
    .WithTableName('Dual')
      .ExportToXmlFile(vFileName);

  CheckTrue(FileExists(vFileName), Format('File "%s" are not created.',[vFileName]));

  CheckTrue(TTestUtils.EqualsFileContent(vFileName, vExpectedXml), 'File content is invalid.');

  DeleteFile(vFileName);
end;

procedure TTestExportDataSet.ExportDualWithQuery;
var
  vExpectedXml,
  vXmlDataSet: String;
begin
  vXmlDataSet := TDataExporter
                  .CreateWithConnection(FConnection)
                  .WithQueryText('Dual', 'SELECT * From Dual').ExportAsXmlText;

  vExpectedXml := Format(DataSetXml, [DualXML]);

  CheckEqualsString(vExpectedXml, vXmlDataSet);
end;

procedure TTestExportDataSet.SetUp;
begin
  inherited;
  FConnection := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);
  FConnection.StartTransaction;
end;

procedure TTestExportDataSet.TearDown;
begin
  inherited;
  FConnection.RollbackTransaction;
  FConnection := nil;
end;

initialization
  TTestExportDataSet.RegisterTest('ExportDataSet');

end.
