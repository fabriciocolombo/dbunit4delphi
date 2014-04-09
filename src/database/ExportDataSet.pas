unit ExportDataSet;

interface

uses DatabaseConnection, Classes, DB, SysUtils;

type
  IExportableDataSet = interface
  ['{716EEAF5-D194-43DA-943B-F2EA0397415A}']
    function GetTableName: String;
    function QueryText: String;
  end;

  TExportQuery = class(TInterfacedObject, IExportableDataSet)
  private
    FTableName: String;
    FQueryText: String;
  public
    constructor Create(ATableName, AQueryText: String);

    function QueryText: String;
    function GetTableName: String;
  end;

  IDataExporter = interface
  ['{C5EA9303-DF93-4E90-85C9-B0F0E8220324}']
    function WithTableName(ATableName: String): IDataExporter;
    function WithQueryText(ATableName, AQueryText: String): IDataExporter;

    function WithMultipleTables(ATableNames: Array of String): IDataExporter;overload;
    function WithMultipleTables(ATableNames: TStrings): IDataExporter;overload;

    function ExportAsXmlText: String;
    procedure ExportToXmlFile(const AFileName: String; AEncoding: TEncoding);
  end;

  TDataExporter = class(TInterfacedObject, IDataExporter)
  private
    FConnection: IDatabaseConnection;
    FDataSets: IInterfaceList;
    FXml: String;

    constructor Create(const AConnection: IDatabaseConnection);

    function WithDataSet(AExportableDataSet: IExportableDataSet): IDataExporter;

    procedure AppendDataSet(AExportableDataSet: IExportableDataSet);
  public
    class function CreateWithConnection(const AConnection: IDatabaseConnection): IDataExporter;

    function ExportAsXmlText: String;

    function WithTableName(ATableName: String): IDataExporter;
    function WithQueryText(ATableName, AQueryText: String): IDataExporter;

    function WithMultipleTables(ATableNames: Array of String): IDataExporter;overload;
    function WithMultipleTables(ATableNames: TStrings): IDataExporter;overload;

    procedure ExportToXmlFile(const AFileName: String; AEncoding: TEncoding);
  end;

const
  sRootDataSet = 'DATASET';
  sIdentation = '  ';

implementation

uses StatementBuilder, Query, Formatter;

{ TDataExporter }

procedure TDataExporter.AppendDataSet(AExportableDataSet: IExportableDataSet);
var
  vQuery: IQuery;
  vTableName, vFieldsFormat: String;
  int_Field: Integer;
  vField: TField;
begin
  vQuery := FConnection.createQuery;
  vQuery.Sql.Text := AExportableDataSet.QueryText;
  vQuery.Open;

  if vQuery.IsEmpty then Exit;

  vTableName := AnsiUpperCase(AExportableDataSet.GetTableName);

  vQuery.First;
  while not vQuery.Eof do
  begin
    vFieldsFormat := EmptyStr;

    for int_Field := 0 to vQuery.FieldCount-1 do
    begin
      vField := vQuery.Fields[int_Field];
      vFieldsFormat := vFieldsFormat + Format('%s="%s" ',[vField.FieldName, TFormatterExport.Format_Field(vField)]);
    end;

    vFieldsFormat := sIdentation + Format('<%s %s/>', [vTableName, TrimRight(vFieldsFormat)]) + sLineBreak;
    
    FXml := FXml + vFieldsFormat;

    vQuery.Next;
  end;

  FXml := FXml; 
end;

constructor TDataExporter.Create(const AConnection: IDatabaseConnection);
begin
  FConnection := AConnection;
  FDataSets := TInterfaceList.Create;
end;

class function TDataExporter.CreateWithConnection(const AConnection: IDatabaseConnection): IDataExporter;
begin
  Result := TDataExporter.Create(AConnection);
end;

function TDataExporter.ExportAsXmlText: String;
var
  i: Integer;
  vExportable: IExportableDataSet;
begin
  FXml := EmptyStr;
  for i := 0 to FDataSets.Count-1 do
  begin
    vExportable := FDataSets.Items[i] as IExportableDataSet;

    AppendDataSet(vExportable);
  end;

  Result := Format('<%s>' + sLineBreak,[sRootDataSet]) +
            FXml + 
            Format('</%s>',[sRootDataSet]);
end;

procedure TDataExporter.ExportToXmlFile(const AFileName: String; AEncoding: TEncoding);
var
  vWriter: TStreamWriter;
begin
  vWriter := TStreamWriter.Create(TFileStream.Create(AFileName, fmCreate or fmOpenWrite), AEncoding);
  try
    vWriter.OwnStream;
    vWriter.Write(ExportAsXmlText);
  finally
    vWriter.Free;
  end;
end;

function TDataExporter.WithDataSet(AExportableDataSet: IExportableDataSet): IDataExporter;
begin
  FDataSets.Add(AExportableDataSet);

  Result := Self;
end;

function TDataExporter.WithMultipleTables(ATableNames: Array of String): IDataExporter;
var
  i: Integer;
begin
  for i := Low(ATableNames) to High(ATableNames) do
  begin
    WithTableName(ATableNames[i]);
  end;

  Result := Self;
end;

function TDataExporter.WithMultipleTables(ATableNames: TStrings): IDataExporter;
var
  i: Integer;
begin
  for i := 0 to ATableNames.Count-1 do
  begin
    WithTableName(ATableNames[i]);
  end;

  Result := Self;
end;

function TDataExporter.WithQueryText(ATableName, AQueryText: String): IDataExporter;
begin
  Result := WithDataSet(TExportQuery.Create(ATableName, AQueryText))
end;

function TDataExporter.WithTableName(ATableName: String): IDataExporter;
begin
  Result := WithDataSet(TExportQuery.Create(ATableName, TStatementBuilder.newSelect(ATableName).buildCommand));
end;

{ TExportQuery }

constructor TExportQuery.Create(ATableName, AQueryText: String);
begin
  FQueryText := AQueryText;
  FTableName := ATableName;
end;

function TExportQuery.GetTableName: String;
begin
  Result := FTableName;
end;

function TExportQuery.QueryText: String;
begin
  Result := FQueryText;
end;

end.
