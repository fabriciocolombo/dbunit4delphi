unit TestXmlDatasetReader;

interface

uses TestCaseExtension,  DataSet, DataSetDecorator, DataSetListBuilder, XmlClientDataSet,
  XmlDatasetReader, XmlValidator, TestResources, Classes, Forms, Windows;

type
  TTestXmlDatasetReader = class(TTestCaseExtension)
  private
  public
  published
    procedure FileNotFound;
    procedure CheckTablesSampleFile;
    procedure CheckDataSampleFile;
    procedure InvalidXML_NodeNotClosedStartedTag;
    procedure InvalidXML_NodeNotClosedTag;
    procedure LoadFromXmlText;
    procedure LoadFromStream;
    procedure LoadFromFile_EmptyXML;
    procedure LoadFromXmlText_EmptyXML;
    procedure LoadFromStream_EmptyXML;
    procedure NoFieldDefinition;
    procedure withDataSet;
    procedure withValidator;
    procedure DataSetWithNullValues;
  end;

implementation

uses Exceptions, TestFramework, MockDataSet, SysUtils, MockXmlValidator;

{ TTestXmlDatasetReader }

procedure TTestXmlDatasetReader.FileNotFound;
begin
  ExpectedException := Exceptions.EFileNotFoundException;

  TXmlDataSetBuilder.newFromFile('c:\xyz.xyz', nil).build;
end;

procedure TTestXmlDatasetReader.CheckTablesSampleFile;
var
  vIterator: IDataSetIterator;
begin
  vIterator := TXmlDataSetBuilder.newFromFile(SampleXMLPath, nil).buildIterator;

  CheckNotNull(vIterator);

  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckNotNull(vIterator.Next);
  CheckFalse(vIterator.HasNext);
end;

procedure TTestXmlDatasetReader.DataSetWithNullValues;
const
  sInitialXml = '<root><tabela col1="col1 row1" col2="null"/></root>';
var
  vDataSet: IDataSetReadOnly;
begin
  vDataSet := TXmlDataSetBuilder.newFromText(sInitialXml).build;

  CheckNotNull(vDataSet);
  CheckEquals(2, vDataSet.getFieldCount);
  CheckEqualsString('tabela', vDataSet.getTableName);
  CheckFalse(vDataSet.Eof);

  CheckEqualsString('col1 row1', vDataSet.getField(0).AsString);
  CheckTrue(vDataSet.getField(1).IsNull, 'Field must be null');
end;

procedure TTestXmlDatasetReader.CheckDataSampleFile;
var
  vIterator: IDataSetIterator;
  vDataset: IDataSet;
begin
  vIterator := TXmlDataSetBuilder.newFromFile(SampleXMLPath, nil).buildIterator;

  CheckNotNull(vIterator);

  vDataset := vIterator.Next;
  CheckEqualsString('TEST_TABLE', vDataset.getTableName);
  CheckEquals(3, vDataset.getFieldCount);

  vDataset.First;
  CheckEqualsString('COL0', vDataset.getField(0).FieldName);
  CheckEqualsString('row 0 col 0', vDataset.getField(0).AsString);
  CheckEqualsString('COL1', vDataset.getField(1).FieldName);
  CheckEqualsString('row 0 col 1', vDataset.getField(1).AsString);
  CheckEqualsString('COL2', vDataset.getField(2).FieldName);
  CheckEqualsString('row 0 col 2', vDataset.getField(2).AsString);
  vDataset.Next;

  CheckEqualsString('COL0', vDataset.getField(0).FieldName);
  CheckEqualsString('row 1 col 0', vDataset.getField(0).AsString);
  CheckEqualsString('COL1', vDataset.getField(1).FieldName);
  CheckEqualsString('row 1 col 1', vDataset.getField(1).AsString);
  vDataset.Next;

  CheckTrue(vDataset.Eof);

  vDataset := vIterator.Next;
  CheckEqualsString('COL0', vDataset.getField(0).FieldName);
  CheckEqualsString('row 0 col 0', vDataset.getField(0).AsString);
  CheckEqualsString('COL1', vDataset.getField(1).FieldName);
  CheckEqualsString('row 0 col 1', vDataset.getField(1).AsString);
  vDataset.Next;

  CheckTrue(vDataset.Eof);
  CheckFalse(vIterator.HasNext);
end;

procedure TTestXmlDatasetReader.LoadFromXmlText;
const
  sInitialXml = '<root><tabela col1="col1 row1" col2="col2 row1"/></root>';
var
  vDataSet: IDataSetReadOnly;
begin
  vDataSet := TXmlDataSetBuilder.newFromText(sInitialXml).build;

  CheckNotNull(vDataSet);
  CheckEquals(2, vDataSet.getFieldCount);
  CheckEqualsString('tabela', vDataSet.getTableName);
  CheckFalse(vDataSet.Eof);

  CheckEqualsString('col1 row1', vDataSet.getField(0).AsString);
  CheckEqualsString('col2 row1', vDataSet.getField(1).AsString);
end;

procedure TTestXmlDatasetReader.LoadFromStream;
const
  sInitialXml = '<root><tabela col1="col1 row1" col2="col2 row1"/></root>';
var
  vStream: TStringStream;
  vDataSet: IDataSetReadOnly;
begin
  vStream := TStringStream.Create(sInitialXml);
  try
    vDataSet := TXmlDataSetBuilder.newFromStream(vStream).build;

    CheckNotNull(vDataSet);
    CheckEquals(2, vDataSet.getFieldCount);
    CheckEqualsString('tabela', vDataSet.getTableName);
    CheckFalse(vDataSet.Eof);

    CheckEqualsString('col1 row1', vDataSet.getField(0).AsString);
    CheckEqualsString('col2 row1', vDataSet.getField(1).AsString);
  finally
    vStream.Free;
  end;
end;

procedure TTestXmlDatasetReader.NoFieldDefinition;
const
  sInitialXml = '<root><tabela/></root>';
var
  vDataSet: IDataSetReadOnly;
begin
  ExpectedException := ENoFieldDefinition;
  vDataSet := TXmlDataSetBuilder.newFromText(sInitialXml).build;
end;

procedure TTestXmlDatasetReader.withDataSet;
const
  sInitialXml = '<root><tabela col1="col1 row1" col2="col2 row1"/></root>';
begin
  {$WARN SYMBOL_PLATFORM OFF}
  ExpectedException := EAbstractError;
  {$WARN SYMBOL_PLATFORM ON}

  TXmlDataSetBuilder.newFromText(sInitialXml).withDataSet(TAbstractDataSet).build;
end;

procedure TTestXmlDatasetReader.withValidator;
const
  sInitialXml = '<root><tabela col1="col1 row1" col2="col2 row1"/></root>';
var
  vValidator: IMockXmlValidator;
begin
  vValidator := TMockXmlValidator.Create;
  vValidator.setExpectationCalls(1);
  
  TXmlDataSetBuilder.newFromText(sInitialXml).withValidator(vValidator).build;

  vValidator.checkCalls;
end;

procedure TTestXmlDatasetReader.LoadFromFile_EmptyXML;
var
  vFile: TextFile;
  vFileName: String;
begin
  vFileName := ChangeFileExt(Application.ExeName, 'emptyfile');

  AssignFile(vFile, vFileName);
  try
    Rewrite(vFile);
  finally
    CloseFile(vFile);
  end;

  try
    ExpectedException := EEmptyXML;
    TXmlDataSetBuilder.newFromFile(vFileName, nil).build;
  finally
    Windows.DeleteFile(PChar(vFileName));
  end;
end;

procedure TTestXmlDatasetReader.LoadFromStream_EmptyXML;
var
  vStream: TStringStream;
begin
  ExpectedException := EEmptyXML;
  vStream := TStringStream.Create('');
  try
    TXmlDataSetBuilder.newFromStream(vStream).build;
  finally
    vStream.Free;
  end;

end;

procedure TTestXmlDatasetReader.LoadFromXmlText_EmptyXML;
begin
  ExpectedException := EEmptyXML;
  TXmlDataSetBuilder.newFromText('').build;
end;

procedure TTestXmlDatasetReader.InvalidXML_NodeNotClosedStartedTag;
begin
  ExpectedException := EInvalidXML;
  TXmlDataSetBuilder.newFromText('<root><root').build;
end;

procedure TTestXmlDatasetReader.InvalidXML_NodeNotClosedTag;
begin
  ExpectedException := EInvalidXML;
  TXmlDataSetBuilder.newFromText('<root><root>').build;
end;

initialization
  TTestXmlDatasetReader.RegisterTest('XmlDatasetReader');

end.
