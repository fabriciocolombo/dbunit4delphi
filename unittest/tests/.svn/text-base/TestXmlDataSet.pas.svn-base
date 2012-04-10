unit TestXmlDataSet;

interface

uses BaseTestCase, XmlClientDataSet, DataSet, DB;

type
  TTestXmlDataSet = class(TBaseTestCase)
  private
    FDataSet: IDataSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure addField;
    procedure addFieldRequired;
    procedure addRowBrowseMode;
    procedure addRow;
    procedure navigation;
    procedure InsertModeException;
    procedure MetadataInitialized;
    procedure MetadataNotInitialized;
    procedure NoFieldDefinition;
    procedure getAllFields;
  end;

implementation

uses TypInfo, SysUtils, Exceptions, TestFramework;

{ TTestXmlDataSet }

procedure TTestXmlDataSet.addField;
begin
  CheckEquals(0, FDataSet.getFieldCount);
  FDataSet.AddField('teste');
  CheckEquals(1, FDataSet.getFieldCount);
end;

procedure TTestXmlDataSet.addFieldRequired;
begin
  FDataSet.AddField('teste', ftString, True);
  CheckTrue(FDataSet.getField(0).Required);
end;

procedure TTestXmlDataSet.addRow;
begin
  FDataSet.AddField('teste').Build.BeginInsert.AddRow('teste', 'um').EndInsert;

  CheckEquals(1, FDataSet.getFieldCount);
  CheckEquals('um', FDataSet.getField(0).AsString);
end;

procedure TTestXmlDataSet.addRowBrowseMode;
begin
  ExpectedException := EBrowseModeException;
  FDataSet.AddField('teste').Build.AddRow('teste', 'um');
end;

procedure TTestXmlDataSet.InsertModeException;
begin
  ExpectedException := EInsertModeException;
  FDataSet.AddField('teste').Build.BeginInsert.BeginInsert;
end;

procedure TTestXmlDataSet.MetadataInitialized;
begin
  ExpectedException := EMetadataInitialized;
  FDataSet.AddField('teste').Build.Build;
end;

procedure TTestXmlDataSet.MetadataNotInitialized;
begin
  ExpectedException := EMetadataNotInitialized;
  FDataSet.AddField('teste').BeginInsert;
end;

procedure TTestXmlDataSet.NoFieldDefinition;
begin
  ExpectedException := ENoFieldDefinition;
  FDataSet.Build;
end;

procedure TTestXmlDataSet.getAllFields;
var
  vAllFields: String;
begin
  vAllFields := FDataSet.AddField('testeum').AddField('testedois').Build.getAllFields;
  CheckEquals('testeum;testedois', vAllFields);
end;

procedure TTestXmlDataSet.navigation;
begin
  FDataSet.AddField('teste').Build.BeginInsert.AddRow('teste', 'um').EndInsert;

  FDataSet.First;
  CheckFalse(FDataSet.Eof);
  FDataSet.Next;
  CheckTrue(FDataSet.Eof);
  FDataSet.First;
  CheckFalse(FDataSet.Eof);
end;

procedure TTestXmlDataSet.SetUp;
begin
  inherited;
  FDataSet := TXmlClientDataSet.Create;
end;

procedure TTestXmlDataSet.TearDown;
begin
  FDataSet := nil;
  inherited;
end;

initialization
  TTestXmlDataSet.RegisterTest('XmlDataSet');

end.
