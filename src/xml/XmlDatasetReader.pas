unit XmlDatasetReader;

interface

uses xmldom, XMLIntf, msxmldom, XMLDoc, SysUtils, DataSet, Exceptions, XmlValidator,
     Classes, DatabaseConnection;

type
  TXmlReadingMode = (xmlFile, xmlText, xmlStream);

  IXmlDataSetBuilder = interface
  ['{A9BD93A1-5DA4-4F79-9BDA-D9D6ACFF9BB1}']
    function withValidator(const AValidator: IXmlValidator): IXmlDataSetBuilder;
    function withDataSet(ADataSetClass: TAbstractDataSetClass): IXmlDataSetBuilder;
    function usingConnection(const ADatabaseConnection: IDatabaseConnection): IXmlDataSetBuilder;

    function build: IDataSetReadOnly;
    function buildIterator: IDataSetIterator;
  end;

  //Default class TXmlClientDataSet
  TXmlDataSetBuilder = class(TInterfacedObject, IXmlDataSetBuilder)
  private
    FFileName: TFileName;
    FXmlText: String;
    FStream: TStream;
    FValidator: IXmlValidator;
    FDataSetClass: TAbstractDataSetClass;
    FReadingMode: TXmlReadingMode;
    FDatabaseConnection: IDatabaseConnection;

    constructor CreateFromFile(AFileName: TFileName);
    constructor CreateFromText(AXmlText: String);
    constructor CreateFromStream(AStream: TStream);
  public
    class function newFromFile(AFileName: TFileName): IXmlDataSetBuilder;
    class function newFromText(AXmlText: String): IXmlDataSetBuilder;
    class function newFromStream(AStream: TStream): IXmlDataSetBuilder;

    function withValidator(const AValidator: IXmlValidator): IXmlDataSetBuilder;
    function withDataSet(ADataSetClass: TAbstractDataSetClass): IXmlDataSetBuilder;
    function usingConnection(const ADatabaseConnection: IDatabaseConnection): IXmlDataSetBuilder;

    function build: IDataSetReadOnly;
    function buildIterator: IDataSetIterator;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  TXmlDatasetReader = class
  private
    FXml: IXMLDocument;
    FBuilder: TXmlDataSetBuilder;
    FDatabaseConnection: IDatabaseConnection;

    procedure MetadataUpdate(const ADataSet: IDataSet; const ARowNode: IXMLNode);

    procedure ApplyValidator;

    procedure LoadXML;
  public
    constructor Create(ABuilder: TXmlDataSetBuilder; ADatabaseConnection: IDatabaseConnection);

    function ReadToDataset: IDataSetIterator;

    destructor Destroy; override;
    class function NewInstance: TObject; override;
  end;

implementation

uses DataSetDecorator, DataSetListBuilder, XmlClientDataSet,
  DataSetIterator, XMLDomParseError, Data.DB;

{ TXmlDatasetReader }

procedure TXmlDatasetReader.ApplyValidator;
begin
  if (FBuilder.FValidator = nil) then Exit;

  FBuilder.FValidator.Validate(FXml);
end;

constructor TXmlDatasetReader.Create(ABuilder: TXmlDataSetBuilder; ADatabaseConnection: IDatabaseConnection);
begin
  FBuilder := ABuilder;
  FDatabaseConnection := ADatabaseConnection;
end;

destructor TXmlDatasetReader.Destroy;
begin
  FXml := nil;
  inherited;
end;

procedure TXmlDatasetReader.LoadXML;
begin
  try
    case FBuilder.FReadingMode of
      xmlFile: begin
                 if not FileExists(FBuilder.FFileName) then
                   raise Exceptions.EFileNotFoundException.Create(ClassName, FBuilder.FFileName);
                 FXml.LoadFromFile(FBuilder.FFileName);
               end;
      xmlText: begin
                 FXml.LoadFromXml(FBuilder.FXmlText);
               end;
      xmlStream: FXml.LoadFromStream(FBuilder.FStream);
    end;
  except
    on E: Exception do
    begin
      if (E is EDOMParseError) then
      begin
        with EDOMParseError(E) do
        begin
          if (errorCode = MSG_E_INVALIDATROOTLEVEL) then
            raise EEmptyXML.Create
          else
            raise EInvalidXML.CreateFmt('%s: ErrorCode: %d URL: %s Reason: %s SrcText: %s Line: %d LinePos: %d FilePos: %d',
                                        [ClassName, ErrorCode, URL, Reason, SrcText, Line, LinePos, FilePos]);
        end;
      end else
        raise;
    end;
  end;

  if not Assigned(FXml.DocumentElement) then
    raise EEmptyXML.Create;
end;

procedure TXmlDatasetReader.MetadataUpdate(const ADataSet: IDataSet; const ARowNode: IXMLNode);
var
  int_attr: Integer;
  vColumnNode: IXMLNode;
  vMetadata: TFieldListMetadata;
  vMetaField: TField;
  vFieldType: TFieldType;
  vFieldSize: Integer;
  vRequired: Boolean;
begin
  if not ADataSet.IsInitMetadata then
  begin
    vMetadata := nil;
    try
      if Assigned(FDatabaseConnection) then
      begin
        vMetadata := FDatabaseConnection.getFields(ADataSet.getTableName);
      end;

      for int_attr := 0 to ARowNode.AttributeNodes.Count-1 do
      begin
        vColumnNode := ARowNode.AttributeNodes.Get(int_attr);

        vFieldType := ftString;
        vFieldSize := 0;
        vRequired := False;

        if Assigned(vMetadata) then
        begin
          vMetaField := vMetadata.FindField(vColumnNode.NodeName);
          if Assigned(vMetaField) then
          begin
            vFieldType := vMetaField.DataType;
            vFieldSize := vMetaField.Size;
            vRequired  := vMetaField.Required;
          end;
        end;
        ADataSet.AddField(vColumnNode.NodeName, vFieldType, vRequired, vFieldSize);
      end;

      if ADataSet.getFieldCount = 0 then
        raise ENoFieldDefinition.Create(ClassName, ADataSet.getTableName);

      ADataSet.Build;
    finally
      vMetadata.Free;
    end;
  end;
end;

class function TXmlDatasetReader.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  
  TXmlDatasetReader(Result).FXml := TXMLDocument.Create(nil);
end;

function TXmlDatasetReader.ReadToDataset: IDataSetIterator;
var
  int_node,
  int_attr: Integer;
  vRoot,
  vRowNode,
  vColumnNode: IXMLNode;
  vDataSetBuilder: TDataSetListBuilder;
  vDataSet: IDataSet;
begin
  LoadXML;

  ApplyValidator;

  vRoot := FXml.DocumentElement;

  vDataSetBuilder := TDataSetListBuilder.Create(FBuilder.FDataSetClass);
  try
    for int_node := 0 to vRoot.ChildNodes.Count-1 do
    begin
      vRowNode := vRoot.ChildNodes.Get(int_node);

      if not (vRowNode.NodeType = ntElement) then Continue;

      //Create DataSet or Retrieve an existent
      vDataSet := vDataSetBuilder.AddTableName(vRowNode.NodeName);

      MetadataUpdate(vDataSet, vRowNode);

      vDataSet.BeginInsert;
      for int_attr := 0 to vRowNode.AttributeNodes.Count-1 do
      begin
        vColumnNode := vRowNode.AttributeNodes.Get(int_attr);

        vDataSet.AddRow(vColumnNode.NodeName, vColumnNode.NodeValue);
      end;
      vDataSet.EndInsert;
    end;

    Result := TDataSetIterator.Create(vDataSetBuilder.getList);
  finally
    vDataSetBuilder.Free;
  end;
end;

{ TXmlDataSetBuilder }

procedure TXmlDataSetBuilder.AfterConstruction;
begin
  inherited;
  FValidator := nil;
  FDataSetClass := TXmlClientDataSet;
end;

function TXmlDataSetBuilder.build: IDataSetReadOnly;
begin
  Result := TXmlDataSetDecorator.Create(buildIterator);
end;

function TXmlDataSetBuilder.buildIterator: IDataSetIterator;
var
  vReader: TXmlDatasetReader;
begin
  Assert(Assigned(FDataSetClass));

  vReader := TXmlDatasetReader.Create(Self, FDatabaseConnection);
  try
    Result := vReader.ReadToDataset;
  finally
    vReader.Free;
  end;
end;

constructor TXmlDataSetBuilder.CreateFromFile(AFileName: TFileName);
begin
  FFileName := AFileName;
  FReadingMode := xmlFile;
end;

constructor TXmlDataSetBuilder.CreateFromStream(AStream: TStream);
begin
  AStream.Position := 0;

  FStream := TMemoryStream.Create;
  FStream.CopyFrom(AStream, AStream.Size);

  FReadingMode := xmlStream;
end;

constructor TXmlDataSetBuilder.CreateFromText(AXmlText: String);
begin
  FXmlText := AXmlText;
  FReadingMode := xmlText;
end;

destructor TXmlDataSetBuilder.Destroy;
begin
  FStream.Free;
  inherited;
end;

class function TXmlDataSetBuilder.newFromFile(AFileName: TFileName): IXmlDataSetBuilder;
begin
  Result := TXmlDataSetBuilder.CreateFromFile(AFileName);
end;

class function TXmlDataSetBuilder.newFromStream(AStream: TStream): IXmlDataSetBuilder;
begin
  Result := TXmlDataSetBuilder.CreateFromStream(AStream);
end;

class function TXmlDataSetBuilder.newFromText(AXmlText: String): IXmlDataSetBuilder;
begin
  Result := TXmlDataSetBuilder.CreateFromText(AXmlText);
end;

function TXmlDataSetBuilder.usingConnection(const ADatabaseConnection: IDatabaseConnection): IXmlDataSetBuilder;
begin
  FDatabaseConnection := ADatabaseConnection;
  Result := Self;
end;

function TXmlDataSetBuilder.withDataSet(ADataSetClass: TAbstractDataSetClass): IXmlDataSetBuilder;
begin
  FDataSetClass := ADataSetClass;

  Result := Self;
end;

function TXmlDataSetBuilder.withValidator(const AValidator: IXmlValidator): IXmlDataSetBuilder;
begin
  FValidator := AValidator;

  Result := Self;
end;

initialization
  MSXML6_ProhibitDTD := False;

end.
