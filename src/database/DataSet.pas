unit DataSet;

interface

uses
  Classes, DB, DBClient;

type
  IField = interface
  ['{E5E9A9BC-11DB-43FA-BF56-F3AF42A13268}']
    function getFieldName: String;
    function getFieldValue: String;
    function Required: Boolean;
  end;

  IDataSetReadOnly = interface
  ['{C338F6FB-7D7E-49F4-B158-706692F63296}']
    function getTableName: String;
    procedure setTableName(ATableName: String);

    function getFieldCount: Integer;
    function getField(AIndex: Integer): TField;

    function getAllFields: String;

    procedure First;
    function Eof: Boolean;
    procedure Next;
  end;

  IDataSet = interface(IDataSetReadOnly)
  ['{BC081D54-C0E4-49B8-875E-C10D4234D2AA}']
    function GetActive: Boolean;

    function IsInitMetadata: Boolean;

    function AddField(AFieldName: String; AFieldType: TFieldType = ftString; ARequired: Boolean=False; AFieldSize: Integer = 0): IDataSet;
    function Build: IDataSet;

    function BeginInsert: IDataSet;
    function EndInsert: IDataSet;
    function AddRow(AField, AValue: String): IDataSet;  
  end;

  IDataSetIterator = interface
  ['{5214593B-83FC-4676-B716-8C70CE2C35EB}']
    procedure First;
    function HasNext: Boolean;
    function Next: IDataSet;
  end;

  TAbstractDataSet = class(TInterfacedObject, IDataSet)
  private
  public
    function AddField(AFieldName: String;AFieldType: TFieldType = ftString;ARequired: Boolean = False; AFieldSize: Integer = 0): IDataSet;virtual;abstract;
    function AddRow(AField: String; AValue: String): IDataSet;virtual;abstract;
    function BeginInsert: IDataSet;virtual;abstract;
    function Build: IDataSet;virtual;abstract;
    function EndInsert: IDataSet;virtual;abstract;
    procedure First;virtual;abstract;
    function GetActive: Boolean;virtual;abstract;
    function getAllFields: String;virtual;abstract;
    function getField(AIndex: Integer): TField;virtual;abstract;
    function getFieldCount: Integer;virtual;abstract;
    function getTableName: String;virtual;abstract;
    function Eof: Boolean;virtual;abstract;
    function IsInitMetadata: Boolean;virtual;abstract;
    procedure Next;virtual;abstract;

    procedure setTableName(ATableName: String);virtual;abstract;

    constructor Create;virtual;
  end;

  TAbstractDataSetClass = class of TAbstractDataSet;

  TFieldListMetadata = class
  private
    FList: TStringList;
    function GetCount: Integer;
    function GetFields(AIndex: Integer): TField;
  public
    property Count: Integer read GetCount;
    property Fields[AIndex: Integer]: TField read GetFields;

    procedure AddField(AField: TField);overload;
    function AddField(AFieldName: String; AFieldType: TFieldType): TField;overload;
    procedure PopulateFromFieldList(AFieldList: TFields);

    function FindField(AFieldName: String): TField;
    function FieldByName(AFieldName: String): TField;

    function IsPrimaryKeyField(AField: TField): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Exceptions;

{ TAbstractDataSet }

constructor TAbstractDataSet.Create;
begin

end;

{ TFieldListMetadata }

procedure TFieldListMetadata.AddField(AField: TField);
begin
  FList.AddObject(AField.FieldName, AField);
end;

function TFieldListMetadata.AddField(AFieldName: String;AFieldType: TFieldType): TField;
begin
  Result := DefaultFieldClasses[AFieldType].Create(nil);
  Result.FieldName := AFieldName;

  AddField(Result); 
end;

constructor TFieldListMetadata.Create;
begin
  FList := TStringList.Create;
end;

destructor TFieldListMetadata.Destroy;
begin
  while FList.Count > 0 do
  begin
    FList.Objects[0].Free;
    FList.Delete(0);
  end;

  FList.Free;
  inherited;
end;

function TFieldListMetadata.FieldByName(AFieldName: String): TField;
begin
  Result := FindField(AFieldName);
  if not Assigned(Result) then
    raise EFieldMetadataNotFound.Create(AFieldName);
end;

function TFieldListMetadata.FindField(AFieldName: String): TField;
var
  vIndex: Integer;
begin
  vIndex := FList.IndexOf(AFieldName);

  if (vIndex >= 0) then
    Result := TField(FList.Objects[vIndex])
  else
    Result := nil;
end;

function TFieldListMetadata.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFieldListMetadata.GetFields(AIndex: Integer): TField;
begin
  Result := TField(FList.Objects[AIndex]);
end;

function TFieldListMetadata.IsPrimaryKeyField(AField: TField): Boolean;
begin
//TODO - substituir pfInKey quando a informação vier carregada corretamente
//  Result := (pfInKey in AField.ProviderFlags);
  Result := AField.Required;
end;

procedure TFieldListMetadata.PopulateFromFieldList(AFieldList: TFields);
var
  i: Integer;
  vSource, vTarget: TField;
begin
  for i := 0 to AFieldList.Count-1 do
  begin
    vSource := AFieldList[i];

    vTarget := DefaultFieldClasses[vSource.DataType].Create(nil);
    vTarget.FieldName := vSource.FieldName;
    vTarget.Required := vSource.Required;
    vTarget.Size := vSource.Size;

    AddField(vTarget);
  end;
end;

end.
