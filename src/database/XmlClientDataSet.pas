unit XmlClientDataSet;

interface

uses DataSet, SysUtils, DB, DBClient, Classes, Exceptions;

type

(*

  - Cada elemento do xml corresponde à uma tupla da tabela. O nome do elemento
    corresponde ao nome da tabela e cada atributo corresponde aos campos da tabela.
  - Para inserir campos nulos, basta omitir os atributos
  - O metadata da tabela será definido baseada na primeira tupla da tabela ou pode
    ser criado um DTD, que será usado como definição do dados.

  O DBUnit possui uma funcionalidade chamada ColumnSensing, que vai adicionando
  as colunas conforme vão sendo usada dentro do xml, por exemplo, caso o primeiro
  registro não possua determinada coluna, ela não fará parte do metadata, mais
  se a coluna existir no segundo registro, o metadata é atualizado.


  Possível implementação:
    - Criar uma lista onde vão sendo adicionados os DataSets, um para cada tabela.
    - A cada linha, se o dataset não existir, cria o mesmo com a definição das colunas,
      senão, apenas insere o registro no dataset.
    - Após finalizado, ordena a lista de dataset de acordo com a dependência dos dados,
      que pode ser definida de alguma forma
    - Prossegue com a inserção
*)

  TXmlClientDataSet = class(TAbstractDataSet, IDataSet)
  private
    FDataSet: TClientDataSet;
    FTableName: String;
    FMetadataIsInit: Boolean;
    FInsertMode: Boolean;
    FAllFields: String;

    procedure CheckBrowseMode;
    procedure CheckMetadata;
  public
    constructor Create;override;
    destructor Destroy; override;

    function AddField(AFieldName: String; AFieldType: TFieldType = ftString; ARequired: Boolean=False): IDataSet;override;
    function Build: IDataSet;override;

    function GetActive: Boolean;override;

    function getField(AIndex: Integer): TField;override;
    function getFieldCount: Integer;override;
    function getTableName: String;override;

    procedure First;override;
    function Eof: Boolean;override;
    procedure Next;override;

    function BeginInsert: IDataSet;override;
    function EndInsert: IDataSet;override;
    function AddRow(AField: String; AValue: String): IDataSet;override;

    function IsInitMetadata: Boolean;override;

    function getAllFields: String;override;

    procedure setTableName(ATableName: String);override;
   end;

implementation

uses Field, StrUtils;

{ TXmlClientDataSet }

function TXmlClientDataSet.AddField(AFieldName: String; AFieldType: TFieldType; ARequired: Boolean  ): IDataSet;
begin
  with DefaultFieldClasses[AFieldType].Create(FDataSet) do
  begin
    FieldName := AFieldName;
    FieldKind := fkData;
    Required  := ARequired;
    DataSet   := FDataSet;
  end;

  FAllFields := FAllFields + IfThen(FAllFields <> EmptyStr, ';') + AFieldName;

  Result := Self;
end;

function TXmlClientDataSet.AddRow(AField, AValue: String): IDataSet;
begin
  CheckBrowseMode;

  FDataSet.FieldByName(AField).AsString := AValue;

  Result := Self;
end;

function TXmlClientDataSet.BeginInsert: IDataSet;
begin
  CheckMetadata;

  if FInsertMode then
    raise EInsertModeException.Create(ClassName);

  FDataSet.Append;

  FInsertMode := True;

  Result := Self;
end;

function TXmlClientDataSet.Build: IDataSet;
begin
  if FMetadataIsInit then
    raise EMetadataInitialized.Create(ClassName);

  if FDataSet.FieldCount = 0 then
    raise ENoFieldDefinition.Create(ClassName, FTableName);

  FDataSet.CreateDataSet;

  FMetadataIsInit := True;
  
  Result := Self;
end;

procedure TXmlClientDataSet.CheckBrowseMode;
begin
  if not FInsertMode then
    raise EBrowseModeException.Create(ClassName);
end;

procedure TXmlClientDataSet.CheckMetadata;
begin
  if not FMetadataIsInit then
    raise EMetadataNotInitialized.Create(ClassName);
end;

constructor TXmlClientDataSet.Create;
begin
  FDataSet := TClientDataSet.Create(nil);
end;

destructor TXmlClientDataSet.Destroy;
begin
  FDataSet.Free;
  inherited;
end;

function TXmlClientDataSet.EndInsert: IDataSet;
begin
  CheckBrowseMode;

  FDataSet.Post;

  FInsertMode := False;

  Result := Self;
end;

function TXmlClientDataSet.GetActive: Boolean;
begin
  Result := FDataSet.Active;
end;

function TXmlClientDataSet.getAllFields: String;
begin
  Result := FAllFields;
end;

function TXmlClientDataSet.getField(AIndex: Integer): TField;
begin
  Result := FDataSet.Fields[AIndex];
end;

function TXmlClientDataSet.getFieldCount: Integer;
begin
  Result := FDataSet.FieldCount;
end;

function TXmlClientDataSet.getTableName: String;
begin
  Result := FTableName;
end;

procedure TXmlClientDataSet.First;
begin
  if FDataSet.Active then
    FDataSet.First;
end;

function TXmlClientDataSet.Eof: Boolean;
begin
  Result := FDataSet.Eof;
end;

procedure TXmlClientDataSet.Next;
begin
  if FDataSet.Active then
    FDataSet.Next;
end;

function TXmlClientDataSet.IsInitMetadata: Boolean;
begin
  Result := FMetadataIsInit;
end;

procedure TXmlClientDataSet.setTableName(ATableName: String);
begin
  FTableName := ATableName;
end;

end.
