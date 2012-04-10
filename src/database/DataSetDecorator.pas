unit DataSetDecorator;

interface

uses DataSet, SysUtils, DB, DBClient, Classes;

type
  TXmlDataSetDecorator = class(TInterfacedObject, IDataSetReadOnly)
  private
    FIterator: IDataSetIterator;
    FDataSet: IDataSetReadOnly;
  public
    constructor Create(const AIterator: IDataSetIterator);overload;
    constructor Create(const ADataSet: IDataSetReadOnly);overload;

    destructor Destroy; override;

    function getTableName: String;
    procedure setTableName(ATableName: String);

    function getFieldCount: Integer;
    function getField(AIndex: Integer): TField;

    function getAllFields: String;

    procedure First;
    function Eof: Boolean;
    procedure Next;
  end;

implementation

uses DataSetIterator;

{ TXmlDataSetDecorator }

constructor TXmlDataSetDecorator.Create(const AIterator: IDataSetIterator);
begin
  FIterator := AIterator;
  First;
end;

constructor TXmlDataSetDecorator.Create(const ADataSet: IDataSetReadOnly);
var
  vList: IInterfaceList;
begin
  vList := TInterfaceList.Create;
  vList.Add(ADataSet);

  Create(TDataSetIterator.Create(vList) as IDataSetIterator); 
end;

destructor TXmlDataSetDecorator.Destroy;
begin
  FIterator := nil;
  FDataSet  := nil;
  inherited;
end;

procedure TXmlDataSetDecorator.First;
begin
  FIterator.First;
  FDataSet := FIterator.Next;
  FDataSet.First;
end;

procedure TXmlDataSetDecorator.Next;
begin
  if Assigned(FDataSet) and not FDataSet.Eof then
    FDataSet.Next
  else if FIterator.HasNext then
  begin
    FDataSet := FIterator.Next;
  end;
end;

function TXmlDataSetDecorator.Eof: Boolean;
begin
  Result := (Assigned(FDataSet) and FDataSet.Eof);

  if Result then
  begin
    if (FIterator.HasNext) then
    begin
      FDataSet := FIterator.Next;
      Result := False;
    end;
  end;
end;

function TXmlDataSetDecorator.getAllFields: String;
begin
  Result := FDataSet.getAllFields;
end;

function TXmlDataSetDecorator.getField(AIndex: Integer): TField;
begin
  Result := FDataSet.getField(AIndex);
end;

function TXmlDataSetDecorator.getFieldCount: Integer;
begin
  Result := FDataSet.getFieldCount;
end;

function TXmlDataSetDecorator.getTableName: String;
begin
  Result := FDataSet.getTableName;
end;

procedure TXmlDataSetDecorator.setTableName(ATableName: String);
begin
  FDataSet.setTableName(ATableName);
end;

end.
