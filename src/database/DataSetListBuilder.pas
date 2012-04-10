unit DataSetListBuilder;

interface

uses DataSet, DataSetDecorator, classes;

type
  TDataSetListBuilder = class
  private
    FDataSets: IInterfaceList;
    FDataSetClass: TAbstractDataSetClass;

    function find(ATableName: String): IDataSet;

    function createDataSet(ATableName: String): IDataSet;
  public
    constructor Create(ADataSetClass: TAbstractDataSetClass);

    function AddTableName(ATableName: String): IDataSet;

    function getList: IInterfaceList;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

uses MaskUtils, SysUtils, XmlClientDataSet;


{ TDataSetListBuilder }

function TDataSetListBuilder.AddTableName(ATableName: String): IDataSet;
var
  vIndex: Integer;
begin
  Result := find(ATableName);

  if not Assigned(Result) then
  begin
    Result := createDataSet(ATableName);

    vIndex := FDataSets.Add(Result);

    Result := FDataSets.Items[vIndex] as IDataSet;
  end;
end;

procedure TDataSetListBuilder.AfterConstruction;
begin
  inherited;
  FDataSets := TInterfaceList.Create;
end;

constructor TDataSetListBuilder.Create(ADataSetClass: TAbstractDataSetClass);
begin
  FDataSetClass := ADataSetClass;
end;

function TDataSetListBuilder.createDataSet(ATableName: String): IDataSet;
begin
  Result := FDataSetClass.Create;
  Result.setTableName(ATableName);
end;

destructor TDataSetListBuilder.Destroy;
begin
  FDataSets := nil;
  inherited;
end;

function TDataSetListBuilder.find(ATableName: String): IDataSet;
var
  i: Integer;
  vDataSet: IDataSet; 
begin
  Result := nil;
  for i := 0 to FDataSets.Count-1 do
  begin
    vDataSet := FDataSets.Items[i] as IDataSet;
    if SameText(vDataSet.getTableName, ATableName) then
    begin
      Result := vDataSet;
      Break;
    end;  
  end;
end;

function TDataSetListBuilder.getList: IInterfaceList;
begin
  Result := FDataSets;
end;

end.
