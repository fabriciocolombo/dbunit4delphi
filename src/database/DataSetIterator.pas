unit DataSetIterator;

interface

uses DataSet, Classes, COntnrs;

type
  TDataSetIterator = class(TInterfacedObject, IDataSetIterator)
  private
    FList: IInterfaceList;
    FCurrentIndex: Integer;

    procedure CheckListContents;
  public
    procedure First;
    function HasNext: Boolean;
    function Next: IDataSet;

    constructor Create(const ADataSetList: IInterfaceList);
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Exceptions;


{ TDataSetIterator }

procedure TDataSetIterator.CheckListContents;
var
  i: Integer;
begin
  for i := 0 to FList.Count-1 do
  begin
    if not Supports(FList[i], IDataSet) then
      raise EInvalidDataSetItem.Create(ClassName); 
  end;
end;

constructor TDataSetIterator.Create(const ADataSetList: IInterfaceList);
begin
  Assert(Assigned(ADataSetList), 'Not assigned list');
  
  FList := ADataSetList;

  CheckListContents;
end;

destructor TDataSetIterator.Destroy;
begin
  FList := nil;
  inherited;
end;

procedure TDataSetIterator.First;
begin
  FCurrentIndex := 0;
end;

function TDataSetIterator.HasNext: Boolean;
begin
  Result := (FList.Count > 0) and (FCurrentIndex in [0..FList.Count-1]);
end;

function TDataSetIterator.Next: IDataSet;
begin
  Result := nil;

  //Validation to prevent invalid call
  if HasNext then
  begin
    Result := (FList[FCurrentIndex] as IDataSet);
    Result.First;
    Inc(FCurrentIndex);
  end;
end;

end.
