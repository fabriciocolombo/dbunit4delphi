unit TestDataSetIterator;

interface

uses BaseTestCase, DataSet, DataSetIterator, Classes;

type
  TTestDataSetIterator = class(TBaseTestCase)
  private
  public
  published
    procedure EmptyList;
    procedure NilList;
    procedure InvalidItem;
    procedure ValidList;
    procedure ValidListWithReset;
  end;

implementation

uses SysUtils, Exceptions, XmlClientDataSet;

{ TTestDataSetIterator }

procedure TTestDataSetIterator.EmptyList;
var
  vList: IInterfaceList;
  vIterator: IDataSetIterator;
begin
  vList := TInterfaceList.Create;

  vIterator := TDataSetIterator.Create(vList);

  CheckFalse(vIterator.HasNext);
  CheckNull(vIterator.Next);
end;

procedure TTestDataSetIterator.InvalidItem;
var
  vList: IInterfaceList;
begin
  vList := TInterfaceList.Create;
  vList.Add(TInterfaceList.Create);

  ExpectedException := EInvalidDataSetItem;
  TDataSetIterator.Create(vList);
end;

procedure TTestDataSetIterator.NilList;
begin
  ExpectedException := EAssertionFailed; 
  TDataSetIterator.Create(nil);
end;

procedure TTestDataSetIterator.ValidList;
var
  vList: IInterfaceList;
  vIterator: IDataSetIterator;
begin
  vList := TInterfaceList.Create;
  vList.Add(TXmlClientDataSet.Create);
  vList.Add(TXmlClientDataSet.Create);

  vIterator := TDataSetIterator.Create(vList);

  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckFalse(vIterator.HasNext);
end;

procedure TTestDataSetIterator.ValidListWithReset;
var
  vList: IInterfaceList;
  vIterator: IDataSetIterator;
begin
  vList := TInterfaceList.Create;
  vList.Add(TXmlClientDataSet.Create);
  vList.Add(TXmlClientDataSet.Create);

  vIterator := TDataSetIterator.Create(vList);

  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckFalse(vIterator.HasNext);

  vIterator.First;

  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckTrue(vIterator.HasNext);
  CheckNotNull(vIterator.Next);
  CheckFalse(vIterator.HasNext);  
end;

initialization
  TTestDataSetIterator.RegisterTest('DataSetIterator');

end.
