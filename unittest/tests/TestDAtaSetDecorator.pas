unit TestDataSetDecorator;

interface

uses TestCaseExtension,  DataSet, DataSetDecorator, DataSetIterator, DataSetListBuilder,
  XmlClientDataSet, XmlDatasetReader, Classes;

type
  TTestDataSetDecorator = class(TTestCaseExtension)
  private
  published
    procedure oneRow;
    procedure twoRows;
    procedure oneRowTwoDataSet;
    procedure twoRowsTwoDataSet;
  end;

implementation

uses Table, StubDataset;

{ TTestDataSetDecorator }

procedure TTestDataSetDecorator.oneRow;
var
  vDataSet: IDataSetReadOnly;
  vDecorator: IDataSetReadOnly;
begin
  vDataSet := TStubDataSet.newWithRecordCount('um', 1, '1');

  vDecorator := TXmlDataSetDecorator.Create(vDataSet);

  vDecorator.First;
  CheckFalse(vDecorator.Eof);
  vDecorator.Next;
  CheckTrue(vDecorator.Eof)
end;

procedure TTestDataSetDecorator.oneRowTwoDataSet;
var
  vDataSetOne,
  vDataSetTwo: IDataSet;
  vList: IInterfaceList;
  vDecorator: IDataSetReadOnly;
begin
  vDataSetOne := TStubDataSet.newWithRecordCount('um', 1, '1');
  vDataSetTwo := TStubDataSet.newWithRecordCount('dois', 1, '2');

  vList := TInterfaceList.Create;
  vList.Add(vDataSetOne);
  vList.Add(vDataSetTwo);

  vDecorator := TXmlDataSetDecorator.Create(TDataSetIterator.Create(vList) as IDataSetIterator);

  vDecorator.First;
  CheckFalse(vDecorator.Eof);
  CheckEqualsString('1', vDecorator.getField(0).AsString);
  vDecorator.Next;
  CheckFalse(vDecorator.Eof);
  CheckEqualsString('2', vDecorator.getField(0).AsString);
  vDecorator.Next;
  CheckTrue(vDecorator.Eof)
end;

procedure TTestDataSetDecorator.twoRows;
var
  vDataSet: IDataSet;
  vDecorator: IDataSetReadOnly;
begin
  vDataSet := TStubDataSet.newWithRecordCount('um', 2, '2');

  vDecorator := TXmlDataSetDecorator.Create(vDataSet);

  vDecorator.First;
  CheckFalse(vDecorator.Eof);
  vDecorator.Next;
  CheckFalse(vDecorator.Eof);
  vDecorator.Next;
  CheckTrue(vDecorator.Eof)
end;

procedure TTestDataSetDecorator.twoRowsTwoDataSet;
var
  vDataSetOne,
  vDataSetTwo: IDataSet;
  vList: IInterfaceList;
  vDecorator: IDataSetReadOnly;
begin
  vDataSetOne := TStubDataSet.newWithRecordCount('um', 2, '1');
  vDataSetTwo := TStubDataSet.newWithRecordCount('dois', 2, '2');

  vList := TInterfaceList.Create;
  vList.Add(vDataSetOne);
  vList.Add(vDataSetTwo);

  vDecorator := TXmlDataSetDecorator.Create(TDataSetIterator.Create(vList) as IDataSetIterator);

  vDecorator.First;
  CheckFalse(vDecorator.Eof);
  CheckEqualsString('1', vDecorator.getField(0).AsString);
  vDecorator.Next;
  CheckFalse(vDecorator.Eof);
  CheckEqualsString('1', vDecorator.getField(0).AsString);
  vDecorator.Next;
  CheckFalse(vDecorator.Eof);
  CheckEqualsString('2', vDecorator.getField(0).AsString);
  vDecorator.Next;
  CheckFalse(vDecorator.Eof);
  CheckEqualsString('2', vDecorator.getField(0).AsString);
  vDecorator.Next;
  CheckTrue(vDecorator.Eof)
end;

initialization
  TTestDataSetDecorator.RegisterTest('DataSetDecorator');

end.
