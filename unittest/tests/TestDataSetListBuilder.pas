unit TestDataSetListBuilder;

interface

uses TestCaseExtension,  DataSetListBuilder, DataSet;

type
  TTestDataSetListBuilder = class(TTestCaseExtension)
  private
  published
    procedure oneTable;
    procedure twoTable;
    procedure duplicateTable;
  end;

implementation

uses SysUtils, MockDataSet, TestFramework;

{ TTestDataSetListBuilder }

procedure TTestDataSetListBuilder.duplicateTable;
var
  vBuilder: TDataSetListBuilder;
  vDataSetOne,
  vDataSetTwo,
  vDataSetThree: IDataSet;
begin
  vBuilder := TDataSetListBuilder.Create(TMockDataSet);
  try
    vDataSetOne := vBuilder.AddTableName('one');
    vDataSetTwo := vBuilder.AddTableName('one');
    vDataSetThree := vBuilder.AddTableName('one');

    CheckNotNull(vDataSetOne);
    CheckEqualsString('one', vDataSetOne.getTableName);

    CheckNotNull(vDataSetTwo);
    CheckEqualsString('one', vDataSetTwo.getTableName);

    CheckSame(vDataSetTwo, vDataSetThree, 'two x three');
    CheckSame(vDataSetOne, vDataSetTwo, 'one x two');
    CheckSame(vDataSetOne, vDataSetThree, 'one x three');

    CheckNotNull(vBuilder.getList);
    CheckEquals(1, vBuilder.getList.Count);
  finally
    vBuilder.Free;
  end;
end;

procedure TTestDataSetListBuilder.oneTable;
var
  vBuilder: TDataSetListBuilder;
  vDataSet: IDataSet;
begin
  vBuilder := TDataSetListBuilder.Create(TMockDataSet);
  try
    vDataSet := vBuilder.AddTableName('one');

    CheckNotNull(vDataSet);
    CheckEqualsString('one', vDataSet.getTableName);
    CheckNotNull(vBuilder.getList);
    CheckEquals(1, vBuilder.getList.Count);
  finally
    vBuilder.Free;
  end;
end;

procedure TTestDataSetListBuilder.twoTable;
var
  vBuilder: TDataSetListBuilder;
  vDataSetOne,
  vDataSetTwo: IDataSet;
begin
  vBuilder := TDataSetListBuilder.Create(TMockDataSet);
  try
    vDataSetOne := vBuilder.AddTableName('one');
    vDataSetTwo := vBuilder.AddTableName('two');

    CheckNotNull(vDataSetOne);
    CheckEqualsString('one', vDataSetOne.getTableName);

    CheckNotNull(vDataSetTwo);
    CheckEqualsString('two', vDataSetTwo.getTableName);

    CheckNotNull(vBuilder.getList);
    CheckEquals(2, vBuilder.getList.Count);
  finally
    vBuilder.Free;
  end;
end;

initialization
  TTestDataSetListBuilder.RegisterTest('DataSetListBuilder');

end.
