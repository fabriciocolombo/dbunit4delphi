unit StubDataset;

interface

uses DataSet, XmlClientDataSet;

type
  TStubDataSet = class(TXmlClientDataSet)
  private
  public
    class function newWithRecordCount(ATableName: String; ARecordCount: Integer; ARecordValue: String): IDataSet;
  end;

implementation

uses Formatter;

{ TStubDataSet }

class function TStubDataSet.newWithRecordCount(ATableName: String; ARecordCount: Integer; ARecordValue: String): IDataSet;
begin
  Result := TStubDataSet.Create;
  Result.setTableName(ATableName); 
  Result.AddField('field0');
  Result.Build;

  while ARecordCount > 0 do
  begin
    Result.BeginInsert;
    Result.AddRow('field0', ARecordValue);
    Result.EndInsert;

    Dec(ARecordCount);
  end;
end;

end.






