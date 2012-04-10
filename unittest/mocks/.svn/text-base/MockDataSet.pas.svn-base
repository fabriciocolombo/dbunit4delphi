unit MockDataSet;

interface

uses DataSet;

type
  TMockDataSet = class(TAbstractDataSet, IDataSet)
  private
    FTableName: String;
  public
    function getTableName: String; override;
    procedure setTableName(ATableName: String);override;
  end;

implementation

{ TMockDataSet }

function TMockDataSet.getTableName: String;
begin
  Result := FTableName;
end;

procedure TMockDataSet.setTableName(ATableName: String);
begin
  FTableName := ATableName;
end;

end.
