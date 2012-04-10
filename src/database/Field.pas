unit Field;

interface

uses DataSet;

type
  TFieldMetaData = class(TInterfacedObject, IField)
  private
    FFieldName: String;
    FFieldValue: String;
    FRequired: Boolean; 
  public
    function getFieldName: String;
    function getFieldValue: String;
    function Required: Boolean;

    class function newFrom(AFieldName, AFieldValue: String; ARequired: Boolean): TFieldMetaData;
  end;

implementation


{ TFieldMetaData }

function TFieldMetaData.getFieldName: String;
begin
  Result := FFieldName;
end;

function TFieldMetaData.getFieldValue: String;
begin
  Result := FFieldValue;
end;

class function TFieldMetaData.newFrom(AFieldName, AFieldValue: String;ARequired: Boolean): TFieldMetaData;
begin
  Result := TFieldMetaData.Create;
  Result.FFieldName := AFieldName;
  Result.FFieldValue := AFieldValue;
  Result.FRequired := ARequired;
end;

function TFieldMetaData.Required: Boolean;
begin
  Result := FRequired; 
end;

end.
