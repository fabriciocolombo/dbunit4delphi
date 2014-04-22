unit Formatter;

interface

uses DB, SysUtils;

type
  TFormatter = class
    class function Format_Int(value: Integer): String;
    class function Format_Double(value: Double): String;
    class function Format_Date(value: TDateTime): String;
    class function Format_Datetime(value: TDateTime): String;

    class function Format_Field(value: TField; AFieldType: TFieldType = ftUnknown): String;
  end;

  TFormatterExport = class
  private
  public
    class function Format_Field(value: TField; AFieldType: TFieldType = ftUnknown): String;
  end;

implementation

{ TFormatter }

class function TFormatter.Format_Date(value: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd', value);
end;

class function TFormatter.Format_Datetime(value: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value);
end;

class function TFormatter.Format_Double(value: Double): String;
begin
  Result := StringReplace(FormatFloat('0.00', value), ',', '.', []);
end;

class function TFormatter.Format_Field(value: TField; AFieldType: TFieldType): String;
var
  vTypeToFormat: TFieldType;
begin
  if (value.IsNull) or (Trim(value.AsString) = EmptyStr) then
  begin
    Result := 'null'
  end
  else
  begin
    if (AFieldType <> ftUnknown) then
      vTypeToFormat := AFieldType
    else
      vTypeToFormat := value.DataType;

    case vTypeToFormat of
      ftInteger, ftWord, ftBCD, ftLargeint: Result := Format_Int(value.AsInteger);
      ftFloat, ftCurrency, ftFMTBcd: Result := Format_Double(value.AsFloat);
      ftDate, ftDateTime: Result := QuotedStr(Format_Date(value.AsDateTime));
    else
      Result := QuotedStr(value.AsString);
    end;
  end;
end;

class function TFormatter.Format_Int(value: Integer): String;
begin
  Result := IntToStr(value);
end;


{ TFormatterExport }

class function TFormatterExport.Format_Field(value: TField; AFieldType: TFieldType): String;
var
  vTypeToFormat: TFieldType;
begin
  if (value.IsNull) or (Trim(value.AsString) = EmptyStr) then
  begin
    Result := 'null'
  end
  else
  begin
    if (AFieldType <> ftUnknown) then
      vTypeToFormat := AFieldType
    else
      vTypeToFormat := value.DataType;

    case vTypeToFormat of
      ftDate, ftDateTime: Result := TFormatter.Format_Date(value.AsDateTime);
      ftTimeStamp: Result := TFormatter.Format_Datetime(value.AsDateTime);
    else
      Result := value.AsString;
    end;
  end;

end;

end.
