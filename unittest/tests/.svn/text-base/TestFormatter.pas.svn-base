unit TestFormatter;

interface

uses BaseTestCase, Formatter, DB, DBClient;

type
  TTestFormatter = class(TBaseTestCase)
  private
    function newField(AFieldType: TFieldType): TField;
  public
  published
    procedure intValue;
    procedure dateValue;
    procedure dateTime;
    procedure doubleValue_DecimalDot;
    procedure doubleValue_DecimalColon;
    procedure formatField_null;
    procedure formatField_empty;
    procedure formatField_unknown;
    procedure formatField_integer;
    procedure formatField_float;
    procedure formatField_string;
  end;

implementation

uses DateUtils, SysUtils, TestFramework, Windows, TestUtils;

{ TTestFormatter }

procedure TTestFormatter.dateTime;
var
  vDate: TDateTime;
begin
  vDate := EncodeDateTime(2011, 4, 3, 1, 2, 3, 4);

  CheckEquals('2011-04-03 01:02:03', TFormatter.Format_Datetime(vDate));
end;

procedure TTestFormatter.dateValue;
var
  vDate: TDateTime;
begin
  vDate := EncodeDateTime(2011, 4, 3, 1, 2, 3, 4);

  CheckEquals('2011-04-03', TFormatter.Format_Date(vDate));
end;

procedure TTestFormatter.doubleValue_DecimalColon;
var
  oldSeparator: Char;
begin
  TTestUtils.ChangeSystemDecimalSeparator(',', oldSeparator);
  try
    CheckEquals('1234.56', TFormatter.Format_Double(1234.56));
  finally
    TTestUtils.ChangeSystemDecimalSeparator(oldSeparator, oldSeparator);
  end;
end;

procedure TTestFormatter.doubleValue_DecimalDot;
var
  vOldSeparator: Char;
begin
  TTestUtils.ChangeSystemDecimalSeparator('.', vOldSeparator);
  try
    CheckEquals('1234.56', TFormatter.Format_Double(1234.56));
  finally
    TTestUtils.ChangeSystemDecimalSeparator(vOldSeparator, vOldSeparator);
  end;
end;

procedure TTestFormatter.formatField_empty;
var
  vField: TField;
begin
  vField := newField(ftString);
  try
    CheckEqualsString('null', TFormatter.Format_Field(vField, vField.DataType));
  finally
    vField.DataSet.Free;
  end;
end;

procedure TTestFormatter.formatField_float;
var
  vField: TField;
begin
  vField := newField(ftFloat);
  try
    vField.AsFloat := 1.23;

    CheckEqualsString('1.23', TFormatter.Format_Field(vField, vField.DataType));
  finally
    vField.DataSet.Free;
  end;
end;

procedure TTestFormatter.formatField_integer;
var
  vField: TField;
begin
  vField := newField(ftInteger);
  try
    vField.AsInteger := 1;

    CheckEqualsString('1', TFormatter.Format_Field(vField, vField.DataType));
  finally
    vField.DataSet.Free;
  end;
end;

procedure TTestFormatter.formatField_null;
var
  vField: TField;
begin
  vField := newField(ftInteger);
  try
    CheckEqualsString('null', TFormatter.Format_Field(vField, vField.DataType));
  finally
    vField.DataSet.Free;
  end;
end;

procedure TTestFormatter.formatField_string;
var
  vField: TField;
begin
  vField := newField(ftString);
  try
    vField.AsString := 'abc123';

    CheckEqualsString('''abc123''', TFormatter.Format_Field(vField, vField.DataType));
  finally
    vField.DataSet.Free;
  end;
end;

procedure TTestFormatter.formatField_unknown;
var
  vField: TField;
begin
  vField := newField(ftInteger);
  try
    vField.AsInteger := 1;

    CheckEqualsString('1', TFormatter.Format_Field(vField, ftUnknown));
  finally
    vField.DataSet.Free;
  end;
end;

procedure TTestFormatter.intValue;
begin
  CheckEquals('1', TFormatter.Format_Int(1));
  CheckEquals('123', TFormatter.Format_Int(123));
end;

function TTestFormatter.newField(AFieldType: TFieldType): TField;
var
  cds: TClientDataSet;
begin
  cds := TClientDataSet.Create(nil);
  Result := DefaultFieldClasses[AFieldType].Create(cds);
  Result.FieldName := 'testField';
  Result.DataSet := cds;

  cds.CreateDataSet;
  cds.Append;
end;

initialization
  TTestFormatter.RegisterTest('Formatter');

end.
