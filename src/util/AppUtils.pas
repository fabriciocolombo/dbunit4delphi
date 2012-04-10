unit AppUtils;

interface

{$I ..\..\src\dbunit4delphi.inc}

type
  TAppUtils = class
  private
    class function IsCommandLineSwitch(AChar: Char): Boolean;
  public
    class function hasAppParam(AParamName: String; var AParamValue: String): Boolean;
  end;

implementation

uses SysUtils;

{ TAppUtils }

class function TAppUtils.hasAppParam(AParamName: String;var AParamValue: String): Boolean;
var
  I, vParamLength: Integer;
  CurrentParam: string;
begin
  vParamLength := Length(AParamName);

  Result := False;

  I := 1;
  while I <= ParamCount do
  begin
    CurrentParam := ParamStr(I);

    if (IsCommandLineSwitch(CurrentParam[1]) and SameText(Copy(CurrentParam, 2, vParamLength), AParamName)) then
    begin
      Result := True;

      I := I + 1;

      while I <= ParamCount do
      begin
        CurrentParam := ParamStr(I);

        if not IsCommandLineSwitch(CurrentParam[1]) then
          AParamValue := AParamValue + CurrentParam + sLineBreak
        else
          Break;

        Inc(I);
      end;

      Break;
    end;
    Inc(I);
  end;

  AParamValue := Trim(AParamValue);
end;

class function TAppUtils.IsCommandLineSwitch(AChar: Char): Boolean;
begin
  {$IFDEF D2010UP}
    Result := CharInSet(AChar, ['-','/','\']);
  {$ELSE}
    Result := AChar in ['-','/','\'];
  {$ENDIF}
end;

end.
