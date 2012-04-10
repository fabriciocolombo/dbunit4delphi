unit Statement;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, DB;

type
  IStatement = interface
  ['{8498ADFA-3D05-4DD8-BFEB-020E5EE68562}']
    function GetCommand: string;
    function GetParams: TParams;
    procedure SetParams(const Value: TParams);

    property Command: string read GetCommand;
    property Params: TParams read GetParams write SetParams;
  end;

  TStatement = class(TInterfacedObject, IStatement)
  private
    FCommand: string;
    FParams: TParams;
    function GetCommand: string;
    function GetParams: TParams;
    procedure SetParams(const Value: TParams);
  public
    property Command: string read GetCommand;
    property Params: TParams read GetParams write SetParams;

    constructor Create(command: String);
    destructor Destroy; override;
  end;


implementation



{ TStatement }

constructor TStatement.Create(command: String);
begin
  FCommand := command;
  FParams := TParams.Create;
end;

destructor TStatement.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TStatement.GetCommand: string;
begin
  Result := FCommand;
end;

function TStatement.GetParams: TParams;
begin
  Result := FParams;
end;

procedure TStatement.SetParams(const Value: TParams);
begin
  FParams.Assign(Value);
end;

end.
