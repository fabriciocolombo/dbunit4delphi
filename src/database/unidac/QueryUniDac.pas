unit QueryUniDac;

interface

uses Query, Classes, DB, Uni;

type
  TQueryUniDac = class(TInterfacedObject, IQuery)
  private
    FQuery: TUniQuery;
  protected
    function GetSql: TStrings;
    function GetFields(AIndex: Integer): TField;
    function GetFieldCount: Integer;
  public
    function Execute: Integer;

    function realQuery: TObject;

    function GetActive: Boolean;

    procedure Close;
    procedure Open;

    function Eof: Boolean;

    procedure First;
    procedure Next;

    property Sql: TStrings read GetSql;

    constructor Create(AConnection: TUniConnection);
    destructor Destroy; override;

    property Fields[AIndex: Integer]: TField read GetFields;default;
    property FieldCount: Integer read GetFieldCount;

    function IsEmpty: Boolean;
  end;

implementation

{ TQueryUniDac }

procedure TQueryUniDac.Close;
begin
  FQuery.Close;
end;

constructor TQueryUniDac.Create(AConnection: TUniConnection);
begin
  FQuery := TUniQuery.Create(nil);
  FQuery.Connection := AConnection;
end;

destructor TQueryUniDac.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  inherited;
end;

function TQueryUniDac.Eof: Boolean;
begin
  Result := FQuery.Eof;
end;

function TQueryUniDac.Execute: Integer;
begin
  FQuery.ExecSQL();

  Result := FQuery.RowsAffected;
end;

procedure TQueryUniDac.First;
begin
  FQuery.First;
end;

function TQueryUniDac.GetActive: Boolean;
begin
  Result := FQuery.Active;
end;

function TQueryUniDac.GetFieldCount: Integer;
begin
  Result := FQuery.FieldCount;
end;

function TQueryUniDac.GetFields(AIndex: Integer): TField;
begin
  Result := FQuery.Fields[AIndex];
end;

function TQueryUniDac.GetSql: TStrings;
begin
  Result := FQuery.SQL;
end;

function TQueryUniDac.IsEmpty: Boolean;
begin
  Result := FQuery.IsEmpty;
end;

procedure TQueryUniDac.Next;
begin
  FQuery.Next;
end;

procedure TQueryUniDac.Open;
begin
  FQuery.Open;
end;

function TQueryUniDac.realQuery: TObject;
begin
  Result := FQuery;
end;

end.
