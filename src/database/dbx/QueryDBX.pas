unit QueryDBX;

interface

uses Query, SqlExpr, Classes, DB;

type
  TQueryDBX = class(TInterfacedObject, IQuery)
  private
    FQuery: TSQLQuery;
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

    constructor Create(ASqlConnection: TSQLConnection);
    destructor Destroy; override;

    property Fields[AIndex: Integer]: TField read GetFields;default;
    property FieldCount: Integer read GetFieldCount;

    function IsEmpty: Boolean;
  end;

implementation

{ TQueryDBX }

procedure TQueryDBX.Close;
begin
  FQuery.Close;
end;

constructor TQueryDBX.Create(ASqlConnection: TSQLConnection);
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.SQLConnection := ASqlConnection;
end;

destructor TQueryDBX.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  inherited;
end;

function TQueryDBX.Eof: Boolean;
begin
  Result := FQuery.Eof;
end;

function TQueryDBX.Execute: Integer;
begin
  Result := FQuery.ExecSQL();
end;

procedure TQueryDBX.First;
begin
  FQuery.First;
end;

function TQueryDBX.GetActive: Boolean;
begin
  Result := FQuery.Active;
end;

function TQueryDBX.GetFieldCount: Integer;
begin
  Result := FQuery.FieldCount;
end;

function TQueryDBX.GetFields(AIndex: Integer): TField;
begin
  Result := FQuery.Fields[AIndex]; 
end;

function TQueryDBX.GetSql: TStrings;
begin
  Result := FQuery.SQL;
end;

function TQueryDBX.IsEmpty: Boolean;
begin
  Result := FQuery.IsEmpty;
end;

procedure TQueryDBX.Next;
begin
  FQuery.Next;
end;

procedure TQueryDBX.Open;
begin
  FQuery.Open;
end;

function TQueryDBX.realQuery: TObject;
begin
  Result := FQuery;
end;

end.
