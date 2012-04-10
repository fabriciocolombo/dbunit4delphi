unit Query;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  DataSet, DB;

type
  IQuery = interface
  ['{09420981-0334-4253-A918-0093537C0ABD}']
    function GetSql: TStrings;
    function GetFields(AIndex: Integer): TField;
    function GetFieldCount: Integer;

    function Execute: Integer;

    function realQuery: TObject;

    function GetActive: Boolean;

    procedure Close;
    procedure Open;

    procedure First;
    procedure Next;

    function Eof: Boolean;
    function IsEmpty: Boolean;

    property Sql: TStrings read GetSql;
    property Fields[AIndex: Integer]: TField read GetFields;default;
    property FieldCount: Integer read GetFieldCount;
  end;


implementation


end.
