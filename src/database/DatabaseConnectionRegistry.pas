unit DatabaseConnectionRegistry;

interface

uses
  System.Generics.Collections, DatabaseConnectionType;

type
  TDatabaseConnectionRegistry = class
  strict private
    class var REGISTRY: TDictionary<TDatabaseConnectionType,TClass>;

    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterConnection(const AType: TDatabaseConnectionType; AImplementation: TClass);
    class function Resolve(const AType: TDatabaseConnectionType): TClass;
  end;

implementation

uses
  SysUtils, TypInfo;

{ TDatabaseConnectionRegistry }

class constructor TDatabaseConnectionRegistry.Create;
begin
  TDatabaseConnectionRegistry.REGISTRY := TDictionary<TDatabaseConnectionType,TClass>.Create;
end;

class destructor TDatabaseConnectionRegistry.Destroy;
begin
  TDatabaseConnectionRegistry.REGISTRY.Free;
end;

class procedure TDatabaseConnectionRegistry.RegisterConnection(const AType: TDatabaseConnectionType; AImplementation: TClass);
begin
  TDatabaseConnectionRegistry.REGISTRY.Add(AType, AImplementation);
end;

class function TDatabaseConnectionRegistry.Resolve(const AType: TDatabaseConnectionType): TClass;
begin
  if not TDatabaseConnectionRegistry.REGISTRY.TryGetValue(AType, Result) then
  begin
    raise Exception.CreateFmt('Database Connection not registered for type "%s".',
                              [GetEnumName(TypeInfo(TDatabaseConnectionType), Ord(AType))]);
  end;
end;

end.
