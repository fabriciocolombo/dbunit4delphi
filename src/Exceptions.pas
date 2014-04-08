unit Exceptions;

interface

uses SysUtils, DatabaseConnectionType;

type
  EBaseException = class(Exception);

  EInvalidDatabaseConnectionType = class(EBaseException)
  public
    constructor Create(AType: TDatabaseConnectionType);
  end;

  EInvalidDatabaseConfigClass = class(EBaseException)
  public
    constructor Create(AExpectedClass, AActualClass: String);
  end;

  EInvalidDatabaseConfiguration = class(EBaseException);

  EMetadataInitialized = class(EBaseException)
  public
    constructor Create(AOrigin: String);
  end;

  EMetadataNotInitialized = class(EBaseException)
  public
    constructor Create(AOrigin: String);
  end;

  EInsertModeException = class(EBaseException)
  public
    constructor Create(AOrigin: String);
  end;

  EBrowseModeException = class(EBaseException)
  public
    constructor Create(AOrigin: String);
  end;

  EFileNotFoundException = class(EBaseException)
  public
    constructor Create(AOrigin, AFileName: String);
  end;

  EInvalidDataSetItem = class(EBaseException)
  public
    constructor Create(AOrigin: String);
  end;

  ENoFieldDefinition = class(EBaseException)
  public
    constructor Create(AOrigin, ATableName: String);
  end;

  EFieldMetadataNotFound = class(EBaseException)
  public
    constructor Create(AFieldName: String);
  end;

  EEmptyXML = class(EBaseException)
  public
    constructor Create;
  end;

  EInvalidXML = class(EBaseException);

  EMockException = class(EBaseException);

  EExpectationNotSatisfied = class(EMockException)
  public
    constructor Create(AExpectedCalls, ARealCalls: Integer; AMethod: String);
  end;

  EMetadataError = class(EBaseException);

  ETableNotExists = class(EMetadataError)
  public
    constructor Create(ATableName: String);
  end;

implementation

uses TypInfo;

{ EInvalidDatabaseConnectionType }

constructor EInvalidDatabaseConnectionType.Create(AType: TDatabaseConnectionType);
begin
  inherited CreateFmt('Connection not implemented to type "%s".',[ClassName, GetEnumName(TypeInfo(TDatabaseConnectionType), Ord(AType))]);
end;

{ EInvalidDatabaseConfigClass }

constructor EInvalidDatabaseConfigClass.Create(AExpectedClass,AActualClass: String);
begin
  inherited CreateFmt('Invalid databaseconfig: expecting "%s" but was "%s".',[AExpectedClass, AActualClass]);
end;

{ EMetadataInitialized }

constructor EMetadataInitialized.Create(AOrigin: String);
begin
  inherited CreateFmt('%s: Metatada initialized!',[AOrigin]);
end;

{ EInsertModeException }

constructor EInsertModeException.Create(AOrigin: String);
begin
  inherited CreateFmt('%s: Dataset is in insert mode.',[AOrigin]);
end;

{ EBrowseModeException }

constructor EBrowseModeException.Create(AOrigin: String);
begin
  inherited CreateFmt('%s: Dataset is in browse mode.',[AOrigin]);
end;

{ EFileNotFoundException }

constructor EFileNotFoundException.Create(AOrigin, AFileName: String);
begin
  inherited CreateFmt('%s: File "%s" not found.',[AOrigin, AFileName]);
end;

{ EInvalidDataSetItem }

constructor EInvalidDataSetItem.Create(AOrigin: String);
begin
  inherited CreateFmt('%s: Datasets are accept only.',[AOrigin]);
end;

{ EMetadataNotInitialized }

constructor EMetadataNotInitialized.Create(AOrigin: String);
begin
  inherited CreateFmt('%s: Metatada not initialized!',[AOrigin]);
end;

{ ENoFieldDefinition }

constructor ENoFieldDefinition.Create(AOrigin, ATableName: String);
begin
  inherited CreateFmt('%s: No fields definition for table "%s".', [AOrigin, ATableName]);
end;

{ EFieldMetadataNotFound }

constructor EFieldMetadataNotFound.Create(AFieldName: String);
begin
  inherited CreateFmt('Field metadata not found for field "%s".', [AFieldName]);
end;

{ EEmptyXML }

constructor EEmptyXML.Create;
begin
  inherited Create('XML is empty');
end;

{ EExpectationNotSatisfied }

constructor EExpectationNotSatisfied.Create(AExpectedCalls, ARealCalls: Integer; AMethod: String);
begin
  inherited CreateFmt('Expecting %d calls to méthod "%s" but was %d.',[AExpectedCalls, AMethod,ARealCalls]);
end;

{ ETableNotExists }

constructor ETableNotExists.Create(ATableName: String);
begin
  inherited CreateFmt('Table "%s" not exists',[ATableName]);
end;

end.
