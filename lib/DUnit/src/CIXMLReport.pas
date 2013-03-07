
{*************************************************************************}
{                                                                         }
{                            XML Data Binding                             }
{                                                                         }
{         Generated on: 11/29/2005 4:14:24 PM                             }
{       Generated from: F:\src\LandoltSandbox\DunitTest\CIXMLReport.dtd   }
{   Settings stored in: F:\src\LandoltSandbox\DunitTest\CIXMLReport.xdb   }
{                                                                         }
{*************************************************************************}

unit CIXMLReport;

interface

uses xmldom, XMLDoc, XMLIntf;

const
    C_TESTSUITES     = 'testsuites'; (* the testsuites element for the aggregate document *)
    C_TESTSUITE      = 'testsuite';  (* the testsuite element *)
    C_TESTCASE       = 'testcase';   (* the testcase element *)
    C_ERROR          = 'error';      (* the error element *)
    C_FAILURE        = 'failure';    (* the failure element *)
    C_PROPERTIES     = 'properties'; (* the properties element *)
    C_PROPERTY       = 'property';   (* the property element *)
    C_SYSTEM_ERR     = 'system-err'; (* the system-err element *)
    C_SYSTEM_OUT     = 'system-out'; (* the system-out element *)
    C_ATTR_NAME      = 'name';       (* name attribute for property, testcase and testsuite elements *)
    C_ATTR_VALUE     = 'value';      (* name attribute for property *)
    C_ATTR_TIME      = 'time';       (* time attribute for testcase and testsuite elements *)
    C_ATTR_ERRORS    = 'errors';     (* errors attribute for testsuite elements *)
    C_ATTR_FAILURES  = 'failures';   (* failures attribute for testsuite elements *)
    C_ATTR_TESTS     = 'tests';      (* tests attribute for testsuite elements *)
    C_ATTR_MESSAGE   = 'message';    (* message attribute for failure elements *)
    C_ATTR_TYPE      = 'type';       (* type attribute for failure elements *)

type

{ Forward Decls }

  IXMLTestsuiteType = interface;
  IXMLPropertiesType = interface;
  IXMLPropertyType = interface;
  IXMLTestcaseType = interface;
  IXMLTestcaseTypeList = interface;
  IXMLFailureType = interface;
  IXMLErrorType = interface;

{ IXMLTestsuiteType }

  IXMLTestsuiteType = interface(IXMLNode)
    ['{6C840BEA-16A9-47DD-BD2E-A883503001C3}']
    { Property Accessors }
    function Get_Errors: string;
    function Get_Failures: string;
    function Get_Name: string;
    function Get_Tests: string;
    function Get_Time: string;
    function Get_Properties: IXMLPropertiesType;
    function Get_Testcase: IXMLTestcaseTypeList;
    function Get_Systemout: WideString;
    function Get_Systemerr: WideString;
    procedure Set_Errors(Value: string);
    procedure Set_Failures(Value: string);
    procedure Set_Name(Value: string);
    procedure Set_Tests(Value: string);
    procedure Set_Time(Value: string);
    procedure Set_Systemout(Value: WideString);
    procedure Set_Systemerr(Value: WideString);
    { Methods & Properties }
    property Errors: string read Get_Errors write Set_Errors;
    property Failures: string read Get_Failures write Set_Failures;
    property Name: string read Get_Name write Set_Name;
    property Tests: string read Get_Tests write Set_Tests;
    property Time: string read Get_Time write Set_Time;
    property Properties: IXMLPropertiesType read Get_Properties;
    property Testcase: IXMLTestcaseTypeList read Get_Testcase;
    property Systemout: WideString read Get_Systemout write Set_Systemout;
    property Systemerr: WideString read Get_Systemerr write Set_Systemerr;
  end;

{ IXMLPropertiesType }

  IXMLPropertiesType = interface(IXMLNodeCollection)
    ['{4A78F110-3759-4838-821C-8226343072EC}']
    { Property Accessors }
    function Get_Property_(Index: Integer): IXMLPropertyType;
    { Methods & Properties }
    function Add: IXMLPropertyType;
    function Insert(const Index: Integer): IXMLPropertyType;
    property Property_[Index: Integer]: IXMLPropertyType read Get_Property_; default;
  end;

{ IXMLPropertyType }

  IXMLPropertyType = interface(IXMLNode)
    ['{73F2575F-643F-46AA-AE81-F64F1F0D74B2}']
    { Property Accessors }
    function Get_Name: string;
    function Get_Value: WideString;
    procedure Set_Name(Value: string);
    procedure Set_Value(Value: WideString);
    { Methods & Properties }
    property Name: string read Get_Name write Set_Name;
    property Value: WideString read Get_Value write Set_Value;
  end;

{ IXMLTestcaseType }

  IXMLTestcaseType = interface(IXMLNode)
    ['{A149138A-7F67-4321-BDD5-BCD171155BA7}']
    { Property Accessors }
    function Get_Name: string;
    function Get_Time: string;
    function Get_Failure: IXMLFailureType;
    function Get_Error: IXMLErrorType;
    procedure Set_Name(Value: string);
    procedure Set_Time(Value: string);
    { Methods & Properties }
    property Name: string read Get_Name write Set_Name;
    property Time: string read Get_Time write Set_Time;
    property Failure: IXMLFailureType read Get_Failure;
    property Error: IXMLErrorType read Get_Error;
  end;

{ IXMLTestcaseTypeList }

  IXMLTestcaseTypeList = interface(IXMLNodeCollection)
    ['{162912F7-703D-4A16-9BB4-6C1788E9FBF5}']
    { Methods & Properties }
    function Add: IXMLTestcaseType;
    function Insert(const Index: Integer): IXMLTestcaseType;
    function Get_Item(Index: Integer): IXMLTestcaseType;
    property Items[Index: Integer]: IXMLTestcaseType read Get_Item; default;
  end;

{ IXMLFailureType }

  IXMLFailureType = interface(IXMLNode)
    ['{11E079C7-067A-41C0-AFEA-1D27C1D5F895}']
    { Property Accessors }
    function Get_Message: WideString;
    function Get_Type_: string;
    procedure Set_Message(Value: WideString);
    procedure Set_Type_(Value: string);
    { Methods & Properties }
    property Message: WideString read Get_Message write Set_Message;
    property Type_: string read Get_Type_ write Set_Type_;
  end;

{ IXMLErrorType }

  IXMLErrorType = interface(IXMLNode)
    ['{29109002-5443-4BD2-9B70-CA7AB293C77F}']
    { Property Accessors }
    function Get_Message: WideString;
    function Get_Type_: string;
    procedure Set_Message(Value: WideString);
    procedure Set_Type_(Value: string);
    { Methods & Properties }
    property Message: WideString read Get_Message write Set_Message;
    property Type_: string read Get_Type_ write Set_Type_;
  end;

{ Forward Decls }

  TXMLTestsuiteType = class;
  TXMLPropertiesType = class;
  TXMLPropertyType = class;
  TXMLTestcaseType = class;
  TXMLTestcaseTypeList = class;
  TXMLFailureType = class;
  TXMLErrorType = class;

{ TXMLTestsuiteType }

  TXMLTestsuiteType = class(TXMLNode, IXMLTestsuiteType)
  private
    FTestcase: IXMLTestcaseTypeList;
  protected
    { IXMLTestsuiteType }
    function Get_Errors: string;
    function Get_Failures: string;
    function Get_Name: string;
    function Get_Tests: string;
    function Get_Time: string;
    function Get_Properties: IXMLPropertiesType;
    function Get_Testcase: IXMLTestcaseTypeList;
    function Get_Systemout: WideString;
    function Get_Systemerr: WideString;
    procedure Set_Errors(Value: string);
    procedure Set_Failures(Value: string);
    procedure Set_Name(Value: string);
    procedure Set_Tests(Value: string);
    procedure Set_Time(Value: string);
    procedure Set_Systemout(Value: WideString);
    procedure Set_Systemerr(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPropertiesType }

  TXMLPropertiesType = class(TXMLNodeCollection, IXMLPropertiesType)
  protected
    { IXMLPropertiesType }
    function Get_Property_(Index: Integer): IXMLPropertyType;
    function Add: IXMLPropertyType;
    function Insert(const Index: Integer): IXMLPropertyType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPropertyType }

  TXMLPropertyType = class(TXMLNode, IXMLPropertyType)
  protected
    { IXMLPropertyType }
    function Get_Name: string;
    function Get_Value: WideString;
    procedure Set_Name(Value: string);
    procedure Set_Value(Value: WideString);
  end;

{ TXMLTestcaseType }

  TXMLTestcaseType = class(TXMLNode, IXMLTestcaseType)
  protected
    { IXMLTestcaseType }
    function Get_Name: string;
    function Get_Time: string;
    function Get_Failure: IXMLFailureType;
    function Get_Error: IXMLErrorType;
    procedure Set_Name(Value: string);
    procedure Set_Time(Value: string);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTestcaseTypeList }

  TXMLTestcaseTypeList = class(TXMLNodeCollection, IXMLTestcaseTypeList)
  protected
    { IXMLTestcaseTypeList }
    function Add: IXMLTestcaseType;
    function Insert(const Index: Integer): IXMLTestcaseType;
    function Get_Item(Index: Integer): IXMLTestcaseType;
  end;

{ TXMLFailureType }

  TXMLFailureType = class(TXMLNode, IXMLFailureType)
  protected
    { IXMLFailureType }
    function Get_Message: WideString;
    function Get_Type_: string;
    procedure Set_Message(Value: WideString);
    procedure Set_Type_(Value: string);
  end;

{ TXMLErrorType }

  TXMLErrorType = class(TXMLNode, IXMLErrorType)
  protected
    { IXMLErrorType }
    function Get_Message: WideString;
    function Get_Type_: string;
    procedure Set_Message(Value: WideString);
    procedure Set_Type_(Value: string);
  end;

{ Global Functions }

function Gettestsuite(Doc: IXMLDocument): IXMLTestsuiteType;
function Loadtestsuite(const FileName: WideString): IXMLTestsuiteType;
function Newtestsuite: IXMLTestsuiteType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function Gettestsuite(Doc: IXMLDocument): IXMLTestsuiteType;
begin
  Result := Doc.GetDocBinding(C_TESTSUITE, TXMLTestsuiteType, TargetNamespace) as IXMLTestsuiteType;
end;

function Loadtestsuite(const FileName: WideString): IXMLTestsuiteType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding(C_TESTSUITE, TXMLTestsuiteType, TargetNamespace) as IXMLTestsuiteType;
end;

function Newtestsuite: IXMLTestsuiteType;
begin
  Result := NewXMLDocument.GetDocBinding(C_TESTSUITE, TXMLTestsuiteType, TargetNamespace) as IXMLTestsuiteType;
end;

{ TXMLTestsuiteType }

procedure TXMLTestsuiteType.AfterConstruction;
begin
  RegisterChildNode(C_PROPERTIES, TXMLPropertiesType);
  RegisterChildNode(C_TESTCASE, TXMLTestcaseType);
  FTestcase := CreateCollection(TXMLTestcaseTypeList, IXMLTestcaseType, C_TESTCASE) as IXMLTestcaseTypeList;
  inherited;
end;

function TXMLTestsuiteType.Get_Errors: string;
begin
  Result := AttributeNodes[C_ATTR_ERRORS].Text;
end;

procedure TXMLTestsuiteType.Set_Errors(Value: string);
begin
  SetAttribute(C_ATTR_ERRORS, Value);
end;

function TXMLTestsuiteType.Get_Failures: string;
begin
  Result := AttributeNodes[C_ATTR_FAILURES].Text;
end;

procedure TXMLTestsuiteType.Set_Failures(Value: string);
begin
  SetAttribute(C_ATTR_FAILURES, Value);
end;

function TXMLTestsuiteType.Get_Name: string;
begin
  Result := AttributeNodes[C_ATTR_NAME].Text;
end;

procedure TXMLTestsuiteType.Set_Name(Value: string);
begin
  SetAttribute(C_ATTR_NAME, Value);
end;

function TXMLTestsuiteType.Get_Tests: string;
begin
  Result := AttributeNodes[C_ATTR_TESTS].Text;
end;

procedure TXMLTestsuiteType.Set_Tests(Value: string);
begin
  SetAttribute(C_ATTR_TESTS, Value);
end;

function TXMLTestsuiteType.Get_Time: string;
begin
  Result := AttributeNodes[C_ATTR_TIME].Text;
end;

procedure TXMLTestsuiteType.Set_Time(Value: string);
begin
  SetAttribute(C_ATTR_TIME, Value);
end;

function TXMLTestsuiteType.Get_Properties: IXMLPropertiesType;
begin
  Result := ChildNodes[C_PROPERTIES] as IXMLPropertiesType;
end;

function TXMLTestsuiteType.Get_Testcase: IXMLTestcaseTypeList;
begin
  Result := FTestcase;
end;

function TXMLTestsuiteType.Get_Systemout: WideString;
begin
  Result := ChildNodes[C_SYSTEM_OUT].Text;
end;

procedure TXMLTestsuiteType.Set_Systemout(Value: WideString);
begin
  ChildNodes[C_SYSTEM_OUT].NodeValue := Value;
end;

function TXMLTestsuiteType.Get_Systemerr: WideString;
begin
  Result := ChildNodes[C_SYSTEM_ERR].Text;
end;

procedure TXMLTestsuiteType.Set_Systemerr(Value: WideString);
begin
  ChildNodes[C_SYSTEM_ERR].NodeValue := Value;
end;

{ TXMLPropertiesType }

procedure TXMLPropertiesType.AfterConstruction;
begin
  RegisterChildNode(C_PROPERTY, TXMLPropertyType);
  ItemTag := C_PROPERTY;
  ItemInterface := IXMLPropertyType;
  inherited;
end;

function TXMLPropertiesType.Get_Property_(Index: Integer): IXMLPropertyType;
begin
  Result := List[Index] as IXMLPropertyType;
end;

function TXMLPropertiesType.Add: IXMLPropertyType;
begin
  Result := AddItem(-1) as IXMLPropertyType;
end;

function TXMLPropertiesType.Insert(const Index: Integer): IXMLPropertyType;
begin
  Result := AddItem(Index) as IXMLPropertyType;
end;

{ TXMLPropertyType }

function TXMLPropertyType.Get_Name: string;
begin
  Result := AttributeNodes[C_ATTR_NAME].Text;
end;

procedure TXMLPropertyType.Set_Name(Value: string);
begin
  SetAttribute(C_ATTR_NAME, Value);
end;

function TXMLPropertyType.Get_Value: WideString;
begin
  Result := AttributeNodes[C_ATTR_VALUE].Text;
end;

procedure TXMLPropertyType.Set_Value(Value: WideString);
begin
  SetAttribute(C_ATTR_VALUE, Value);
end;

{ TXMLTestcaseType }

procedure TXMLTestcaseType.AfterConstruction;
begin
  RegisterChildNode(C_FAILURE, TXMLFailureType);
  RegisterChildNode(C_ERROR, TXMLErrorType);
  inherited;
end;

function TXMLTestcaseType.Get_Name: string;
begin
  Result := AttributeNodes[C_ATTR_NAME].Text;
end;

procedure TXMLTestcaseType.Set_Name(Value: string);
begin
  SetAttribute(C_ATTR_NAME, Value);
end;

function TXMLTestcaseType.Get_Time: string;
begin
  Result := AttributeNodes[C_ATTR_TIME].Text;
end;

procedure TXMLTestcaseType.Set_Time(Value: string);
begin
  SetAttribute(C_ATTR_TIME, Value);
end;

function TXMLTestcaseType.Get_Failure: IXMLFailureType;
begin
  Result := ChildNodes[C_FAILURE] as IXMLFailureType;
end;

function TXMLTestcaseType.Get_Error: IXMLErrorType;
begin
  Result := ChildNodes[C_ERROR] as IXMLErrorType;
end;

{ TXMLTestcaseTypeList }

function TXMLTestcaseTypeList.Add: IXMLTestcaseType;
begin
  Result := AddItem(-1) as IXMLTestcaseType;
end;

function TXMLTestcaseTypeList.Insert(const Index: Integer): IXMLTestcaseType;
begin
  Result := AddItem(Index) as IXMLTestcaseType;
end;
function TXMLTestcaseTypeList.Get_Item(Index: Integer): IXMLTestcaseType;
begin
  Result := List[Index] as IXMLTestcaseType;
end;

{ TXMLFailureType }

function TXMLFailureType.Get_Message: WideString;
begin
  Result := AttributeNodes[C_ATTR_MESSAGE].Text;
end;

procedure TXMLFailureType.Set_Message(Value: WideString);
begin
  SetAttribute(C_ATTR_MESSAGE, Value);
end;

function TXMLFailureType.Get_Type_: string;
begin
  Result := AttributeNodes[C_ATTR_TYPE].Text;
end;

procedure TXMLFailureType.Set_Type_(Value: string);
begin
  SetAttribute(C_ATTR_TYPE, Value);
end;

{ TXMLErrorType }

function TXMLErrorType.Get_Message: WideString;
begin
  Result := AttributeNodes[C_ATTR_MESSAGE].Text;
end;

procedure TXMLErrorType.Set_Message(Value: WideString);
begin
  SetAttribute(C_ATTR_MESSAGE, Value);
end;

function TXMLErrorType.Get_Type_: string;
begin
  Result := AttributeNodes[C_ATTR_TYPE].Text;
end;

procedure TXMLErrorType.Set_Type_(Value: string);
begin
  SetAttribute(C_ATTR_TYPE, Value);
end;

end.