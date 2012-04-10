unit MockXmlValidator;

interface

uses XmlValidator, XMLIntf;

type
  IMockXmlValidator = interface(IXmlValidator)
  ['{6B372176-3DE5-48D0-A532-A83B145F684F}']
    procedure setExpectationCalls(AQuantity: Integer);
    procedure checkCalls; 
  end;

  TMockXmlValidator = class(TInterfacedObject, IXmlValidator, IMockXmlValidator)
  private
    FExpectedCalls: Integer;
    FCalls: Integer;
  public
    procedure Validate(const AXMLDocument: IXMLDocument);
    procedure checkCalls;
    procedure setExpectationCalls(AQuantity: Integer);
   end;

implementation

uses Exceptions;


{ TMockXmlValidator }

procedure TMockXmlValidator.checkCalls;
begin
  if (FCalls <> FExpectedCalls) then
    raise EExpectationNotSatisfacted.Create(FExpectedCalls, FCalls, 'Validate');
end;

procedure TMockXmlValidator.setExpectationCalls(AQuantity: Integer);
begin
  FExpectedCalls := AQuantity;
end;

procedure TMockXmlValidator.Validate(const AXMLDocument: IXMLDocument);
begin
  Inc(FCalls);
end;

end.
