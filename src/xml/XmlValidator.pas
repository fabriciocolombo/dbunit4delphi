unit XmlValidator;

interface

uses XmlIntf;

type
  IXmlValidator = interface
  ['{F7831981-2A44-49A3-946E-E63DDB0820AE}']
    procedure Validate(const AXMLDocument: IXMLDocument);
  end;

implementation

end.
