(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 * Laurent Laffont <llaffont@altaiire.fr>
 *
 *)

{
 Contributor : J. Paul Landolt <landolt@royalsys.com>

 Purpose: To create an XML report formatter for use with DUnit, but be
          similar in nature to the Junit test report generated via Ant.

          The ultimate purpose was for the test report to be consumed by
          CruiseControl, a Continuous Integration Tool.

          This Runner makes use of the XML Data Binding tool to produce
          XML Documents with really easy-to-read code (and without lots
          of brittle 'write' statements with XML stuff hard-coded)

          The work was derived from the XMLTestRunner unit contributed by
          Laurent Laffont <llaffont@altaiire.fr>
}
unit CIXMLTestRunner;

interface
uses
  SysUtils, Windows, Classes,
  XMLDoc, XMLIntf, xmldom, msxmldom,

  CIXMLReport,
  TestFramework;

const
   DEFAULT_PREFIX = 'dunit';
   DEFAULT_SUFFIX = '-Out.xml';

type
  TRunnerExitBehavior = (
    rxbContinue,
    rxbHaltOnFailures
  );

  TCIXMLTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
  private
    FFilePrefix: String;
    FFileSuffix: String;
    FAppName:    String;
    iXMLDoc:     TXMLDocument;
    iSuite:      IXMLTestsuiteType;

    procedure Log(AMessage: String);overload;
    procedure Log(AMessage: String; Args: Array of Const);overload;

    function FormatElapsedTime(AElapsedTime: TDateTime): String;
  protected
    suiteStartTime: TDateTime;
    caseStartTime:  TDateTime;

  public
    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function  ShouldRunTest(test :ITest):boolean; virtual;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    constructor Create; overload;
    constructor Create(strPrefix, strSuffix: String); overload;

    class function RunTest(suite: ITest;
                           strFilePrefix,
                           strFileSuffix: String;
                           exitBehavior: TRunnerExitBehavior = rxbContinue
                          ): TTestResult; overload;

    class function RunRegisteredTests(strFilePrefix,
                                      strFileSuffix: String;
                                      exitBehavior: TRunnerExitBehavior = rxbContinue
                                     ): TTestResult;

    class function text2sgml(text : String) : String;
    class function StringReplaceAll (text,byt,mot : string ) :string;
    class function EnvToStringlist: TStringlist;

    { Report Prefix and suffix. If an empty string, then the defaults consts
      above are used
    }
    property FilePrefix: String read FFilePrefix write FFilePrefix;
    property FileSuffix: String read FFileSuffix write FFileSuffix;
  end;

{ Run the given test suite }
function RunTest(suite: ITest;
                 strFilePrefix: String = DEFAULT_PREFIX;
                 strFileSuffix: String = DEFAULT_SUFFIX;
                 exitBehavior: TRunnerExitBehavior = rxbContinue
                ): TTestResult; overload;

function RunRegisteredTests(strFilePrefix: String = DEFAULT_PREFIX;
                            strFileSuffix: String = DEFAULT_SUFFIX;
                            exitBehavior: TRunnerExitBehavior = rxbContinue
                           ): TTestResult; overload;

implementation

uses StrUtils;

const
   CRLF = #13#10;

{* ---- TCIXMLTestListener Class ---------------------------------------- *}
constructor TCIXMLTestListener.Create;
begin
   inherited Create;

   iXMLDoc := nil;
   FilePrefix := DEFAULT_PREFIX;
   FileSuffix := DEFAULT_SUFFIX;
end;

constructor TCIXMLTestListener.Create(strPrefix, strSuffix: String);
begin
   inherited Create;

   iXMLDoc := nil;
   FilePrefix := strPrefix;
   FileSuffix := strSuffix;
end;

procedure TCIXMLTestListener.AddSuccess(test: ITest);
var
  runTime : TDateTime;
  iCase: IXMLTestcaseType;

begin
    { only record a TestCase (not suite info) }
    if (assigned(iSuite)           and
        (isuite.Name <> FAppName)  and
        (isuite.Name <> test.Name)
       ) then
    begin
      { Add a new Test case to the suite }
      iCase := iSuite.Testcase.Add;
      iCase.Name := test.Name;

      runTime := now - caseStartTime;
      iCase.Time := FormatElapsedTime(runTime);
    end;
end;

procedure TCIXMLTestListener.AddError(error: TTestFailure);
var
  runTime: TDateTime;
  iCase:   IXMLTestcaseType;

begin
    { Added the test case with the error message }
    { only record a TestCase (not suite info) }
    if (assigned(iSuite)            and
        (isuite.Name <> FAppName)   and
        (isuite.Name <> error.FailedTest.GetName)
       ) then
    begin
      { Increment the Error count for this suite. Remember that it
        could be an empty string
      }
      try
        iSuite.Errors := IntToStr(StrToInt(iSuite.Errors)+ 1);
      except
        iSuite.Errors := '1';
      end;

      { Add a new Test case to the suite, and note the error info }
      runTime := now - caseStartTime;

      iCase      := iSuite.Testcase.Add;
      iCase.Name := error.FailedTest.GetName;
      iCase.Time := FormatElapsedTime(runTime);

      iCase.Error.Message := error.ThrownExceptionMessage;
      iCase.Error.Type_   := error.ThrownExceptionName;
      iCase.Error.Text    := text2sgml(error.ThrownExceptionMessage);
    end;
end;


procedure TCIXMLTestListener.AddFailure(failure: TTestFailure);
var
  runTime: TDateTime;
  iCase:   IXMLTestcaseType;

begin
    { Added the test case with the failure message }
    { only record a TestCase (not suite info) }
    if (assigned(iSuite)            and
        (isuite.Name <> FAppName)   and
        (isuite.Name <> failure.FailedTest.GetName)
       ) then
    begin
      { Increment the Failure count for this suite. Remember that it
        could be an empty string
      }
      try
        iSuite.Failures := IntToStr(StrToInt(iSuite.Failures)+ 1);
      except
        iSuite.Failures := '1';
      end;

      { Add a new Test case to the suite, and note the failure info }
      runTime := now - caseStartTime;

      iCase      := iSuite.Testcase.Add;
      iCase.Name := failure.FailedTest.GetName;
      iCase.Time := FormatElapsedTime(runTime);
      
      iCase.Failure.Message := failure.ThrownExceptionMessage;
      iCase.Failure.Type_   := failure.ThrownExceptionName;
      iCase.Failure.Text    := text2sgml(failure.ThrownExceptionMessage);
    end;
end;


procedure TCIXMLTestListener.StartTest(test: ITest);
begin
   caseStartTime := now;

   { Increment the Test count for this suite. Remember that it
     could be an empty string
   }
   if assigned(iSuite) then
   begin
     try
       iSuite.Tests := IntToStr(StrToInt(iSuite.Tests)+ 1);
     except
       iSuite.Tests := '1';
     end;
   end;
end;

procedure TCIXMLTestListener.EndTest(test: ITest);
begin
end;

procedure TCIXMLTestListener.TestingStarts;
begin
end;

procedure TCIXMLTestListener.TestingEnds(testResult: TTestResult);
begin
end;

class function TCIXMLTestListener.RunTest(suite: ITest; strFilePrefix,
                                          strFileSuffix: String;
                                          exitBehavior: TRunnerExitBehavior
                                         ): TTestResult;
begin
  Result := TestFramework.RunTest(suite,[TCIXMLTestListener.Create(strFilePrefix, strFileSuffix)]);

  case exitBehavior of
    rxbHaltOnFailures:
{$IFNDEF CLR}
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
{$ENDIF}
    // else fall through
  end;
end;

class function TCIXMLTestListener.RunRegisteredTests(strFilePrefix, strFileSuffix: String; exitBehavior: TRunnerExitBehavior): TTestResult;
begin
  Result := RunTest(registeredTests, strFilePrefix, strFileSuffix, exitBehavior);
end;

function RunTest(suite: ITest; strFilePrefix: String = DEFAULT_PREFIX; strFileSuffix: String = DEFAULT_SUFFIX; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := TestFramework.RunTest(suite, [TCIXMLTestListener.Create(strFilePrefix, strFileSuffix)]);
  
  case exitBehavior of
    rxbHaltOnFailures:
{$IFNDEF CLR}
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
{$ENDIF}
    // else fall through
  end;
end;


function RunRegisteredTests(strFilePrefix: String = DEFAULT_PREFIX;
                            strFileSuffix: String = DEFAULT_SUFFIX;
                            exitBehavior: TRunnerExitBehavior = rxbContinue
                           ): TTestResult;
begin
   Result := RunTest(registeredTests, strFilePrefix, strFileSuffix, exitBehavior);
end;

procedure TCIXMLTestListener.Status(test: ITest; const Msg: string);
begin
end;

procedure TCIXMLTestListener.Warning(test :ITest; const Msg :string);
begin
end;

function TCIXMLTestListener.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
end;


procedure TCIXMLTestListener.EndSuite(suite: ITest);
var
   runTime: TDateTime;
   fstr:    TFileStream;
   s:       String;

begin
    runTime := now-suiteStartTime;

    { Write out Test Suite XML Document
      Since the 'EndSuite' also ends the top level suite (related to AppName)
      ignore it.  Each test suite is generating a separate file, so the
      Top level suite is extraneous
    }
    if (assigned(iXMLDoc) and
        assigned(iSuite)  and
        (suite.Name <> FAppName)
       ) then
    begin
      iSuite.Time := FormatElapsedTime(runTime);

      try
        try
          { Write out the 'XML' statement at the top of the file.  If there is
            a better way to do this (ie, specifying version or encoding as
            a part of DOM) then please change
          }
          fstr := TFileStream.Create(FFilePrefix + suite.Name + FFileSuffix, fmCreate);
          s := '<?xml version="1.0" encoding="UTF-8" ?>' + CRLF;

          fstr.Write(s[1], length(s));
          iXMLDoc.SaveToStream(fstr);
        except
          on E: Exception do
          begin
            Log('ERROR: Error try save the xml file. Cause: %s', [E.Message]);
          end;
        end;
      finally
        FreeAndNil(fstr);
      end;
    end;
end;


procedure TCIXMLTestListener.StartSuite(suite: ITest);
var
  iProp:   IXMLPropertyType;
  stlEnvs: TStringlist;
  I:       Integer;

begin
   suiteStartTime := now;

   { Slurp out the application name.  DUnit stores it as the
     'top-level' suite name.  Since the junit report
     generated via 'ant' (and subsequently consumed by tools
     such as CruiseControl) has no notion of a top-level suite
     (only info per test suite), it should be ignored.
   }
   if length(FAppName) <= 0 then
   begin
     FAppName := suite.Name;
   end;

   { Generate a new XML Document for this test suite }
   if suite.Name <> FAppName then
   begin
      try
        try
          iXMLDoc := TXMLDocument.Create(nil);

          iXMLDoc.Options   := [doNodeAutoCreate,
                                doNodeAutoIndent,
                                doAttrNull,
                                doAutoPrefix,
                                doNamespaceDecl
                               ];
          iXMLDoc.DOMVendor := GetDOMVendor('MSXML');
          iXMLDoc.Active    := True;
        except
          on E: Exception do
          begin
            Log('ERROR: Error activating XmlDocument. Cause: %s', [E.Message]);
          end;
        end;
      finally
      end;

      { Define the new suite Node and give it some basline values }
      try
        try
          iSuite := Gettestsuite(iXMLDoc);

          iSuite.Name     := suite.getName;
          iSuite.Tests    := '0';
          iSuite.Failures := '0';
          iSuite.Errors   := '0';
          iSuite.Time     := '0';

          stlEnvs := EnvToStringlist;
          try
            if assigned(stlEnvs) then
            begin
              for I := 0 to stlEnvs.Count-1 do
              begin
                if length(stlEnvs.Names[i]) > 0 then
                begin
                  iProp := iSuite.Properties.Add;
                  iProp.Name  := stlEnvs.Names[i];
                  iProp.Value := stlEnvs.Values[stlEnvs.Names[i]];
                end;
              end;
            end;
          finally
            stlEnvs.Free;
          end;
        except
          on E: Exception do
          begin
            Log('ERROR: Error retrieving enviroment properties. Cause: %s', [E.Message]);
          end;
        end;
      finally
      end;
   end;
end;

{:
 Replace byt string by mot in text string
 }
class function TCIXMLTestListener.StringReplaceAll(text, byt, mot: string): string;
var
   plats: integer;

begin
  while pos(byt, text) > 0 do
  begin
    plats := pos(byt, text);
    delete(text, plats, length(byt));
    insert(mot, text, plats);
  end;

  result := text;
end;

{:
 Replace special character by sgml compliant characters
 }
class function TCIXMLTestListener.text2sgml(text: String): String;
begin
  text := stringreplaceall(text, '<', '&lt;');
  text := stringreplaceall(text, '>', '&gt;');
  text := stringreplaceall(text, '"', '&quot;');
  text := stringreplaceall(text, #39, '&apos;');
  result := text;
end;


class function TCIXMLTestListener.EnvToStringlist: TStringlist;
var
  stlResult: TStringlist;
  strWork:   WideString;
  DosEnv:    PChar;
  EnvVar:    PChar;

begin
  stlResult := nil;
  DosEnv := GetEnvironmentStrings;
  EnvVar := DosEnv;

  { Loop through the environment variables in the Env string }
  if EnvVar <> nil then
  begin
    repeat
      strWork := StrPas(EnvVar);
      inc(EnvVar, StrLen(EnvVar) + 1);

      { Add the 'name=value' pair to the result stringlist }
      if pos('=', strWork) > 0 then
      begin
        if not assigned(stlResult) then
        begin
          stlResult := TStringList.Create;
          stlResult.Sorted     := True;
          stlResult.Duplicates := dupAccept;
        end;

        stlResult.Add(strWork);
      end;
    until EnvVar^ = #0;
  end;

  { Clean up }
  FreeEnvironmentStrings(DosEnv);

  Result := stlResult;
end;

procedure TCIXMLTestListener.Log(AMessage: String);
begin
  if IsConsole then
  begin
    Writeln(AMessage);
  end;
end;

procedure TCIXMLTestListener.Log(AMessage: String; Args: array of Const);
begin
  Log(Format(AMessage, Args));
end;

function TCIXMLTestListener.FormatElapsedTime(AElapsedTime: TDateTime): String;
begin
  Result := StringReplace(Format('%3.3f', [AElapsedTime * 1E5]), ',', '.', []);
end;

end.
