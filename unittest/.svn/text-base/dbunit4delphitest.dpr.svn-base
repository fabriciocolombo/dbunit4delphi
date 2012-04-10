program dbunit4delphitest;

uses
  FastMM4,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Forms,
  TestFormatter in 'tests\TestFormatter.pas',
  TestStatementBuilder in 'tests\TestStatementBuilder.pas',
  TestDatabaseConnection in 'tests\TestDatabaseConnection.pas',
  TestDatabaseConnectionDBX in 'tests\TestDatabaseConnectionDBX.pas',
  StubDatabaseConfig in 'stubs\StubDatabaseConfig.pas',
  StubDatabaseConfigDBX in 'stubs\StubDatabaseConfigDBX.pas',
  TestXmlDatasetReader in 'tests\TestXmlDatasetReader.pas',
  SampleDBTestCase in 'tests\SampleDBTestCase.pas',
  TestDataSetIterator in 'tests\TestDataSetIterator.pas',
  TestXmlDataSet in 'tests\TestXmlDataSet.pas',
  TestDataSetListBuilder in 'tests\TestDataSetListBuilder.pas',
  MockDataSet in 'mocks\MockDataSet.pas',
  TestDataSetDecorator in 'tests\TestDataSetDecorator.pas',
  MockDatabaseConnection in 'mocks\MockDatabaseConnection.pas',
  StubDataset in 'stubs\StubDataset.pas',
  TestInsertOperation in 'tests\TestInsertOperation.pas',
  TestResources in 'TestResources.pas',
  TestUtils in 'TestUtils.pas',
  TestDeleteOperation in 'tests\TestDeleteOperation.pas',
  TestUpdateOperation in 'tests\TestUpdateOperation.pas',
  MockXmlValidator in 'mocks\MockXmlValidator.pas',
  TestCompositeOperation in 'tests\TestCompositeOperation.pas',
  MockDatabaseOperation in 'mocks\MockDatabaseOperation.pas',
  TestRefreshOperation in 'tests\TestRefreshOperation.pas',
  TestMockObjects in 'tests\TestMockObjects.pas',
  TestExportDataSet in 'tests\TestExportDataSet.pas',
  TesteMetadata in 'tests\TesteMetadata.pas';

{$R *.res}

{$IFDEF AsConsole}
  {$AppType Console}
{$ENDIF}

var
  vResult: TTestResult;
begin
  Application.Initialize;
  Application.Title := 'DBunit4delphi';

  if IsConsole then
  begin
    vResult := TextTestRunner.RunRegisteredTests();
    try
      if (vResult.ErrorCount + vResult.FailureCount) > 0 then
        ExitCode := 1
      else
        ExitCode := 0;
    finally
      vResult.Free;
    end;
  end
  else
  begin
    GUITestRunner.RunRegisteredTests;
  end;
end.
