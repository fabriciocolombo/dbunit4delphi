program dbunit4delphitest;

{$IFDEF AsConsole}
  {$AppType Console}
{$ENDIF}

uses
  FastMM4,
  TestRunnerUtils,
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
  TestDAtaSetDecorator in 'tests\TestDAtaSetDecorator.pas',
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
  TesteMetadata in 'tests\TesteMetadata.pas',
  DBTestCase in '..\src\testcase\DBTestCase.pas',
  Exceptions in '..\src\Exceptions.pas',
  Formatter in '..\src\Formatter.pas',
  DatabaseConfig in '..\src\database\DatabaseConfig.pas',
  DatabaseConnection in '..\src\database\DatabaseConnection.pas',
  DatabaseConnectionFactory in '..\src\database\DatabaseConnectionFactory.pas',
  DatabaseConnectionType in '..\src\database\DatabaseConnectionType.pas',
  DataSet in '..\src\database\DataSet.pas',
  DataSetDecorator in '..\src\database\DataSetDecorator.pas',
  DataSetIterator in '..\src\database\DataSetIterator.pas',
  DataSetListBuilder in '..\src\database\DataSetListBuilder.pas',
  ExportDataSet in '..\src\database\ExportDataSet.pas',
  Field in '..\src\database\Field.pas',
  FlatXmlDataSet in '..\src\database\FlatXmlDataSet.pas',
  Query in '..\src\database\Query.pas',
  Table in '..\src\database\Table.pas',
  XmlClientDataSet in '..\src\database\XmlClientDataSet.pas',
  DatabaseConfigDBX in '..\src\database\dbx\DatabaseConfigDBX.pas',
  DatabaseConnectionDBX in '..\src\database\dbx\DatabaseConnectionDBX.pas',
  QueryDBX in '..\src\database\dbx\QueryDBX.pas',
  CompositeOperation in '..\src\database\operation\CompositeOperation.pas',
  DatabaseOperation in '..\src\database\operation\DatabaseOperation.pas',
  DeleteAllOperation in '..\src\database\operation\DeleteAllOperation.pas',
  DeleteOperation in '..\src\database\operation\DeleteOperation.pas',
  InsertOperation in '..\src\database\operation\InsertOperation.pas',
  NoneOperation in '..\src\database\operation\NoneOperation.pas',
  RefreshOperation in '..\src\database\operation\RefreshOperation.pas',
  UpdateOperation in '..\src\database\operation\UpdateOperation.pas',
  CustomStatementBuilder in '..\src\statement\CustomStatementBuilder.pas',
  Statement in '..\src\statement\Statement.pas',
  StatementBuilder in '..\src\statement\StatementBuilder.pas',
  StatementBuilderIntf in '..\src\statement\StatementBuilderIntf.pas',
  StatementDeleteBuilder in '..\src\statement\StatementDeleteBuilder.pas',
  StatementInsertBuilder in '..\src\statement\StatementInsertBuilder.pas',
  StatementSelectBuilder in '..\src\statement\StatementSelectBuilder.pas',
  StatementUpdateBuilder in '..\src\statement\StatementUpdateBuilder.pas',
  XmlDatasetReader in '..\src\xml\XmlDatasetReader.pas',
  XMLDomParseError in '..\src\xml\XMLDomParseError.pas',
  XmlValidator in '..\src\xml\XmlValidator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DBunit4delphi';

  ExitCode := TTestRunnerUtils.RunRegisteredTests;
end.
