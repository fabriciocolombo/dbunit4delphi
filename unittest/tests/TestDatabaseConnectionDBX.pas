unit TestDatabaseConnectionDBX;

interface

uses TestCaseExtension,  DatabaseConnection, DatabaseConnectionDBX, DatabaseConnectionFactory,
     DatabaseConfig, DatabaseConfigDBX, DatabaseConnectionType, SqlExpr, Exceptions,
     Query, QueryDBX, StubDatabaseConfig, StubDatabaseConfigDBX, DB;

type
  TTestDatabaseConnectionDBX = class(TTestCaseExtension)
  private
  public
  published
    procedure invalidConfigClass;
    procedure invalidConfiguration;
    procedure newConnection;
    procedure newQuery;
    procedure queryExecSql;
    procedure OpenAndClose;
    procedure Transaction;
    procedure ExecuteStatement;
    procedure GetFields;
  end;

implementation

uses TestUtils, TestFramework, Field, DataSet, Statement;

{ TTestDatabaseConnectionDBX }

procedure TTestDatabaseConnectionDBX.ExecuteStatement;
var
  vConn: IDatabaseConnection;
  vStatement: IStatement;
  vRowsAffected: Integer;
begin
  vConn := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);

  vStatement := TStatement.Create('update dual set dummy = dummy');

  vRowsAffected := vConn.Execute(vStatement);

  CheckEquals(1, vRowsAffected);
end;

procedure TTestDatabaseConnectionDBX.GetFields;
var
  vConn: IDatabaseConnection;
  vFields: TFieldListMetadata;
begin
  vConn := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);

  vFields := vConn.getFields('DUAL');
  try
    CheckEquals(1, vFields.Count, 'FieldListMetadata size');
    CheckEqualsText('Dummy', vFields.Fields[0].FieldName);
    CheckEquals(Ord(ftString), Ord(vFields.Fields[0].DataType));
    CheckEquals(1, vFields.Fields[0].Size);
  finally
    vFields.Free;
  end;
end;

procedure TTestDatabaseConnectionDBX.invalidConfigClass;
begin
  ExpectedException := EInvalidDatabaseConfigClass;
  ConnectionFactory.newConnection(TStubDatabaseConfig.newConfig(dctDBX));
end;

procedure TTestDatabaseConnectionDBX.invalidConfiguration;
begin
  ExpectedException := EInvalidDatabaseConfiguration;
  ConnectionFactory.newConnection(TDatabaseConfigDBX.newConfig(dctDBX));
end;

procedure TTestDatabaseConnectionDBX.newConnection;
var
  vConn: IDatabaseConnection;
begin
  vConn := ConnectionFactory.newConnection(TStubDatabaseConfigDBX.newConfig(dctDBX), False);

  CheckNotNull(vConn);
  CheckTrue(vConn.realConnection is TSQLConnection);
end;

procedure TTestDatabaseConnectionDBX.newQuery;
var
  vConn: IDatabaseConnection;
  vQuery: IQuery; 
begin
  vConn := ConnectionFactory.newConnection(TStubDatabaseConfigDBX.newConfig(dctDBX), False);

  CheckTrue(vConn.realConnection is TSQLConnection);

  vQuery := vConn.createQuery;

  CheckNotNull(vQuery);
  CheckTrue(vQuery.realQuery is TSQLQuery);
end;

procedure TTestDatabaseConnectionDBX.OpenAndClose;
var
  vConn: IDatabaseConnection;
begin
  vConn := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX, False);
  CheckFalse(vConn.connected);
  vConn.open;
  CheckTrue(vConn.connected);
  vConn.close;
  CheckFalse(vConn.connected);
end;

procedure TTestDatabaseConnectionDBX.queryExecSql;
var
  vConn: IDatabaseConnection;
  vQuery: IQuery; 
begin
  vConn := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);

  vQuery := vConn.createQuery;
  vQuery.Sql.Text := 'update dual set dummy = dummy';
  CheckEquals(1, vQuery.Execute);
end;

procedure TTestDatabaseConnectionDBX.Transaction;
var
  vConn: IDatabaseConnection;
begin
  vConn := ConnectionFactory.newConnection(TTestUtils.DATABASECONFIGDBX);

  vConn.open;

  CheckFalse(vConn.inTransaction);

  vConn.StartTransaction;
  CheckTrue(vConn.inTransaction);

  vConn.RollbackTransaction;
  CheckFalse(vConn.inTransaction);

  vConn.StartTransaction;
  CheckTrue(vConn.inTransaction);

  vConn.CommitTransaction;
  CheckFalse(vConn.inTransaction);
end;

initialization
  TTestDatabaseConnectionDBX.RegisterTest('DatabaseConnectionDBX');
  
end.
