unit TestStatementBuilder;

interface

uses BaseTestCase,
     Statement, StatementBuilder, StatementDeleteBuilder, StatementSelectBuilder,
     StatementUpdateBuilder, StatementInsertBuilder,
     Formatter;

type
  TTestStatementBuilder = class(TBaseTestCase)
  private
  public
  published
    //TODO - include restrictions using params to optimization

    procedure SelectAll;
    procedure SelectOneField;
    procedure SelectFiveFields;
    procedure SelectWhereOneField;
    procedure SelectWhereFiveField;
    procedure SelectOneFieldWhereOneField;
    procedure SelectThreeFieldsWhereThreeFields;

    //TODO - include validation in select to test

    procedure DeleteAll;
    procedure DeleteWhereOneField;
    procedure DeleteWhereFiveField;

    //TODO - include validation in delete to test

    procedure InsertOneField;
    procedure InsertThreeField;

    //TODO - include validation in insert to test

    procedure UpdateOneFieldAllRows;
    procedure UpdateOneFieldWhereOneField;
    procedure UpdateThreeFieldsWhereThreeField;    
  end;

implementation

uses CustomStatementBuilder, SysUtils;

const
  sDefaultTableName = 'TestTable';

{ TTestStatementBuilder }

procedure TTestStatementBuilder.DeleteAll;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder.newDelete(sDefaultTableName).build;

  CheckNotNull(vStament);
  CheckEquals('delete from ' + sDefaultTableName, vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.DeleteWhereFiveField;
const
  sExpected = 'delete from ' + sDefaultTableName +
                      ' where um=''1'''+
                      ' and dois=''2'''+
                      ' and tres=''3'''+
                      ' and quatro=''4'''+
                      ' and cinco=''5''';
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder
                .newDelete(sDefaultTableName)
                  .addWhere('um', QuotedStr('1'))
                  .addWhere('dois', QuotedStr('2'))
                  .addWhere('tres', QuotedStr('3'))
                  .addWhere('quatro', QuotedStr('4'))
                  .addWhere('cinco', QuotedStr('5'))
                .build;

  CheckEquals(sExpected, vStament.Command);
  CheckEquals(0, vStament.Params.Count);

end;

procedure TTestStatementBuilder.DeleteWhereOneField;
var
  vStatement: IStatement;
begin
  vStatement := TStatementBuilder
                  .newDelete(sDefaultTableName)
                    .addWhere('um', TFormatter.Format_Int(1))
                  .build;

  CheckEquals('delete from ' + sDefaultTableName + ' where um=1', vStatement.Command);
end;

procedure TTestStatementBuilder.InsertOneField;
var
  vStatement: IStatement;
begin
  vStatement := TStatementBuilder
                  .newInsert(sDefaultTableName)
                  .addFieldInsert('um', QuotedStr('1'))
                  .build;
  CheckEquals('insert into ' + sDefaultTableName + ' (um) values (''1'')', vStatement.Command);
end;

procedure TTestStatementBuilder.InsertThreeField;
var
  vStatement: IStatement;
begin
  vStatement := TStatementBuilder
                  .newInsert(sDefaultTableName)
                  .addFieldInsert('um', TFormatter.Format_Int(1))
                  .addFieldInsert('dois', TFormatter.Format_Int(2))
                  .addFieldInsert('tres', TFormatter.Format_Int(3))
                  .build;
  CheckEquals('insert into ' + sDefaultTableName + ' (um, dois, tres) values (1, 2, 3)', vStatement.Command);
end;

procedure TTestStatementBuilder.SelectAll;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder.newSelect(sDefaultTableName).build;

  CheckNotNull(vStament);
  CheckEquals('select * from ' + sDefaultTableName, vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.SelectFiveFields;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder
                .newSelect(sDefaultTableName)
                  .addField('um')
                  .addField('dois')
                  .addField('tres')
                  .addField('quatro')
                  .addField('cinco')
                .build;

  CheckEquals('select um, dois, tres, quatro, cinco from ' + sDefaultTableName, vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.SelectOneField;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder.newSelect(sDefaultTableName).addField('um').build;

  CheckEquals('select um from ' + sDefaultTableName, vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.SelectOneFieldWhereOneField;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder
                .newSelect(sDefaultTableName)
                  .addField('um')
                  .addWhere('um', TFormatter.Format_Int(1))
                .build;

  CheckEquals('select um from ' + sDefaultTableName + ' where um=1', vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.SelectThreeFieldsWhereThreeFields;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder
                .newSelect(sDefaultTableName)
                  .addField('um')
                  .addField('dois')
                  .addField('tres')
                  .addWhere('um', TFormatter.Format_Int(1))
                  .addWhere('dois', TFormatter.Format_Int(2))
                  .addWhere('tres', TFormatter.Format_Int(3))
                .build;

  CheckEquals('select um, dois, tres from ' + sDefaultTableName +
              ' where um=1'+
              ' and dois=2'+
              ' and tres=3', vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.SelectWhereFiveField;
const
  sExpected = 'select * from ' + sDefaultTableName +
                      ' where um=1'+
                      ' and dois=2'+
                      ' and tres=3'+
                      ' and quatro=4'+
                      ' and cinco=5';
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder
                .newSelect(sDefaultTableName)
                  .addWhere('um', TFormatter.Format_Int(1))
                  .addWhere('dois', TFormatter.Format_Int(2))
                  .addWhere('tres', TFormatter.Format_Int(3))
                  .addWhere('quatro', TFormatter.Format_Int(4))
                  .addWhere('cinco', TFormatter.Format_Int(5))
                .build;

  CheckEquals(sExpected, vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.SelectWhereOneField;
var
  vStament: IStatement;
begin
  vStament := TStatementBuilder.newSelect(sDefaultTableName).addWhere('um', TFormatter.Format_Int(1)).build;

  CheckEquals('select * from ' + sDefaultTableName + ' where um=1', vStament.Command);
  CheckEquals(0, vStament.Params.Count);
end;

procedure TTestStatementBuilder.UpdateOneFieldAllRows;
var
  vStatement: IStatement;
begin
  vStatement := TStatementBuilder
                .newUpdate(sDefaultTableName)
                .addFieldUpdate('um', QuotedStr('1'))
                .build;

  CheckEquals('update ' + sDefaultTableName + ' set um=''1''', vStatement.Command);
end;

procedure TTestStatementBuilder.UpdateOneFieldWhereOneField;
var
  vStatement: IStatement;
begin
  vStatement := TStatementBuilder
                .newUpdate(sDefaultTableName)
                .addFieldUpdate('um', TFormatter.Format_Int(1))
                .addWhere('um', TFormatter.Format_Int(1))
                .build;

  CheckEquals('update ' + sDefaultTableName + ' set um=1'+
              ' where um=1', vStatement.Command);
end;

procedure TTestStatementBuilder.UpdateThreeFieldsWhereThreeField;
var
  vStatement: IStatement;
begin
  vStatement := TStatementBuilder
                .newUpdate(sDefaultTableName)
                .addFieldUpdate('um', TFormatter.Format_Int(1))
                .addFieldUpdate('dois', TFormatter.Format_Int(2))
                .addFieldUpdate('tres', TFormatter.Format_Int(3))
                .addWhere('um', QuotedStr('1'))
                .addWhere('dois', QuotedStr('2'))
                .addWhere('tres', QuotedStr('3'))
                .build;

  CheckEquals('update ' + sDefaultTableName + ' set um=1, dois=2, tres=3'+
              ' where um=''1'''+
              ' and dois=''2'''+
              ' and tres=''3''', vStatement.Command);
end;

initialization
  TTestStatementBuilder.RegisterTest('StatementBuilder');

end.
