unit CompatibilityClasses;

interface

{$I dbunit4delphi.inc}

uses SqlExpr, {$IFDEF D2009UP}DBXCommon{$ELSE}DBXpress{$ENDIF};

type
  TTransaction = {$IFDEF D2009UP}TDBXTransaction{$ELSE}TTransactionDesc{$ENDIF};

type
  TDBXConnectionCompatibility = class
  private
  public
    class function UniqueTransaction: TTransaction;
    class procedure Commit(ASqlConnection: TSQLConnection; ATransaction: TTransaction);
    class procedure Rollback(ASqlConnection: TSQLConnection; ATransaction: TTransaction);
    class function StartTransaction(ASqlConnection: TSQLConnection; ATransaction: TTransaction): TTransaction;
  end;

implementation

{ TDBXConnectionCompatibility }

class procedure TDBXConnectionCompatibility.Commit(ASqlConnection: TSQLConnection; ATransaction: TTransaction);
begin
  {$IFDEF D2009UP}
    ASqlConnection.CommitFreeAndNil(ATransaction);
  {$ELSE}
    ASqlConnection.Commit(ATransaction);
  {$ENDIF};
end;

class procedure TDBXConnectionCompatibility.Rollback(ASqlConnection: TSQLConnection; ATransaction: TTransaction);
begin
  {$IFDEF D2009UP}
    ASqlConnection.RollbackFreeAndNil(ATransaction);
  {$ELSE}
    ASqlConnection.Rollback(ATransaction);
  {$ENDIF};
end;

class function TDBXConnectionCompatibility.StartTransaction(ASqlConnection: TSQLConnection;ATransaction: TTransaction): TTransaction;
begin
  {$IFDEF D2009UP}
    Result := FSqlConnection.BeginTransaction;
  {$ELSE}
    ASqlConnection.StartTransaction(ATransaction);
    Result := ATransaction;
  {$ENDIF};
end;

class function TDBXConnectionCompatibility.UniqueTransaction: TTransaction;
begin
  Result.TransactionID := 1;
  Result.GlobalID := 0;
  Result.IsolationLevel := xilREADCOMMITTED;
end;

end.
