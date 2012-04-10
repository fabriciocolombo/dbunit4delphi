unit uExportDataset;

interface

uses
  DatabaseConnection, DatabaseConnectionFactory, DataSet,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, DBClient, CheckLst, ComCtrls, ActnList,
  FileCtrl,
  Buttons, ExtCtrls;

const
  EXPORT_STYLE_SINGLE_FILE = 0;
  EXPORT_STYLE_ONE_PER_TABLE = 1;
  DEFAULT_EXTENSION = '.xml';

  sWhereClauseCaption = ' Where clause for [%s] ';

type

  TFrm_ExportDataset = class(TForm)
    dsTables: TDataSource;
    cdsTables: TClientDataSet;
    cdsTablesTableName: TStringField;
    PageControl: TPageControl;
    TabSheet_Tables: TTabSheet;
    TabSheet_Fields: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edTableFilter: TEdit;
    DBGrid_Tables: TDBGrid;
    GroupBox2: TGroupBox;
    ListView_Fields: TListView;
    brnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    GroupBox3: TGroupBox;
    ActionList: TActionList;
    acNext: TAction;
    acBack: TAction;
    acCancel: TAction;
    ListBox_Tables: TListBox;
    GroupBox4: TGroupBox;
    ListBox_TablesFields: TListBox;
    acSortTables: TAction;
    TabSheet_Export: TTabSheet;
    lblPath: TLabel;
    edPath: TEdit;
    btnSelectDir: TSpeedButton;
    RadioGroupOutputStyle: TRadioGroup;
    OpenDialog1: TOpenDialog;
    GroupBox_WhereClause: TGroupBox;
    Memo_WhereClause: TMemo;
    btnLoadTables: TSpeedButton;
    acLoadTables: TAction;
    procedure edTableFilterChange(Sender: TObject);
    procedure DBGrid_TablesDblClick(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acBackExecute(Sender: TObject);
    procedure acNextExecute(Sender: TObject);
    procedure DBGrid_TablesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBox_TablesFieldsClick(Sender: TObject);
    procedure acSortTablesExecute(Sender: TObject);
    procedure btnSelectDirClick(Sender: TObject);
    procedure ListBox_TablesClick(Sender: TObject);
    procedure Memo_WhereClauseChange(Sender: TObject);
    procedure acLoadTablesExecute(Sender: TObject);
  private
    procedure EnableControls;

    procedure NavigateForward;
    procedure NavigateBackward;

    procedure DoNavigate(GoForward: Boolean);

    procedure ValidateCurrentState;

    procedure ValidatePageTables;
    procedure ValidatePageFields;
    procedure ValidatePageExport;

    procedure GoToInitialState;

    procedure ExportDataSet;

    function IsLastPage: Boolean;

    procedure ShowHideWhereClause(AShow: Boolean); 
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Frm_ExportDataset: TFrm_ExportDataset;

implementation

uses Math, uDMGenerator, ExportDataSet, uTableForExport;

{$R *.dfm}

procedure TFrm_ExportDataset.edTableFilterChange(Sender: TObject);
begin
  cdsTables.Filtered := False;
  cdsTables.Filter   := Format('Upper(TableName) like ''%s%%''', [AnsiUpperCase(edTableFilter.Text)]);
  cdsTables.Filtered := True;
end;

procedure TFrm_ExportDataset.DBGrid_TablesDblClick(Sender: TObject);
var
  vTableForExport: TTableForExport;
begin
  if ListBox_Tables.Items.IndexOf(cdsTablesTableName.AsString) < 0 then
  begin
    vTableForExport := TTableForExport.Create(cdsTablesTableName.AsString);
    ListBox_Tables.Items.AddObject(cdsTablesTableName.AsString, vTableForExport);
    ListBox_TablesFields.Items.AddObject(cdsTablesTableName.AsString, vTableForExport);
  end;
end;

procedure TFrm_ExportDataset.EnableControls;
begin
  acBack.Enabled := PageControl.ActivePageIndex > 0;

  if IsLastPage then
    btnNext.Caption := '&Export'
  else
    btnNext.Caption := acNext.Caption;
end;

procedure TFrm_ExportDataset.acCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrm_ExportDataset.NavigateForward;
begin
  DoNavigate(True)
end;

procedure TFrm_ExportDataset.NavigateBackward;
begin
  DoNavigate(False);
end;

procedure TFrm_ExportDataset.acBackExecute(Sender: TObject);
begin
  NavigateBackward;
end;

procedure TFrm_ExportDataset.acNextExecute(Sender: TObject);
begin
  NavigateForward;
end;

procedure TFrm_ExportDataset.ValidateCurrentState;
begin
  if PageControl.ActivePageIndex = TabSheet_Tables.PageIndex then
  begin
    ValidatePageTables;
  end
  else if PageControl.ActivePageIndex = TabSheet_Fields.PageIndex then
  begin
    ValidatePageFields;
  end
  else if PageControl.ActivePageIndex = TabSheet_Export.PageIndex then
  begin
    ValidatePageExport;
  end;
end;

procedure TFrm_ExportDataset.DoNavigate(GoForward: Boolean);
begin
  if GoForward then
  begin
    ValidateCurrentState;
  end;

  if IsLastPage and GoForward then
    ExportDataSet
  else
    PageControl.SelectNextPage(GoForward);

  EnableControls;
end;

procedure TFrm_ExportDataset.GoToInitialState;
begin
  ListBox_Tables.Items.Clear;

  GroupBox_WhereClause.Caption := Format(sWhereClauseCaption, [EmptyStr]);
  Memo_WhereClause.Lines.Clear;
  ShowHideWhereClause(False);

  PageControl.ActivePageIndex := 0;

  EnableControls;

  acLoadTables.Execute;

  if (edPath.Text = EmptyStr) then
  begin
    edPath.Text := ExtractFilePath(Application.ExeName);
  end;
end;

constructor TFrm_ExportDataset.Create(AOwner: TComponent);
begin
  inherited;
  cdsTables.CreateDataSet;
  GoToInitialState;
end;

procedure TFrm_ExportDataset.ValidatePageTables;
begin
  if (ListBox_Tables.Count = 0) then
    raise Exception.Create('No table selected'); 
end;

procedure TFrm_ExportDataset.ValidatePageFields;
var
  i: Integer;
  vHasCheckedItem: Boolean;
begin
  vHasCheckedItem := False;
  for i := 0 to ListView_Fields.Items.Count-1 do
  begin
    if (ListView_Fields.Items[i].Checked) then
    begin
      vHasCheckedItem := True;
      Break;
    end;
  end;

  if not vHasCheckedItem then
    raise Exception.CreateFmt('No fields selected.', []);
end;

procedure TFrm_ExportDataset.DBGrid_TablesKeyDown(Sender: TObject;var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) then
    Key := Ord(0);
end;

procedure TFrm_ExportDataset.ListBox_TablesFieldsClick(Sender: TObject);
var
  vFields: TFieldListMetadata;
  vTable: String;
  i: Integer;
begin
  if ListBox_TablesFields.ItemIndex >= 0 then
  begin
    vTable := ListBox_TablesFields.Items[ListBox_TablesFields.ItemIndex];

    vFields := DMGenerator.Connection.getFields(vTable);

    ListView_Fields.Items.Clear;

    for i := 0 to vFields.Count-1 do
    begin
      with ListView_Fields.Items.Add do
      begin
        Caption := vFields.Fields[i].FieldName;
        SubItems.Add(FieldTypeNames[vFields.Fields[i].DataType]);
      end;
    end;
  end;
end;

procedure TFrm_ExportDataset.ExportDataSet;
var
  i: Integer;
  vTable: TTableForExport;
  vExporter: IDataExporter;
begin
  case RadioGroupOutputStyle.ItemIndex of
    EXPORT_STYLE_SINGLE_FILE:
      begin
        vExporter := TDataExporter.CreateWithConnection(DMGenerator.Connection);

        for i := 0 to ListBox_Tables.Items.Count-1 do
        begin
          vTable := TTableForExport(ListBox_Tables.Items.Objects[i]);

          vExporter.WithQueryText(vTable.TableName, vTable.GetQuery);
        end;

        vExporter.ExportToXmlFile(edPath.Text);
      end;
    EXPORT_STYLE_ONE_PER_TABLE:
      begin
        edPath.Text := IncludeTrailingPathDelimiter(edPath.Text);

        for i := 0 to ListBox_Tables.Items.Count-1 do
        begin
          vTable := TTableForExport(ListBox_Tables.Items.Objects[i]);

          TDataExporter.CreateWithConnection(DMGenerator.Connection)
            .WithQueryText(vTable.TableName, vTable.GetQuery)
            .ExportToXmlFile(Format('%s%s.xml',[edPath.Text, vTable.TableName]));
        end;
      end;
  end;

  ShowMessage('Exported successfully');

  GoToInitialState; 
end;

function TFrm_ExportDataset.IsLastPage: Boolean;
begin
  Result := (PageControl.ActivePageIndex = PageControl.PageCount-1);
end;

procedure TFrm_ExportDataset.acSortTablesExecute(Sender: TObject);
begin
  ListBox_Tables.Sorted := True;
  ListBox_Tables.Sorted := False;
end;

procedure TFrm_ExportDataset.btnSelectDirClick(Sender: TObject);
var
  vTarget: String;
  vIsSelected: Boolean;
begin
  case RadioGroupOutputStyle.ItemIndex of
    EXPORT_STYLE_SINGLE_FILE:
      begin
        vIsSelected := OpenDialog1.Execute;
        if vIsSelected then
          vTarget := OpenDialog1.FileName;
      end;
    EXPORT_STYLE_ONE_PER_TABLE:
      begin
        vIsSelected := SelectDirectory('Directory to export', ExtractFileDrive(Application.ExeName), vTarget);
      end;
  else
    vIsSelected := False;
  end;

  if vIsSelected then
    edPath.Text := vTarget;
end;

procedure TFrm_ExportDataset.ValidatePageExport;
const
  sMsgDirectoryNotExists = 'Directory does not exist. Would you like to create it?';
begin
  if SameText(Trim(edPath.Text), EmptyStr) then
    raise Exception.Create('No path selected to export.');

  case RadioGroupOutputStyle.ItemIndex of
    EXPORT_STYLE_SINGLE_FILE: begin
                                if (ExtractFileName(edPath.Text) = EmptyStr) then
                                begin
                                  raise Exception.Create('Select a valid output file to export.');
                                end;
                                if StringReplace(ExtractFileExt(edPath.Text),'.',EmptyStr,[]) = EmptyStr then
                                  edPath.Text := edPath.Text + DEFAULT_EXTENSION;
                              end;
    EXPORT_STYLE_ONE_PER_TABLE: begin
                                  if SameText(ExtractFileExt(edPath.Text), DEFAULT_EXTENSION) then
                                    edPath.Text := ExtractFilePath(edPath.Text);
                                  if not DirectoryExists(edPath.Text) then
                                  begin
                                    if (MessageBox(Handle, PChar(sMsgDirectoryNotExists), PChar('Directory not exists'), MB_YESNOCANCEL or MB_ICONQUESTION) = mrYes) then
                                    begin
                                      if not ForceDirectories(edPath.Text) then
                                        raise Exception.CreateFmt('Fail to create directory "%s". System error: %s',[edPath.Text, SysErrorMessage(GetLastError)]);
                                    end
                                    else
                                      Abort;
                                  end;
                                end;  
  end;
end;

procedure TFrm_ExportDataset.ListBox_TablesClick(Sender: TObject);
var
  vTable: TTableForExport;
begin
  if ListBox_Tables.ItemIndex >= 0 then
  begin
    vTable := TTableForExport(ListBox_Tables.Items.Objects[ListBox_Tables.ItemIndex]);

    GroupBox_WhereClause.Caption := Format(sWhereClauseCaption, [vTable.TableName]);
    Memo_WhereClause.Lines.Text := vTable.WhereClause;
    ShowHideWhereClause(True);
    Memo_WhereClause.SetFocus;
  end;
end;

procedure TFrm_ExportDataset.Memo_WhereClauseChange(Sender: TObject);
var
  vTable: TTableForExport;
begin
  if ListBox_Tables.ItemIndex >= 0 then
  begin
    vTable := TTableForExport(ListBox_Tables.Items.Objects[ListBox_Tables.ItemIndex]);
    vTable.WhereClause := Memo_WhereClause.Lines.Text;
  end;
end;

procedure TFrm_ExportDataset.acLoadTablesExecute(Sender: TObject);
var
  vTables: TStringList;
  i: Integer;
begin
  vTables := TStringList.Create;
  try
    DMGenerator.Connection.getTableNames(vTables);

    cdsTables.DisableControls;
    try
      cdsTables.EmptyDataSet;
      for i:= 0 to vTables.Count-1 do
      begin
        cdsTables.Append;
        cdsTablesTableName.AsString := vTables[i];
        cdsTables.Post;
      end;
    finally
      cdsTables.First;
      cdsTables.EnableControls;
    end;
  finally
    vTables.Free;
  end;
end;

procedure TFrm_ExportDataset.ShowHideWhereClause(AShow: Boolean);
begin
  if (GroupBox_WhereClause.Visible <> AShow) then
  begin
    GroupBox_WhereClause.Visible := AShow;
    GroupBox_WhereClause.Enabled := AShow;

    if AShow then
      ListBox_Tables.Height := ListBox_Tables.Height - GroupBox_WhereClause.Height
    else
      ListBox_Tables.Height := ListBox_Tables.Height + GroupBox_WhereClause.Height;
  end;
end;

end.
