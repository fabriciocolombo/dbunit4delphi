object Frm_ExportDataset: TFrm_ExportDataset
  Left = 481
  Top = 118
  Width = 647
  Height = 542
  Caption = 'Export Dataset'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    631
    504)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 639
    Height = 467
    ActivePage = TabSheet_Fields
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabStop = False
    object TabSheet_Tables: TTabSheet
      Caption = 'Tables'
      DesignSize = (
        631
        439)
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 435
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Tables '
        TabOrder = 0
        DesignSize = (
          281
          435)
        object Label1: TLabel
          Left = 13
          Top = 23
          Width = 22
          Height = 13
          Caption = 'Filter'
        end
        object btnLoadTables: TSpeedButton
          Left = 200
          Top = 406
          Width = 75
          Height = 22
          Caption = 'Refresh'
        end
        object edTableFilter: TEdit
          Left = 44
          Top = 19
          Width = 228
          Height = 21
          TabOrder = 0
          OnChange = edTableFilterChange
        end
        object DBGrid_Tables: TDBGrid
          Left = 12
          Top = 45
          Width = 261
          Height = 355
          Anchors = [akLeft, akTop, akBottom]
          DataSource = dsTables
          Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          OnDblClick = DBGrid_TablesDblClick
          OnKeyDown = DBGrid_TablesKeyDown
          Columns = <
            item
              Expanded = False
              FieldName = 'TableName'
              Title.Caption = 'Table Name'
              Width = 200
              Visible = True
            end>
        end
      end
      object GroupBox3: TGroupBox
        Left = 288
        Top = 0
        Width = 335
        Height = 435
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = ' Selected Tables '
        TabOrder = 1
        DesignSize = (
          335
          435)
        object ListBox_Tables: TListBox
          Left = 8
          Top = 20
          Width = 319
          Height = 294
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          TabOrder = 0
          OnClick = ListBox_TablesClick
        end
        object GroupBox_WhereClause: TGroupBox
          Left = 9
          Top = 315
          Width = 318
          Height = 113
          Anchors = [akLeft, akRight, akBottom]
          Caption = ' Where clause for [] '
          TabOrder = 1
          DesignSize = (
            318
            113)
          object Memo_WhereClause: TMemo
            Left = 5
            Top = 15
            Width = 307
            Height = 92
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
            OnChange = Memo_WhereClauseChange
          end
        end
      end
    end
    object TabSheet_Fields: TTabSheet
      Caption = 'Fields'
      ImageIndex = 1
      DesignSize = (
        631
        439)
      object GroupBox2: TGroupBox
        Left = 272
        Top = 8
        Width = 351
        Height = 419
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = ' Fields '
        TabOrder = 0
        DesignSize = (
          351
          419)
        object ListView_Fields: TListView
          Left = 15
          Top = 19
          Width = 328
          Height = 392
          Anchors = [akLeft, akTop, akRight, akBottom]
          Checkboxes = True
          Columns = <
            item
              Caption = 'FieldName'
              Width = 150
            end
            item
              Caption = 'FieldType'
              Width = 80
            end>
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 8
        Width = 257
        Height = 419
        Anchors = [akLeft, akTop, akBottom]
        Caption = ' Selected Tables '
        TabOrder = 1
        DesignSize = (
          257
          419)
        object ListBox_TablesFields: TListBox
          Left = 16
          Top = 24
          Width = 225
          Height = 387
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnClick = ListBox_TablesFieldsClick
        end
      end
    end
    object TabSheet_Export: TTabSheet
      Caption = 'TabSheet_Export'
      ImageIndex = 2
      object lblPath: TLabel
        Left = 8
        Top = 65
        Width = 43
        Height = 13
        Alignment = taRightJustify
        Caption = 'Path/File'
      end
      object btnSelectDir: TSpeedButton
        Left = 502
        Top = 62
        Width = 56
        Height = 22
        Caption = 'Select'
        OnClick = btnSelectDirClick
      end
      object edPath: TEdit
        Left = 60
        Top = 62
        Width = 437
        Height = 21
        TabOrder = 0
      end
      object RadioGroupOutputStyle: TRadioGroup
        Left = 60
        Top = 12
        Width = 245
        Height = 37
        Caption = ' Output style  '
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Single file'
          'One file per table')
        TabOrder = 1
      end
    end
  end
  object brnBack: TButton
    Left = 472
    Top = 475
    Width = 75
    Height = 25
    Action = acBack
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object btnNext: TButton
    Left = 552
    Top = 475
    Width = 75
    Height = 25
    Action = acNext
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 8
    Top = 475
    Width = 75
    Height = 25
    Action = acCancel
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object dsTables: TDataSource
    AutoEdit = False
    DataSet = cdsTables
    Left = 80
    Top = 120
  end
  object cdsTables: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 120
    object cdsTablesTableName: TStringField
      FieldName = 'TableName'
      Size = 30
    end
  end
  object ActionList: TActionList
    Left = 96
    Top = 400
    object acNext: TAction
      Caption = '&Next'
      OnExecute = acNextExecute
    end
    object acBack: TAction
      Caption = '&Back'
      OnExecute = acBackExecute
    end
    object acCancel: TAction
      Caption = '&Cancel'
      OnExecute = acCancelExecute
    end
    object acSortTables: TAction
      Caption = 'Sort Tables'
      OnExecute = acSortTablesExecute
    end
    object acLoadTables: TAction
      Caption = 'Refresh'
      OnExecute = acLoadTablesExecute
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.xml'
    Filter = 'XML (*.xml)|*.xml'
    Left = 76
    Top = 152
  end
end
