object SearchStringsMainForm: TSearchStringsMainForm
  Left = 0
  Top = 0
  Caption = 'Search Strings'
  ClientHeight = 667
  ClientWidth = 1041
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    1041
    667)
  PixelsPerInch = 96
  TextHeight = 13
  object CopyrightLabel: TLabel
    Left = 23
    Top = 532
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Copyright (c) 2015-%.2d'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 23
    Top = 551
    Width = 72
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Vitaly Yakovlev'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LinkLabel: TLabel
    Left = 22
    Top = 570
    Width = 58
    Height = 13
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'optinsoft.net'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = LinkLabelClick
  end
  object TimeLabel: TLabel
    Left = 648
    Top = 621
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '0 sec.'
  end
  object Label1: TLabel
    Left = 223
    Top = 627
    Width = 154
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '^ Matches the beginning of  line'
  end
  object Label2: TLabel
    Left = 224
    Top = 646
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$ Matches the end of line'
  end
  object Label4: TLabel
    Left = 560
    Top = 507
    Width = 90
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '%s - Search String'
  end
  object Label5: TLabel
    Left = 560
    Top = 535
    Width = 90
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '%z - Source String'
  end
  object Label8: TLabel
    Left = 560
    Top = 554
    Width = 142
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '%f - Formatted Search String'
  end
  object Label11: TLabel
    Left = 216
    Top = 561
    Width = 60
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Match Type:'
  end
  object Label12: TLabel
    Left = 216
    Top = 608
    Width = 93
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Special Characters:'
  end
  object Label13: TLabel
    Left = 560
    Top = 573
    Width = 131
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '%n - Search String Number'
  end
  object CloseButton: TButton
    Left = 38
    Top = 600
    Width = 81
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = CloseButtonClick
  end
  object SearchButton: TButton
    Left = 432
    Top = 613
    Width = 97
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'Search'
    TabOrder = 3
    OnClick = SearchButtonClick
  end
  object SearchMethodGroupBox: TGroupBox
    Left = 727
    Top = 504
    Width = 226
    Height = 92
    Anchors = [akLeft, akBottom]
    Caption = 'Search Method'
    TabOrder = 4
    object SearchListRadioButton: TRadioButton
      Left = 16
      Top = 43
      Width = 105
      Height = 17
      Caption = 'Search List (Slow)'
      TabOrder = 0
      OnClick = SearchListRadioButtonClick
    end
    object SearchTreeRadioButton: TRadioButton
      Left = 16
      Top = 20
      Width = 113
      Height = 17
      Caption = 'Search Tree (Fast)'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = SearchTreeRadioButtonClick
    end
    object SelfCheckRadioButton: TRadioButton
      Left = 16
      Top = 66
      Width = 137
      Height = 17
      Caption = 'Both (Compare Results)'
      TabOrder = 2
      Visible = False
      OnClick = SelfCheckRadioButtonClick
    end
    object TailCharsCheckBox: TCheckBox
      Left = 144
      Top = 20
      Width = 75
      Height = 17
      Caption = 'Tail Chars'
      Checked = True
      State = cbChecked
      TabOrder = 3
      Visible = False
    end
    object SortedListCheckBox: TCheckBox
      Left = 144
      Top = 43
      Width = 73
      Height = 17
      Caption = 'Sorted List'
      TabOrder = 4
      Visible = False
    end
  end
  object BreakButton: TButton
    Left = 544
    Top = 613
    Width = 89
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'Break'
    Enabled = False
    TabOrder = 5
    OnClick = BreakButtonClick
  end
  object FormatSearchCheckBox: TCheckBox
    Left = 216
    Top = 506
    Width = 129
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Format Search Strings'
    TabOrder = 6
  end
  object SearchFormatEdit: TEdit
    Left = 352
    Top = 504
    Width = 193
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    Text = '%s'
  end
  object OutFormatEdit: TEdit
    Left = 352
    Top = 532
    Width = 193
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 8
    Text = '[%n: %s] %z'
  end
  object FormatOutCheckBox: TCheckBox
    Left = 216
    Top = 531
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Format Output'
    TabOrder = 9
  end
  object ResultPageControl: TPageControl
    Left = 616
    Top = 16
    Width = 401
    Height = 473
    ActivePage = ResultListTabSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object ResultListTabSheet: TTabSheet
      Caption = 'Result List'
      DesignSize = (
        393
        445)
      object ResultLabel: TLabel
        Left = 11
        Top = 10
        Width = 67
        Height = 13
        Caption = 'Result list (0):'
      end
      object ResultMemo: TMemo
        Left = 11
        Top = 29
        Width = 369
        Height = 373
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = ResultMemoChange
      end
      object ResultSaveButton: TButton
        Left = 11
        Top = 408
        Width = 102
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Save to file...'
        TabOrder = 1
        OnClick = ResultSaveButtonClick
      end
    end
    object ResultFileTabSheet: TTabSheet
      Caption = 'Result File'
      ImageIndex = 1
      DesignSize = (
        393
        445)
      object Label6: TLabel
        Left = 11
        Top = 11
        Width = 57
        Height = 13
        Caption = 'Output File:'
      end
      object CompletedLabel: TLabel
        Left = 11
        Top = 88
        Width = 55
        Height = 13
        Caption = 'Completed.'
        Visible = False
      end
      object Label9: TLabel
        Left = 214
        Top = 61
        Width = 38
        Height = 13
        Caption = 'Format:'
      end
      object Label10: TLabel
        Left = 214
        Top = 88
        Width = 63
        Height = 13
        Caption = 'Line Endings:'
      end
      object ResultFileEdit: TEdit
        Left = 11
        Top = 29
        Width = 366
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object ResultBrowseButton: TButton
        Left = 11
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = ResultBrowseButtonClick
      end
      object ResultExploreButton: TButton
        Left = 92
        Top = 56
        Width = 111
        Height = 25
        Caption = 'Explore to file'
        TabOrder = 2
        OnClick = ResultExploreButtonClick
      end
      object OutFileFormatComboBox: TComboBox
        Left = 258
        Top = 58
        Width = 119
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 3
        Text = 'UTF-8 without BOM'
        Items.Strings = (
          'UTF-8 without BOM'
          'UTF-8')
      end
      object OutFileEndingsComboBox: TComboBox
        Left = 283
        Top = 85
        Width = 94
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'Windows'
        Items.Strings = (
          'Windows'
          'Unix')
      end
    end
  end
  object SourcePageControl: TPageControl
    Left = 23
    Top = 16
    Width = 314
    Height = 473
    ActivePage = SourceListTabSheet
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    object SourceListTabSheet: TTabSheet
      Caption = 'Source List'
      DesignSize = (
        306
        445)
      object SourceLabel: TLabel
        Left = 11
        Top = 10
        Width = 85
        Height = 13
        Caption = 'List of strings (0):'
      end
      object SourceMemo: TMemo
        Left = 11
        Top = 29
        Width = 285
        Height = 373
        Anchors = [akLeft, akTop, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = SourceMemoChange
      end
      object SourceLoadButton: TButton
        Left = 11
        Top = 408
        Width = 104
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Load from file...'
        TabOrder = 1
        OnClick = SourceLoadButtonClick
      end
      object SourceSaveButton: TButton
        Left = 121
        Top = 408
        Width = 104
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Save to file...'
        TabOrder = 2
        OnClick = SourceSaveButtonClick
      end
    end
    object SourceFileTabSheet: TTabSheet
      Caption = 'Source File'
      ImageIndex = 1
      DesignSize = (
        306
        445)
      object Label7: TLabel
        Left = 12
        Top = 11
        Width = 56
        Height = 13
        Caption = 'Source File:'
      end
      object SourceFileEdit: TEdit
        Left = 12
        Top = 30
        Width = 285
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object SourceBrowseButton: TButton
        Left = 11
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = SourceBrowseButtonClick
      end
      object SourceExploreButton: TButton
        Left = 92
        Top = 56
        Width = 111
        Height = 25
        Caption = 'Explore to file'
        TabOrder = 2
        OnClick = SourceExploreButtonClick
      end
    end
  end
  object PageControl1: TPageControl
    Left = 352
    Top = 16
    Width = 250
    Height = 473
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 10
    object TabSheet1: TTabSheet
      Caption = 'Search Strings'
      DesignSize = (
        242
        445)
      object SearchLabel: TLabel
        Left = 11
        Top = 10
        Width = 90
        Height = 13
        Caption = 'Search Strings (0):'
      end
      object SearchLoadButton: TButton
        Left = 11
        Top = 408
        Width = 105
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Load from file...'
        TabOrder = 0
        OnClick = SearchLoadButtonClick
      end
      object SearchMemo: TMemo
        Left = 11
        Top = 28
        Width = 218
        Height = 373
        Anchors = [akLeft, akTop, akBottom]
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
        OnChange = SearchMemoChange
      end
      object SearchSaveButton: TButton
        Left = 122
        Top = 407
        Width = 106
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Save to file...'
        TabOrder = 2
        OnClick = SearchSaveButtonClick
      end
    end
  end
  object MatchTypeComboBox: TComboBox
    Left = 216
    Top = 580
    Width = 185
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 0
    TabOrder = 11
    Text = 'Contains'
    Items.Strings = (
      'Contains'
      'Contains with Special Characters'
      'Exact Match'
      'Starts With'
      'Ends With'
      'Equals'
      'Less Than'
      'Less Than Or Equals To'
      'More Than'
      'More Than Or Equals To')
  end
  object SearchAllCheckBox: TCheckBox
    Left = 416
    Top = 582
    Width = 81
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Search All'
    TabOrder = 12
  end
  object CaseSensitiveCheckBox: TCheckBox
    Left = 416
    Top = 559
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Case Sensitive'
    TabOrder = 13
  end
  object SourceSaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = 
      'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)' +
      '|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 72
    Top = 144
  end
  object SourceOpenDialog: TOpenDialog
    Filter = 
      'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)' +
      '|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 88
  end
  object SearchOpenDialog: TOpenDialog
    Filter = 
      'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)' +
      '|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 376
    Top = 48
  end
  object ResultOpenDialog: TOpenDialog
    Filter = 
      'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)' +
      '|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 624
    Top = 88
  end
  object SearchSaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = 
      'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)' +
      '|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 376
    Top = 112
  end
  object ResultSaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = 
      'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.*)' +
      '|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 624
    Top = 152
  end
end
