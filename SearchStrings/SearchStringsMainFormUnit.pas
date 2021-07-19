{ **************************************************************************** }
{                                                                              }
{ This file is part of SearchStrings project                                   }
{                                                                              }
{ Created by Vitaly Yakovlev                                                   }
{ Copyright: (c) 2015 Vitaly Yakovlev                                          }
{ Website: http://optinsoft.net/                                               }
{                                                                              }
{ License: BSD 2-Clause License.                                               }
{                                                                              }
{ Redistribution and use in source and binary forms, with or without           }
{ modification, are permitted provided that the following conditions are met:  }
{                                                                              }
{ 1. Redistributions of source code must retain the above copyright notice,    }
{    this list of conditions and the following disclaimer.                     }
{                                                                              }
{ 2. Redistributions in binary form must reproduce the above copyright notice, }
{    this list of conditions and the following disclaimer in the documentation }
{    and/or other materials provided with the distribution.                    }
{                                                                              }
{ THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  }
{ AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,        }
{ THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR       }
{ PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR            }
{ CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,        }
{ EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,          }
{ PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;  }
{ OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,     }
{ WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      }
{ OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF       }
{ ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                   }
{                                                                              }
{ Last edit by: Vitaly Yakovlev                                                }
{ Date: July 19, 2021                                                          }
{ Version: 1.1                                                                 }
{                                                                              }
{ v1.1:                                                                        }
{  - copyright                                                                 }
{ v1.0:                                                                        }
{ - Initial release                                                            }
{                                                                              }
{ **************************************************************************** }

unit SearchStringsMainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uSearchStrings, uSearchClass,
  uLineReaderWriter, Vcl.ComCtrls;

type
  TSearchMethod = (smSearchList, smSearchTree, smSelfCheck);

  TSearchStringsMainForm = class(TForm)
    CopyrightLabel: TLabel;
    Label3: TLabel;
    LinkLabel: TLabel;
    CloseButton: TButton;
    SourceLabel: TLabel;
    SourceMemo: TMemo;
    SearchLabel: TLabel;
    SearchMemo: TMemo;
    SearchButton: TButton;
    ResultMemo: TMemo;
    ResultLabel: TLabel;
    TimeLabel: TLabel;
    SearchMethodGroupBox: TGroupBox;
    SearchListRadioButton: TRadioButton;
    SearchTreeRadioButton: TRadioButton;
    SelfCheckRadioButton: TRadioButton;
    BreakButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    FormatSearchCheckBox: TCheckBox;
    SearchFormatEdit: TEdit;
    Label4: TLabel;
    OutFormatEdit: TEdit;
    FormatOutCheckBox: TCheckBox;
    Label5: TLabel;
    ResultPageControl: TPageControl;
    ResultListTabSheet: TTabSheet;
    ResultFileTabSheet: TTabSheet;
    SourcePageControl: TPageControl;
    SourceListTabSheet: TTabSheet;
    SourceFileTabSheet: TTabSheet;
    Label6: TLabel;
    ResultFileEdit: TEdit;
    ResultBrowseButton: TButton;
    ResultExploreButton: TButton;
    CompletedLabel: TLabel;
    Label7: TLabel;
    SourceFileEdit: TEdit;
    SourceBrowseButton: TButton;
    SourceExploreButton: TButton;
    SourceSaveDialog: TSaveDialog;
    SourceOpenDialog: TOpenDialog;
    Label8: TLabel;
    TailCharsCheckBox: TCheckBox;
    ResultSaveButton: TButton;
    SourceLoadButton: TButton;
    SourceSaveButton: TButton;
    SearchLoadButton: TButton;
    SearchSaveButton: TButton;
    SearchOpenDialog: TOpenDialog;
    ResultOpenDialog: TOpenDialog;
    SearchSaveDialog: TSaveDialog;
    ResultSaveDialog: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label9: TLabel;
    Label10: TLabel;
    OutFileFormatComboBox: TComboBox;
    OutFileEndingsComboBox: TComboBox;
    Label11: TLabel;
    MatchTypeComboBox: TComboBox;
    Label12: TLabel;
    SearchAllCheckBox: TCheckBox;
    Label13: TLabel;
    CaseSensitiveCheckBox: TCheckBox;
    SortedListCheckBox: TCheckBox;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LinkLabelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SourceMemoChange(Sender: TObject);
    procedure SearchMemoChange(Sender: TObject);
    procedure ResultMemoChange(Sender: TObject);
    procedure BreakButtonClick(Sender: TObject);
    procedure ResultBrowseButtonClick(Sender: TObject);
    procedure SourceBrowseButtonClick(Sender: TObject);
    procedure ResultExploreButtonClick(Sender: TObject);
    procedure SourceExploreButtonClick(Sender: TObject);
    procedure ResultSaveButtonClick(Sender: TObject);
    procedure SourceLoadButtonClick(Sender: TObject);
    procedure SourceSaveButtonClick(Sender: TObject);
    procedure SearchLoadButtonClick(Sender: TObject);
    procedure SearchSaveButtonClick(Sender: TObject);
    procedure SearchListRadioButtonClick(Sender: TObject);
    procedure SearchTreeRadioButtonClick(Sender: TObject);
    procedure SelfCheckRadioButtonClick(Sender: TObject);
  private
    { Private declarations }
    fSearchClass: TSearchClass;
    procedure LoadMemo(lst: TStrings; Memo: TMemo; const SName: String);
    procedure SaveMemo(lst: TStrings; Memo: TMemo; const SName: String);
    function ConfigFileName(bLoad: Boolean): String;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure ReadMemo(lst: TStrings; Memo: TMemo);
    procedure FormatList(lstIn, lstOut: TStrings; const StringFormat: String);
    procedure DoOnProcessed(Sender: TObject; nProcessed, nFound: Integer);
    procedure MsgWarning(const sMessage: String; const sTitle: String = 'Warning');
    procedure SetStatus(const sStatus: String);
    procedure ExploreToFile(const sFileName: String);
  public
    { Public declarations }
  end;

var
  SearchStringsMainForm: TSearchStringsMainForm;

implementation

uses StrUtils, DateUtils, ShellAPI, uAppFolder;

{$I SearchStrings_ver.inc}

{$R *.dfm}

const
  AppDataSubFolder = 'SearchStrings';

function IndexOfText(const S: String; Strings: array of string): Integer;
var
  I: Integer;
begin
  for I := Low(Strings) to High(Strings) do
  begin
    if CompareText(S, Strings[I]) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function IndexOfTrue(Booleans: array of Boolean): Integer;
var
  I: Integer;
begin
  for I := Low(Booleans) to High(Booleans) do
  begin
    if Booleans[I] then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

{ TSearchStringsMainForm }

procedure TSearchStringsMainForm.BreakButtonClick(Sender: TObject);
begin
  (Sender as TButton).Enabled := False;
  fSearchClass.Terminated := True;
end;

procedure TSearchStringsMainForm.ResultSaveButtonClick(Sender: TObject);
begin
  if ResultSaveDialog.Execute then
    ResultMemo.Lines.SaveToFile(ResultSaveDialog.FileName);
end;

procedure TSearchStringsMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

function TSearchStringsMainForm.ConfigFileName(bLoad: Boolean): String;
begin
  Result := GetAppConfigFile('schstr.ini',
    GetAppDataDir(AppDataSubFolder, IfThen(bLoad, 'schstr.ini', '')));
end;

procedure TSearchStringsMainForm.MsgWarning(const sMessage, sTitle: String);
begin
  MessageBox(Handle, PChar(sMessage), PChar(sTitle), MB_ICONEXCLAMATION);
end;

procedure TSearchStringsMainForm.DoOnProcessed(Sender: TObject;
  nProcessed, nFound: Integer);
begin
  SetStatus(IntToStr(nProcessed)+' processed, '+IntToStr(nFound)+' found.');
end;

procedure TSearchStringsMainForm.SetStatus(const sStatus: String);
begin
  TimeLabel.Caption := sStatus;
  Application.ProcessMessages;
end;

procedure TSearchStringsMainForm.ExploreToFile(const sFileName: String);
begin
  if Length(sFileName) = 0 then Exit;
  ShellExecute(Application.Handle, 'open', 'explorer.exe',
    PChar('/select,"' + sFileName + '"'), nil, SW_NORMAL);
end;

procedure TSearchStringsMainForm.FormatList(lstIn, lstOut: TStrings; const StringFormat: String);
var
  I: Integer;
  S: String;
begin
  lstOut.Clear;
  for I := 0 to lstIn.Count-1 do
  begin
    S := StringReplace(StringFormat, '%s', lstIn[I], [rfReplaceAll, rfIgnoreCase]);
    lstOut.AddObject(S, lstIn.Objects[I]);
  end;
end;

procedure TSearchStringsMainForm.FormCreate(Sender: TObject);
begin
  Caption := Caption + ' ' + APP_VERSION;
  //
  fSearchClass := TSearchClass.Create;
  fSearchClass.OnProcessed := DoOnProcessed;
  //
{$IFDEF DEBUG}
  //SearchMethodGroupBox.Visible := True;
  SelfCheckRadioButton.Visible := True;
  TailCharsCheckBox.Visible := SearchTreeRadioButton.Checked;
{$ENDIF}
  //
  SourcePageControl.ActivePage := SourceListTabSheet;
  ResultPageControl.ActivePage := ResultListTabSheet;
  //
  LoadSettings;
end;

procedure TSearchStringsMainForm.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  //
  fSearchClass.Free;
end;

procedure TSearchStringsMainForm.FormShow(Sender: TObject);
var
  y, k: Integer;
begin
  y := YearOf(Now);
  if y > 2015 then
    CopyRightLabel.Caption := Format(CopyRightLabel.Caption, [y-2000])
  else begin
    k := Pos('-', CopyRightLabel.Caption);
    if k > 0 then CopyRightLabel.Caption := LeftStr(CopyRightLabel.Caption, k-1);
  end;
end;

procedure TSearchStringsMainForm.LinkLabelClick(Sender: TObject);
begin
  ShellExecute(GetDesktopWindow(), 'open',
    PChar('http://optinsoft.net/'),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TSearchStringsMainForm.LoadMemo(lst: TStrings; Memo: TMemo;
  const SName: String);
var
  tmp: TStringList;
  i, j, n: Integer;
  s, t: String;
begin
  tmp := TStringList.Create;
  try
    n := StrToIntDef(lst.Values[SName+'N'], -1);
    j := -1;
    for i := 1 to n do
    begin
      t := SName+IntToStr(i);
      if (j < 0) or (j >= lst.Count-1) then
        j := lst.IndexOfName(t)
      else if AnsiCompareText(lst.Names[j+1], t) = 0 then
        j := j+1
      else
        j := lst.IndexOfName(t);
      if j < 0 then Continue;
      s := lst.ValueFromIndex[j];
      if Length(s) = 0 then Continue;
      tmp.Add(s);
    end;
    Memo.Text := tmp.Text;
  finally
    tmp.Free;
  end;
end;

procedure TSearchStringsMainForm.LoadSettings;
var
  sFileName: String;
  lst: TStringList;
  I: Integer;
begin
  sFileName := ConfigFileName(True);
  if not FileExists(sFileName) then Exit;
  lst := TStringList.Create;
  try
    lst.LoadFromFile(sFileName);
    case IndexOfText(lst.Values['SearchMethod'],
{$IFDEF DEBUG}
      ['SearchList', 'SearchTree', 'SelfCheck'])
{$ELSE}
      ['SearchList', 'SearchTree'])
{$ENDIF}
    of
      0: SearchListRadioButton.Checked := True;
      1: SearchTreeRadioButton.Checked := True;
      2: SelfCheckRadioButton.Checked := True;
    end;
{$IFDEF DEBUG}
    TailCharsCheckBox.Checked := (lst.Values['TailChars'] <> '0');
{$ENDIF}
    SortedListCheckBox.Checked := (lst.Values['SortedList'] = '1');
    SourceFileEdit.Text := lst.Values['SourceFile'];
    if lst.Values['Source'] = 'file' then
      SourcePageControl.ActivePage := SourceFileTabSheet;
    ResultFileEdit.Text := lst.Values['ResultFile'];
    if lst.Values['Result'] = 'file' then
      ResultPageControl.ActivePage := ResultFileTabSheet;
    case IndexOfText(lst.Values['MatchType'],
      ['Contains', 'SpecialChars', 'ExactMatch', 'StartsWith', 'EndsWith',
       'Equal', 'Less', 'LessOrEqual', 'More', 'MoreOrEqual'])
    of
      0: MatchTypeComboBox.ItemIndex := 0;
      1: MatchTypeComboBox.ItemIndex := 1;
      2: MatchTypeComboBox.ItemIndex := 2;
      3: MatchTypeComboBox.ItemIndex := 3;
      4: MatchTypeComboBox.ItemIndex := 4;
      5: MatchTypeComboBox.ItemIndex := 5;
      6: MatchTypeComboBox.ItemIndex := 6;
      7: MatchTypeComboBox.ItemIndex := 7;
      8: MatchTypeComboBox.ItemIndex := 8;
      9: MatchTypeComboBox.ItemIndex := 9;
    end;
    CaseSensitiveCheckBox.Checked := (lst.Values['CaseSensitive'] = '1');
    SearchAllCheckBox.Checked := (lst.Values['SearchAll'] = '1');
    I := lst.IndexOfName('SearchFormat');
    if I >= 0 then SearchFormatEdit.Text := lst.ValueFromIndex[I];
    FormatSearchCheckBox.Checked := (lst.Values['FormatSearch'] = '1');
    I := lst.IndexOfName('OutFormat');
    if I >= 0 then OutFormatEdit.Text := lst.ValueFromIndex[I];
    FormatOutCheckBox.Checked := (lst.Values['FormatOut'] = '1');
    LoadMemo(lst, SourceMemo, 'SourceMemo');
    LoadMemo(lst, SearchMemo, 'SearchMemo');
    case IndexOfText(lst.Values['OutFileFormat'], ['UTF8', 'UTF8BOM']) of
      0: OutFileFormatComboBox.ItemIndex := 0;
      1: OutFileFormatComboBox.ItemIndex := 1;
    end;
    case IndexOfText(lst.Values['OutFileEndings'], ['Windows', 'Unix']) of
      0: OutFileEndingsComboBox.ItemIndex := 0;
      1: OutFileEndingsComboBox.ItemIndex := 1;
    end;
  finally
    lst.Free;
  end;
end;

procedure TSearchStringsMainForm.ReadMemo(lst: TStrings; Memo: TMemo);
var
  I: Integer;
  S: String;
begin
  for I := 0 to Memo.Lines.Count-1 do
  begin
    S := Memo.Lines[I];
    if Length(S) = 0 then Continue;
    lst.Add(S);
  end;
end;

procedure TSearchStringsMainForm.ResultBrowseButtonClick(Sender: TObject);
begin
  ResultSaveDialog.FileName := ResultFileEdit.Text;
  if ResultSaveDialog.Execute then
    ResultFileEdit.Text := ResultSaveDialog.FileName;
end;

procedure TSearchStringsMainForm.ResultExploreButtonClick(Sender: TObject);
begin
  ExploreToFile(ResultFileEdit.Text);
end;

procedure TSearchStringsMainForm.ResultMemoChange(Sender: TObject);
begin
  ResultLabel.Caption := 'Result list ('+IntToStr(ResultMemo.Lines.Count)+'):';
end;

procedure TSearchStringsMainForm.SaveMemo(lst: TStrings; Memo: TMemo;
  const SName: String);
var
  bExists: Boolean;
  i, n, ln: Integer;
  s, t: String;
begin
  n := 0;
  ln := Length(SName);
  bExists := (ln = 0);
  if not bExists then
  begin
    for i := 0 to lst.Count-1 do
    begin
      if AnsiCompareText(LeftStr(lst.Names[i], ln), SName) = 0 then
      begin
        bExists := True;
        Break;
      end;
    end;
  end;
  for i := 0 to Memo.Lines.Count-1 do
  begin
    s := Memo.Lines[i];
    if Length(s) = 0 then Continue;
    Inc(n);
    t := SName+IntToStr(n);
    if bExists then
      lst.Values[t] := s
    else
      lst.Add(t+'='+s);
  end;
  t := SName+'N';
  if bExists then
    lst.Values[t] := IntToStr(n)
  else
    lst.Add(t+'='+IntToStr(n));
end;

procedure TSearchStringsMainForm.SaveSettings;
var
  sFileName: String;
  lst: TStringList;
begin
  sFileName := ConfigFileName(False);
  lst := TStringList.Create;
  try
    case IndexOfTrue(
{$IFDEF DEBUG}
      [SearchListRadioButton.Checked, SearchTreeRadioButton.Checked, SelfCheckRadioButton.Checked])
{$ELSE}
      [SearchListRadioButton.Checked, SearchTreeRadioButton.Checked])
{$ENDIF}
    of
      0: lst.Values['SearchMethod'] := 'SearchList';
      1: lst.Values['SearchMethod'] := 'SearchTree';
      2: lst.Values['SearchMethod'] := 'SelfCheck';
    end;
{$IFDEF DEBUG}
    lst.Values['TailChars'] := IfThen(TailCharsCheckBox.Checked, '1', '0');
{$ENDIF}
    lst.Values['SortedList'] := IfThen(SortedListCheckBox.Checked, '1', '0');
    lst.Values['SourceFile'] := SourceFileEdit.Text;
    lst.Values['Source'] :=
      IfThen(SourcePageControl.ActivePage = SourceFileTabSheet, 'file', 'list');
    lst.Values['ResultFile'] := ResultFileEdit.Text;
    lst.Values['Result'] :=
      IfThen(ResultPageControl.ActivePage = ResultFileTabSheet, 'file', 'list');
    case MatchTypeComboBox.ItemIndex of
      0: lst.Values['MatchType'] := 'Contains';
      1: lst.Values['MatchType'] := 'SpecialChars';
      2: lst.Values['MatchType'] := 'ExactMatch';
      3: lst.Values['MatchType'] := 'StartsWith';
      4: lst.Values['MatchType'] := 'EndsWith';
      5: lst.Values['MatchType'] := 'Equal';
      6: lst.Values['MatchType'] := 'Less';
      7: lst.Values['MatchType'] := 'LessOrEqual';
      8: lst.Values['MatchType'] := 'More';
      9: lst.Values['MatchType'] := 'MoreOrEqual';
    end;
    lst.Values['CaseSensitive'] := IfThen(CaseSensitiveCheckBox.Checked, '1', '0');
    lst.Values['SearchAll'] := IfThen(SearchAllCheckBox.Checked, '1', '0');
    lst.Values['SearchFormat'] := SearchFormatEdit.Text;
    lst.Values['FormatSearch'] := IfThen(FormatSearchCheckBox.Checked, '1', '0');
    lst.Values['OutFormat'] := OutFormatEdit.Text;
    lst.Values['FormatOut'] := IfThen(FormatOutCheckBox.Checked, '1', '0');
    SaveMemo(lst, SourceMemo, 'SourceMemo');
    SaveMemo(lst, SearchMemo, 'SearchMemo');
    case OutFileFormatComboBox.ItemIndex of
      0: lst.Values['OutFileFormat'] := 'UTF8';
      1: lst.Values['OutFileFormat'] := 'UTF8BOM';
    end;
    case OutFileEndingsComboBox.ItemIndex of
      0: lst.Values['OutFileEndings'] := 'Windows';
      1: lst.Values['OutFileEndings'] := 'Unix';
    end;
    lst.SaveToFile(sFileName);
  finally
    lst.Free;
  end;
end;

procedure TSearchStringsMainForm.SearchButtonClick(Sender: TObject);
var
  lstSource, lstSearch, lstFormattedSearch, lstResult: TStringList;
  t0, t: TDateTime;
  sm: TSearchMethod;
  st1, st2: TSearcher;
  smt: TSearchMatchType;
  srcLines: TLineReader;
  outLines: TLineWriter;
  N: Integer;
  bUseTChars: Boolean;
  bSortedList: Boolean;
  bCaseSensitive: Boolean;
  bSearchAll: Boolean;
begin
  SaveSettings;
  //
  ResultMemo.Text := '';
  CompletedLabel.Visible := False;
  SetStatus('0 sec.');
  //
  if not SearchMethodGroupBox.Visible then
    sm := smSearchTree
  else begin
    case IndexOfTrue(
{$IFDEF DEBUG}
      [SearchListRadioButton.Checked, SearchTreeRadioButton.Checked, SelfCheckRadioButton.Checked])
{$ELSE}
      [SearchListRadioButton.Checked, SearchTreeRadioButton.Checked])
{$ENDIF}
    of
      0: sm := smSearchList;
      1: sm := smSearchTree;
      2: sm := smSelfCheck;
    else
      Exit;
    end;
  end;
{$IFDEF DEBUG}
  bUseTChars := TailCharsCheckBox.Checked;
{$ELSE}
  bUseTChars := DefUseTChars;
{$ENDIF}
  bSortedList := SortedListCheckBox.Checked;
  fSearchClass.Terminated := False;
  lstSource := nil;
  try
    if SourcePageControl.ActivePage = SourceFileTabSheet then
    begin
      if SourceFileEdit.Text = '' then
      begin
        MsgWarning('Please, enter Source File');
        Exit;
      end;
      srcLines := TLineReader.Create(SourceFileEdit.Text);
    end else
    begin
      lstSource := TStringList.Create;
      srcLines := TLineReader.Create(lstSource);
    end;
  except
    if Assigned(lstSource) then
      lstSource.Free;
    raise;
  end;
  lstResult := nil;
  try
    if ResultPageControl.ActivePage = ResultFileTabSheet then
    begin
      if ResultFileEdit.Text = '' then
      begin
        MsgWarning('Please, enter Result File');
        if Assigned(srcLines) then
          FreeAndNil(srcLines);
        if Assigned(lstSource) then
          FreeAndNil(lstSource);
        Exit;
      end;
      outLines := TLineWriter.Create(ResultFileEdit.Text);
      outLines.WriteBOM := (OutFileFormatComboBox.ItemIndex = 1);
      if OutFileEndingsComboBox.ItemIndex = 1 then
        outLines.LineBreak := #10;
    end else
    begin
      lstResult := TStringList.Create;
      outLines := TLineWriter.Create(lstResult);
    end;
  except
    if Assigned(lstResult) then
      lstResult.Free;
    if Assigned(srcLines) then
      srcLines.Free;
    if Assigned(lstSource) then
      lstSource.Free;
    raise;
  end;
  lstSearch := TStringList.Create;
  if FormatSearchCheckBox.Checked then
    lstFormattedSearch := TStringList.Create
  else
    lstFormattedSearch := nil;
  SearchButton.Enabled := False;
  BreakButton.Enabled := True;
  N := 0;
  t0 := 0;
  try
    SetStatus('reading...');
    if Assigned(lstSource) then
      ReadMemo(lstSource, SourceMemo);
    ReadMemo(lstSearch, SearchMemo);
    if Assigned(lstFormattedSearch) then
      FormatList(lstSearch, lstFormattedSearch, SearchFormatEdit.Text);
    fSearchClass.OutFormat := IfThen(FormatOutCheckBox.Checked, OutFormatEdit.Text, '');
    smt := smtContains;
    case MatchTypeComboBox.ItemIndex of
      0: begin smt := smtContains; end;
      1: begin smt := smtSpecialChars; end;
      2: begin smt := smtExactMatch; end;
      3: begin smt := smtStartsWith; end;
      4: begin smt := smtEndsWith; end;
      5: begin smt := smtStrEqual; end;
      6: begin smt := smtStrLess; end;
      7: begin smt := smtStrLessOrEqual; end;
      8: begin smt := smtStrMore; end;
      9: begin smt := smtStrMoreOrEqual; end;
    end;
    bCaseSensitive := CaseSensitiveCheckBox.Checked;
    bSearchAll := SearchAllCheckBox.Checked;
    t0 := Now;
    if (sm = smSearchList) or (sm = smSelfCheck) then
    begin
      st1 := TSearchList.Create(bSortedList);
    end else
      st1 := nil;
    if (sm = smSearchTree) or (sm = smSelfCheck) then
    begin
      st2 := TSearchTree.Create(bUseTChars);
    end else
      st2 := nil;
    if Assigned(st1) or Assigned(st2) then
    begin
      try
        SetStatus('preparing...');
        if Assigned(st1) then
        begin
          st1.MatchType := smt;
          st1.CaseSensitive := bCaseSensitive;
          if Assigned(lstFormattedSearch) then
            st1.Build(lstFormattedSearch)
          else
            st1.Build(lstSearch)
        end;
        if Assigned(st2) then
        begin
          st2.MatchType := smt;
          st2.CaseSensitive := bCaseSensitive;
          if Assigned(lstFormattedSearch) then
            st2.Build(lstFormattedSearch)
          else
            st2.Build(lstSearch);
        end;
        SetStatus('searching...');
        N := fSearchClass.DoSearch(st1, st2, bSearchAll,
          srcLines, lstSearch, lstFormattedSearch, outLines);
      finally
        if Assigned(st1) then
          st1.Free;
        if Assigned(st2) then
          st2.Free;
      end;
    end;
  finally
    if t0 <> 0 then t := Now else t := 0;
    if Assigned(lstResult) then
      ResultMemo.Text := lstResult.Text
    else
      CompletedLabel.Visible := True;
    SetStatus(IntToStr(SecondsBetween(t0, t))+' sec. '+IntToStr(N)+' found.');
    SearchButton.Enabled := True;
    BreakButton.Enabled := False;
    if Assigned(lstFormattedSearch) then
      lstFormattedSearch.Free;
    lstSearch.Free;
    if Assigned(outLines) then
      outLines.Free;
    if Assigned(lstResult) then
      lstResult.Free;
    if Assigned(srcLines) then
      srcLines.Free;
    if Assigned(lstSource) then
      lstSource.Free;
  end;
end;

procedure TSearchStringsMainForm.SearchListRadioButtonClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  TailCharsCheckBox.Visible := False;
{$ENDIF}
  SortedListCheckBox.Visible := True;
end;

procedure TSearchStringsMainForm.SearchLoadButtonClick(Sender: TObject);
begin
  if SearchOpenDialog.Execute then
    SearchMemo.Lines.LoadFromFile(SearchOpenDialog.FileName);
end;

procedure TSearchStringsMainForm.SearchMemoChange(Sender: TObject);
begin
  SearchLabel.Caption := 'Search strings ('+IntToStr(SearchMemo.Lines.Count)+'):';
end;

procedure TSearchStringsMainForm.SearchSaveButtonClick(Sender: TObject);
begin
  if SearchSaveDialog.Execute then
    SearchMemo.Lines.SaveToFile(SearchSaveDialog.FileName);
end;

procedure TSearchStringsMainForm.SearchTreeRadioButtonClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  TailCharsCheckBox.Visible := True;
{$ENDIF}
  SortedListCheckBox.Visible := False;
end;

procedure TSearchStringsMainForm.SelfCheckRadioButtonClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  TailCharsCheckBox.Visible := True;
{$ENDIF}
  SortedListCheckBox.Visible := True;
end;

procedure TSearchStringsMainForm.SourceBrowseButtonClick(Sender: TObject);
begin
  SourceOpenDialog.FileName := SourceFileEdit.Text;
  if SourceOpenDialog.Execute then
    SourceFileEdit.Text := SourceOpenDialog.FileName;
end;

procedure TSearchStringsMainForm.SourceExploreButtonClick(Sender: TObject);
begin
  ExploreToFile(SourceFileEdit.Text);
end;

procedure TSearchStringsMainForm.SourceLoadButtonClick(Sender: TObject);
begin
  if SourceOpenDialog.Execute then
    SourceMemo.Lines.LoadFromFile(SourceOpenDialog.FileName);
end;

procedure TSearchStringsMainForm.SourceMemoChange(Sender: TObject);
begin
  SourceLabel.Caption := 'List of strings ('+IntToStr(SourceMemo.Lines.Count)+'):';
end;

procedure TSearchStringsMainForm.SourceSaveButtonClick(Sender: TObject);
begin
  if SourceSaveDialog.Execute then
    SourceMemo.Lines.SaveToFile(SourceSaveDialog.FileName);
end;

end.
