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

unit uSearchStrings;

interface

uses System.SysUtils, System.Classes, uIntList;

const
  DefUseTChars = True;

type
  TSearchMatchType = (
    smtContains, smtSpecialChars, smtExactMatch, smtStartsWith, smtEndsWith,
    smtStrEqual, smtStrLess, smtStrLessOrEqual, smtStrMore, smtStrMoreOrEqual);

  TSearchStrType = (sstEqual, sstLess, sstLessOrEqual, sstMore, sstMoreOrEqual);

  TSearcher = class
  protected
    fMatchType: TSearchMatchType;
    fCaseSensitive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Clear; virtual; abstract;
    procedure Build(lst: TStrings); virtual; abstract;
    function FindMatch(const S: String; out Index: Integer): Integer; virtual; abstract;
    function FindAllMatches(const S: String; Indexes: TIntList): Boolean; virtual; abstract;
    //
    property MatchType: TSearchMatchType read fMatchType write fMatchType;
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
  end;

  TSearchList = class(TSearcher)
  protected
    fList: TStringList;
    fBList: TStringList;
    fIdxArr: TIntArray;
    fSorted: Boolean;
    procedure ClearList; virtual;
    procedure BuildIdxArr(Size: Integer); virtual;
    procedure AddListItem(const S: String; pIndex: PInteger; smt: TSearchMatchType);
    function FindEqual(AList: TStrings; const S: string; out Index: Integer): Boolean;
    function FindLess(AList: TStrings; const S: string; out Index: Integer): Boolean;
    function FindLessOrEqual(AList: TStrings; const S: string; out Index: Integer): Boolean;
    function FindMore(AList: TStrings; const S: string; out Index: Integer): Boolean;
    function FindMoreOrEqual(AList: TStrings; const S: string; out Index: Integer): Boolean;
    function CompareStrings(const S1, S2: String): Integer;
  public
    constructor Create(ASorted: Boolean = False);
    destructor Destroy; override;
    //
    procedure Clear; override;
    procedure Build(lst: TStrings); override;
    function FindMatch(const S: String; out Index: Integer): Integer; override;
    function FindAllMatches(const S: String; Indexes: TIntList): Boolean; override;
    //
    function ListFindMatches(const S: String; out Index: Integer; Indexes: TIntList): Integer;
    function ListFindStr(const S: String; SearchStrType: TSearchStrType; out Index: Integer; Indexes: TIntList): Boolean;
    //
    property Sorted: Boolean read fSorted write fSorted;
  end;

  TSearchTree = class(TSearchList)
  protected
    fNextIdxArr: TIntArray;
    fNodes: Pointer;
    fNCount: Cardinal;
    fNSize: Cardinal;
    fTChars: PChar;
    fTCount: Cardinal;
    fTSize: Cardinal;
    fTrees: Pointer;
    fBTrees: Pointer;
    fUseTChars: Boolean;
    procedure ClearList; override;
    procedure BuildIdxArr(Size: Integer); override;
    procedure ReleaseTrees;
    function AllocTreeNode(C: Char): Integer;
    function WriteTailChars(const S: String; K: Integer): Cardinal;
  public
    constructor Create(AUseTChars: Boolean = DefUseTChars);
    destructor Destroy; override;
    //
    procedure Clear; override;
    procedure Build(lst: TStrings); override;
    function FindMatch(const S: String; out Index: Integer): Integer; override;
    function FindAllMatches(const S: String; Indexes: TIntList): Boolean; override;
    //
    function TreeFindMatches(const S: String; out Index: Integer; Indexes: TIntList): Integer;
    function GetNextIndex(Index: Integer): Integer;
    //
    property NodesCount: Cardinal read fNCount;
    property TailCharsCount: Cardinal read fTCount;
    property Sorted: Boolean read fSorted;
  end;

implementation

uses uStrUtils;

{ TSearcher }

constructor TSearcher.Create;
begin
  fMatchType := smtContains;
  fCaseSensitive := False;
end;

destructor TSearcher.Destroy;
begin

  inherited;
end;

{ TSearchList }

procedure TSearchList.AddListItem(const S: String; pIndex: PInteger;
  smt: TSearchMatchType);
var
  L: Integer;
begin
  if Length(S) = 0 then Exit;
  if smt = smtSpecialChars then
  begin
    L := Length(S);
    if S[1] = '^' then
    begin
      if S[L] = '$' then
        fBList.AddObject(Copy(S, 2, L-2)+#10, TObject(pIndex))
      else
        fBList.AddObject(Copy(S, 2, L-1), TObject(pIndex));
    end else
    begin
      if S[L] = '$' then
        fList.AddObject(Copy(S, 1, L-1)+#10, TObject(pIndex))
      else
        fList.AddObject(S, TObject(pIndex));
    end;
    Exit;
  end;
  case smt of
    smtContains:   fList.AddObject(S, TObject(pIndex));
    smtExactMatch: fBList.AddObject(S+#10, TObject(pIndex));
    smtStartsWith: fBList.AddObject(S, TObject(pIndex));
    smtEndsWith:   fList.AddObject(S+#10, TObject(pIndex));
  else
                   fBList.AddObject(S+#10, TObject(pIndex));
  end;
end;

function SortCompareStr(L: TStringList; Item1, Item2: Integer): Integer;
begin
  Result := CompareStr(L[Item1], L[Item2]);
end;

procedure TSearchList.Build(lst: TStrings);
var
  I: Integer;
  S: String;
  bCaseInsensitive: Boolean;
begin
  ClearList;

  if lst.Count = 0 then Exit;

  BuildIdxArr(lst.Count);

  bCaseInsensitive := not fCaseSensitive;
  for I := 0 to lst.Count-1 do
  begin
    S := lst[I];
    if Length(S) = 0 then Continue;
    if bCaseInsensitive then S := UpperCase(S);
    AddListItem(S, @fIdxArr[I], fMatchType);
  end;

  if fSorted then
  begin
    fBList.CustomSort(@SortCompareStr);
    fList.CustomSort(@SortCompareStr);
  end;
end;

procedure TSearchList.BuildIdxArr(Size: Integer);
var
  I: Integer;
begin
  SetLength(fIdxArr, Size);
  for I := 0 to Size-1 do
    fIdxArr[I] := I;
end;

procedure TSearchList.Clear;
begin
  ClearList;
end;

procedure TSearchList.ClearList;
begin
  fList.Clear;
  fBList.Clear;
  SetLength(fIdxArr, 0);
end;

function TSearchList.CompareStrings(const S1, S2: String): Integer;
var
  L1, L2: Integer;
begin
  L1 := Length(S1);
  L2 := Length(S2);
//  if (L1 > 0)  and (S1[L1] = #10) then Dec(L1);
//  if (L2 > 0)  and (S2[L2] = #10) then Dec(L2);
  if fCaseSensitive then
    Result := SensCompare(S1, S2, L1, L2)
  else
    Result := InsensCompare(S1, S2, L1, L2);
end;

constructor TSearchList.Create(ASorted: Boolean);
begin
  inherited Create;

  fList := TStringList.Create;
  fBList := TStringList.Create;

  fSorted := ASorted;
end;

destructor TSearchList.Destroy;
begin
  ClearList;

  fList.Free;
  fBList.Free;

  inherited;
end;

function TSearchList.FindAllMatches(const S: String; Indexes: TIntList): Boolean;
var
  I: Integer;
begin
  case fMatchType of
    smtStrEqual:       Result := ListFindStr(S, sstEqual, I, Indexes);
    smtStrLess:        Result := ListFindStr(S, sstLess, I, Indexes);
    smtStrLessOrEqual: Result := ListFindStr(S, sstLessOrEqual, I, Indexes);
    smtStrMore:        Result := ListFindStr(S, sstMore, I, Indexes);
    smtStrMoreOrEqual: Result := ListFindStr(S, sstMoreOrEqual, I, Indexes);
  else
    Result := (ListFindMatches(S, I, Indexes) > 0);
    Exit;
  end;
end;

function TSearchList.FindEqual(AList: TStrings; const S: string;
  out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(AList[I], S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;

function TSearchList.FindLess(AList: TStrings; const S: string;
  out Index: Integer): Boolean;
var
  I, L, H, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(AList[I], S);
    if C >= 0 then H := I - 1
    else // if C < 0
    begin
      L := I + 1;
      Result := True;
    end;
  end;
  Index := H;
end;

function TSearchList.FindLessOrEqual(AList: TStrings; const S: string;
  out Index: Integer): Boolean;
var
  I, L, H, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(AList[I], S);
    if C > 0 then H := I - 1
    else // if C <= 0
    begin
      L := I + 1;
      Result := True;
    end;
  end;
  Index := H;
end;

function TSearchList.FindMore(AList: TStrings; const S: string;
  out Index: Integer): Boolean;
var
  I, L, H, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(AList[I], S);
    if C <= 0 then L := I + 1
    else // if C > 0
    begin
      H := I - 1;
      Result := True;
    end;
  end;
  Index := L;
end;

function TSearchList.FindMoreOrEqual(AList: TStrings; const S: string;
  out Index: Integer): Boolean;
var
  I, L, H, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(AList[I], S);
    if C < 0 then L := I + 1
    else // if C >= 0
    begin
      H := I - 1;
      Result := True;
    end;
  end;
  Index := L;
end;

function TSearchList.FindMatch(const S: String; out Index: Integer): Integer;
var
  bFound: Boolean;
begin
  case fMatchType of
    smtStrEqual:       bFound := ListFindStr(S, sstEqual, Index, nil);
    smtStrLess:        bFound := ListFindStr(S, sstLess, Index, nil);
    smtStrLessOrEqual: bFound := ListFindStr(S, sstLessOrEqual, Index, nil);
    smtStrMore:        bFound := ListFindStr(S, sstMore, Index, nil);
    smtStrMoreOrEqual: bFound := ListFindStr(S, sstMoreOrEqual, Index, nil);
  else
    Result := ListFindMatches(S, Index, nil);
    Exit;
  end;
  if bFound then
    Result := 1
  else
    Result := 0;
end;

function TSearchList.ListFindStr(const S: String; SearchStrType: TSearchStrType;
  out Index: Integer; Indexes: TIntList): Boolean;

var
  T: String;
  bFindAll: Boolean;

  function FindSorted(AList: TStrings): Boolean;
  var
    I, DI, N: Integer;
    pIdx: PInteger;
  begin
    case SearchStrType of
      sstEqual:       begin Result := FindEqual(AList, T, I); end;
      sstLess:        begin Result := FindLess(AList, T, I); end;
      sstLessOrEqual: begin Result := FindLessOrEqual(AList, T, I); end;
      sstMore:        begin Result := FindMore(AList, T, I); end;
      sstMoreOrEqual: begin Result := FindMoreOrEqual(AList, T, I); end;
    else
      Result := False;
      Exit;
    end;
    if Result then
    begin
      N := AList.Count;
      if (SearchStrType = sstLess) or
         (SearchStrType = sstLessOrEqual)
      then
        DI := -1
      else
        DI := 1;
      while (I >= 0) and (I < N) do
      begin
        pIdx := PInteger(AList.Objects[I]);
        Index := pIdx^;
        if not bFindAll then Exit;
        if Index >= 0 then Indexes.Add(Index);
        Inc(I, DI);
        if (SearchStrType = sstEqual) and (I >= 0) and (I < N) then
        begin
          if CompareStrings(AList[I], T) <> 0
            then Break;
        end;
      end;
    end;
  end;

  function FindUnsorted(AList: TStrings): Boolean;
  var
    I, L, N: Integer;
    pIdx: PInteger;
    fCompare: function (const S1, S2: String; Len: Integer): Boolean;
  begin
    Result := False;
    if fCaseSensitive then
    begin
      case SearchStrType of
        sstEqual:       fCompare := @SensEquals;
        sstLess:        fCompare := @SensLessThan;
        sstLessOrEqual: fCompare := @SensLessOrEquals;
        sstMore:        fCompare := @SensMoreThan;
        sstMoreOrEqual: fCompare := @SensMoreOrEquals;
      else
        Exit;
      end;
    end else
    begin
      case SearchStrType of
        sstEqual:       fCompare := @InsensEquals;
        sstLess:        fCompare := @InsensLessThan;
        sstLessOrEqual: fCompare := @InsensLessOrEquals;
        sstMore:        fCompare := @InsensMoreThan;
        sstMoreOrEqual: fCompare := @InsensMoreOrEquals;
      else
        Exit;
      end;
    end;
    I := 0;
    N := AList.Count;
    while I < N do
    begin
      L := Length(AList[I]);
//      if (L > 0) and (AList[I][L] = #10) then Dec(L);
      if fCompare(AList[I], T, L) then
      begin
        Result := True;
        pIdx := PInteger(AList.Objects[I]);
        Index := pIdx^;
        if not bFindAll then Exit;
        if Index >= 0 then Indexes.Add(Index);
      end;
      Inc(I);
    end;
  end;

var
  L: Integer;
begin
  bFindAll := Assigned(Indexes);
  if bFindAll then Indexes.Clear;
  Index := -1;
  Result := False;
  L := Length(S);
  if L = 0 then Exit;
  if S[L] <> #10 then
  begin
    if S[L] = #13 then
    begin
      if L < 2 then Exit;
      T := Copy(S, 1, L-1) + #10;
    end else
      T := S + #10;
  end else //if S[L] = #10
  begin
    if L < 2 then Exit;
    if S[L-1] = #13 then
      T := Copy(S, 1, L-2) + #10
    else
      T := S;
  end;
  if fSorted then
  begin
    if FindSorted(fBList) then
    begin
      Result := True;
      if not bFindAll then Exit;
    end;
    if FindSorted(fList) then
      Result := True;
  end else
  begin
    if FindUnsorted(fBList) then
    begin
      Result := True;
      if not bFindAll then Exit;
    end;
    if FindUnsorted(fList) then
      Result := True;
  end;
end;

function TSearchList.ListFindMatches(const S: String; out Index: Integer;
  Indexes: TIntList): Integer;
var
  K, I, L: Integer;
  pIdx: PInteger;
  T: String;
  bFindAll: Boolean;
  fPosEx: function (const SubStr, S: string; Offset, Len: Integer): Integer;
begin
  bFindAll := Assigned(Indexes);
  if bFindAll then Indexes.Clear;
  Index := -1;
  Result := 0;
  L := Length(S);
  if L = 0 then Exit;
  if S[L] <> #10 then
  begin
    if S[L] = #13 then
    begin
      if L < 2 then Exit;
      T := Copy(S, 1, L-1) + #10;
    end else
      T := S + #10;
  end else //if S[L] = #10
  begin
    if L < 2 then Exit;
    if S[L-1] = #13 then
      T := Copy(S, 1, L-2) + #10
    else
      T := S;
  end;
  if fCaseSensitive then
    fPosEx := @SensPosEx
  else
    fPosEx := @InsensPosEx;
  for I := 0 to fBList.Count-1 do
  begin
    K := fPosEx(fBList[I], T, 1, Length(fBList[I]));
    if K > 0 then
    begin
      Result := K;
      pIdx := PInteger(fBList.Objects[I]);
      Index := pIdx^;
      if not bFindAll then Exit;
      if Index >= 0 then Indexes.Add(Index);
    end;
  end;
  for I := 0 to fList.Count-1 do
  begin
    K := fPosEx(fList[I], T, 1, MaxInt);
    if K > 0 then
    begin
      if bFindAll or (Result < 1) or (K < Result) then
      begin
        Result := K;
        pIdx := PInteger(fList.Objects[I]);
        Index := pIdx^;
        //if not bFindAll then Exit;
        if bFindAll then
          if Index >= 0 then Indexes.Add(Index);
      end;
    end;
  end;
end;

{ TSearchTree }

const
  CN_CHARS_FLAG = $80000000;
  CN_CHARS_MASK = $7fffffff;

type
  PTreeNode = ^TTreeNode;
  TTreeNode = packed record
    Ch: Char;
    Children: Cardinal;
    Index: Integer;
  end;

  PTreeNodeArray = ^TTreeNodeArray;
  TTreeNodeArray = array [0..0] of TTreeNode;
  //TTreeNodeArray = array [0..32767] of TTreeNode;

  PTreeCharArray = ^TTreeCharArray;
  TTreeCharArray = array [Char] of Integer;

function TSearchTree.AllocTreeNode(C: Char): Integer;
begin
  if FNCount >= FNSize then
  begin
    if FNSize < 16 then
      FNSize := FNSize+1
    else if FNSize < 1024 then
      FNSize := FNSize * 2
    else
      FNSize := FNSize + 1024;
    ReallocMem(fNodes, FNSize * sizeof(TTreeNode));
  end;
  Result := FNCount;
  Inc(FNCount);
  with PTreeNodeArray(fNodes)^[Result] do
  begin
    Ch := C;
    Children := 0;
    Index := -1;
  end;
end;

function TSearchTree.WriteTailChars(const S: String; K: Integer): Cardinal;
var
  I, L: Integer;
  TChars, Delta: Cardinal;
begin
  L := Length(S);
  if K <= L then
    TChars := L+1-K
  else
    TChars := 0;
  if FTCount+TChars >= FTSize then
  begin
    repeat
      if FTSize > 64 then
        Delta := FTSize div 4
      else
        if FTSize > 8 then
          Delta := 16
      else
        Delta := 4;
      Inc(FTSize, Delta);
    until FTCount+TChars < FTSize;
    ReallocMem(fTChars, FTSize * sizeof(Char));
  end;
  Result := FTCount;
  Inc(FTCount, TChars+1);
  I := Result;
  while K <= L do
  begin
    fTChars[I] := S[K];
    Inc(I);
    Inc(K);
  end;
  fTChars[I] := #0;
end;

procedure TSearchTree.Build(lst: TStrings);

  procedure BuildListTreeNode(List: TStringList; P, I1, I2, K: Integer);
  var
    I, I0, N, N0, N1: Integer;
    Ch, Ch0: Char;
  begin
    N0 := -1;
    N1 := -1;
    if Length(List[I1]) < K then
    begin
      if I1 = I2 then
      begin
        with PTreeNodeArray(fNodes)^[P] do
          Index := I1;
        Exit;
      end;
      N := AllocTreeNode(#0);
      N0 := N;
      with PTreeNodeArray(fNodes)^[N] do
        Index := I1;
      Inc(I1);
    end else
    if (I1 = I2) and fUseTChars then
    begin
      with PTreeNodeArray(fNodes)^[P] do
      begin
        Children := CN_CHARS_FLAG or WriteTailChars(List[I1], K);
        Index := I1;
      end;
      Exit;
    end;
    //N := -1;
    I := I1;
    Ch0 := List[I][K];
    repeat
      if I = I2 then
      begin
        N := AllocTreeNode(Ch0);
        if N0 < 0 then N0 := N;
        if N1 < 0 then N1 := N;
        Break;
      end;
      Inc(I);
      Ch := List[I][K];
      if Ch = Ch0 then Continue;
      N := AllocTreeNode(Ch0);
      if N0 < 0 then N0 := N;
      if N1 < 0 then N1 := N;
      Ch0 := Ch;
    until false;
    with PTreeNodeArray(fNodes)^[P] do
    begin
      Children := N+1-N0;
      Index := N0;
    end;
    N := N1;
    I := I1;
    Ch0 := List[I][K];
    I0 := I1;
    repeat
      if I = I2 then
      begin
        BuildListTreeNode(List, N, I0, I, K+1);
        Break;
      end;
      Inc(I);
      Ch := List[I][K];
      if Ch = Ch0 then Continue;
      BuildListTreeNode(List, N, I0, I-1, K+1);
      Inc(N);
      I0 := I;
      Ch0 := Ch;
    until false;
  end;

  procedure BuildListTrees(List: TStringList; Trees: PTreeCharArray);
  var
    K, I, I0, N: Integer;
    Ch, Ch0: Char;
  begin
    K := 1;
    N := List.Count;
    if N = 0 then Exit;
    I0 := 0;
    if Length(List[I0]) < 1 then
    begin
      if I0 = N-1 then Exit;
      Inc(I0);
    end;
    Ch0 := List[I0][K];
    I := I0;
    repeat
      if I = N-1 then
      begin
        Trees^[Ch0] := AllocTreeNode(Ch0);
        BuildListTreeNode(List, Trees^[Ch0], I0, I, K+1);
        Break;
      end;
      Inc(I);
      Ch := List[I][K];
      if Ch = Ch0 then Continue;
      Trees^[Ch0] := AllocTreeNode(Ch0);
      BuildListTreeNode(List, Trees^[Ch0], I0, I-1, K+1);
      I0 := I;
      Ch0 := Ch;
    until false;
  end;

var
  I: Integer;
  S: String;
  tmp: TStringList;
  pIdx, pPrevIdx: PInteger;
  bCaseInsensitive: Boolean;
begin
  ReleaseTrees;
  ClearList;

  if lst.Count = 0 then Exit;

  BuildIdxArr(lst.Count);

  tmp := TStringList.Create;
  try
    bCaseInsensitive := not fCaseSensitive;
    for I := 0 to lst.Count-1 do
    begin
      S := lst[I];
      if Length(S) = 0 then Continue;
      if bCaseInsensitive then S := UpperCase(S);
      tmp.AddObject(S, TObject(@fIdxArr[I]));
    end;
    //tmp.Duplicates := dupIgnore;
    //tmp.CaseSensitive := False;
    //tmp.Sort;
    tmp.CustomSort(@SortCompareStr);
    if tmp.Count = 0 then Exit;
    pIdx := PInteger(tmp.Objects[0]);
    AddListItem(tmp[0], pIdx, fMatchType);
    S := tmp[0];
    for I := 1 to tmp.Count-1 do
    begin
      if tmp[I] = S then
      //if Copy(tmp[I], 1, Length(S)) = S then
      begin
        pPrevIdx := pIdx;
        pIdx := PInteger(tmp.Objects[I]);
        fNextIdxArr[pPrevIdx^] := pIdx^;
        Continue;
      end;
      pIdx := PInteger(tmp.Objects[I]);
      AddListItem(tmp[I], pIdx, fMatchType);
      S := tmp[I];
    end;
  finally
    tmp.Free;
  end;

  if fBList.Count > 0 then
  begin
    GetMem(fBTrees, sizeof(TTreeCharArray));
    FillChar(fBTrees^, sizeof(TTreeCharArray), #255);
    BuildListTrees(fBList, fBTrees);
  end;

  if fList.Count > 0 then
  begin
    GetMem(fTrees, sizeof(TTreeCharArray));
    FillChar(fTrees^, sizeof(TTreeCharArray), #255);
    BuildListTrees(fList, fTrees);
  end;

end;

procedure TSearchTree.BuildIdxArr(Size: Integer);
var
  I: Integer;
begin
  inherited BuildIdxArr(Size);

  SetLength(FNextIdxArr, Size);
  for I := 0 to Size-1 do
    FNextIdxArr[I] := -1;
end;

procedure TSearchTree.Clear;
begin
  ReleaseTrees;
  ClearList;
end;

procedure TSearchTree.ClearList;
begin
  inherited ClearList;

  SetLength(fNextIdxArr, 0);
end;

constructor TSearchTree.Create(AUseTChars: Boolean);
begin
  inherited Create(True);

  fNodes := nil;
  fNCount := 0;
  fNSize := 0;
  fTChars := nil;
  fTCount := 0;
  fTSize := 0;
  fTrees := nil;
  fBTrees := nil;
  fUseTChars := AUseTChars;
end;

destructor TSearchTree.Destroy;
begin
  ReleaseTrees;

  inherited;
end;

function TSearchTree.FindAllMatches(const S: String; Indexes: TIntList): Boolean;
var
  I, J, N: Integer;
begin
  case fMatchType of
    smtStrEqual:       Result := ListFindStr(S, sstEqual, I, Indexes);
    smtStrLess:        Result := ListFindStr(S, sstLess, I, Indexes);
    smtStrLessOrEqual: Result := ListFindStr(S, sstLessOrEqual, I, Indexes);
    smtStrMore:        Result := ListFindStr(S, sstMore, I, Indexes);
    smtStrMoreOrEqual: Result := ListFindStr(S, sstMoreOrEqual, I, Indexes);
  else
    Result := (TreeFindMatches(S, I, Indexes) > 0);
    Exit;
  end;
  if Result and Assigned(Indexes) then
  begin
    N := Indexes.Count;
    for I := 0 to N-1 do
    begin
      J := fNextIdxArr[Indexes[I]];
      while J >= 0 do
      begin
        Indexes.Add(J);
        J := fNextIdxArr[J];
      end;
    end;
  end;
end;

function TSearchTree.FindMatch(const S: String; out Index: Integer): Integer;
var
  bFound: Boolean;
begin
  case fMatchType of
    smtStrEqual:       bFound := ListFindStr(S, sstEqual, Index, nil);
    smtStrLess:        bFound := ListFindStr(S, sstLess, Index, nil);
    smtStrLessOrEqual: bFound := ListFindStr(S, sstLessOrEqual, Index, nil);
    smtStrMore:        bFound := ListFindStr(S, sstMore, Index, nil);
    smtStrMoreOrEqual: bFound := ListFindStr(S, sstMoreOrEqual, Index, nil);
  else
    Result := TreeFindMatches(S, Index, nil);
    Exit;
  end;
  if bFound then
    Result := 1
  else
    Result := 0;
end;

function TSearchTree.GetNextIndex(Index: Integer): Integer;
begin
  Result := fNextIdxArr[Index];
end;

procedure TSearchTree.ReleaseTrees;
begin
  if fNodes <> nil then
  begin
    FreeMem(fNodes);
    fNodes := nil;
  end;
  FNCount := 0;
  FNSize := 0;
  if fTChars <> nil then
  begin
    FreeMem(fTChars);
    fTChars := nil;
  end;
  fTCount := 0;
  fTSize := 0;
  if Assigned(fTrees) then
  begin
    FreeMem(fTrees);
    fTrees := nil;
  end;
  if Assigned(fBTrees) then
  begin
    FreeMem(fBTrees);
    fBTrees := nil;
  end;
end;

function TSearchTree.TreeFindMatches(const S: String; out Index: Integer;
  Indexes: TIntList): Integer;
var
  L: Integer;
  T: String;
  bCaseInsensitive: Boolean;
  bFindAll: Boolean;
  pIdx: PInteger;

  procedure OutIndex(J: Integer);
  begin
    Index := J;
    if bFindAll then
    begin
      while J >= 0 do
      begin
        Indexes.AddSorted(J, True);
        J := fNextIdxArr[J];
      end;
    end;
  end;

  function ProcessTreeNode(Node: PTreeNode; List: TStrings; K: Integer): Integer;
  var
    I: Integer;
    CN: Cardinal;
    Ch: Char;
    P: PChar;
  begin
    Result := -1;
    repeat
      CN := Node^.Children;
      if CN = 0 then
      begin
        Result := Node^.Index;
        pIdx := PInteger(List.Objects[Result]);
        OutIndex(pIdx^);
        Exit;
      end;
      if (Cardinal(CN) and CN_CHARS_FLAG) = CN_CHARS_FLAG then
      begin
        P := @fTChars[Cardinal(CN) and CN_CHARS_MASK];
        while P^ <> #0 do
        begin
          if L <= K then Exit;
          Inc(K);
          if bCaseInsensitive then
            Ch := CharUCaseTable[T[K]]
          else
            Ch := T[K];
          if Ch <> P^ then Exit;
          Inc(P);
        end;
        Result := Node^.Index;
        pIdx := PInteger(List.Objects[Result]);
        OutIndex(pIdx^);
        Exit;
      end;
      I := Node^.Index;
      Node := @PTreeNodeArray(fNodes)^[I];
      if Node^.Ch = #0 then
      begin
        Result := Node^.Index;
        pIdx := PInteger(List.Objects[Result]);
        OutIndex(pIdx^);
        Dec(CN);
        if CN = 0 then Exit;
        Inc(I);
        Node := @PTreeNodeArray(fNodes)^[I];
      end;
      if L <= K then Exit;
      Inc(K);
      if bCaseInsensitive then
        Ch := CharUCaseTable[T[K]]
      else
        Ch := T[K];
      repeat
        if Node^.Ch = Ch then Break;
        Dec(CN);
        if CN = 0 then Break;
        Inc(I);
        Node := @PTreeNodeArray(fNodes)^[I];
      until False;
    until CN = 0;
  end;

var
  I, J, K: Integer;
  Ch: Char;
  Node: PTreeNode;
begin
  bFindAll := Assigned(Indexes);
  if bFindAll then Indexes.Clear;
  Index := -1;
  Result := 0;
  if not Assigned(fBTrees) and
     not Assigned(fTrees) //not build
    then Exit;
  L := Length(S);
  if L = 0 then Exit;
  if S[L] <> #10 then
  begin
    if S[L] = #13 then
    begin
      if L < 2 then Exit;
      T := Copy(S, 1, L-1) + #10;
    end else
    begin
      T := S + #10;
      Inc(L);
    end;
  end else //if S[L] = #10
  begin
    if L < 2 then Exit;
    if S[L-1] = #13 then
    begin
      T := Copy(S, 1, L-2) + #10;
      Dec(L);
    end else
      T := S;
  end;
  bCaseInsensitive := not fCaseSensitive;
  for K := 1 to L do
  begin
    if bCaseInsensitive then
      Ch := CharUCaseTable[T[K]]
    else
      Ch := T[K];
    if (K = 1) and Assigned(fBTrees) then
    begin
      I := PTreeCharArray(fBTrees)^[Ch];
      if I >= 0 then
      begin
        Node := @PTreeNodeArray(fNodes)^[I];
        J := ProcessTreeNode(Node, fBList, K);
        if J >= 0 then
        begin
          Result := K;
          if not bFindAll then Exit;
        end;
      end;
    end;
    if not Assigned(fTrees) then Break;
    I := PTreeCharArray(fTrees)^[Ch];
    if I < 0 then Continue;
    Node := @PTreeNodeArray(fNodes)^[I];
    J := ProcessTreeNode(Node, fList, K);
    if J >= 0 then
    begin
      Result := K;
      if not bFindAll then Exit;
    end;
  end;
end;

end.
