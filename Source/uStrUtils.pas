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

unit uStrUtils;

interface

type
  TArrayOfChar = array [Char] of Char;

var
  CharUCaseTable: TArrayOfChar;
  CharLCaseTable: TArrayOfChar;

function LowerCase(Ch: AnsiChar): AnsiChar; overload; inline;
function LowerCase(Ch: WideChar): WideChar; overload; inline;
function InsensPosEx(const SubStr, S: String; Offset: Integer = 1; Len: Integer = MaxInt): Integer;
function SensPosEx(const SubStr, S: String; Offset: Integer = 1; Len: Integer = MaxInt): Integer;
function Empty(const S: String): Boolean;
function FirstChar(const S: String): Char;
function LastChar(const S: String): Char;
function CharPos(Ch: Char; const S: String): Integer;
function CharPosEx(Ch: Char; const S: String; Offset: Integer = 1; Len: Integer = MaxInt): Integer;
function InsensEquals(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensContains(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensStartsWith(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensEndsWith(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensLessThan(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensLessOrEquals(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensMoreThan(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensMoreOrEquals(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function InsensCompare(const S1, S2: String; Len1: Integer = MaxInt; Len2: Integer = MaxInt): Integer;
function InsensStrAfter(const S, SPrefix: String; Offset: Integer = 1; Len: Integer = MaxInt): String;
function InsensStrBefore(const S, SPostfix: String; Offset: Integer = 1; Len: Integer = MaxInt): String;
function InsensStrBetween(const S, SPrefix, SPostfix: String; Offset: Integer = 1; Len: Integer = MaxInt): String;
function SensEquals(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensContains(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensStartsWith(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensEndsWith(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensLessThan(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensLessOrEquals(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensMoreThan(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensMoreOrEquals(const S1, S2: String; Len: Integer = MaxInt): Boolean;
function SensCompare(const S1, S2: String; Len1: Integer = MaxInt; Len2: Integer = MaxInt): Integer;
function SensStrAfter(const S, SPrefix: String; Offset: Integer = 1; Len: Integer = MaxInt): String;
function SensStrBefore(const S, SPostfix: String; Offset: Integer = 1; Len: Integer = MaxInt): String;
function SensStrBetween(const S, SPrefix, SPostfix: String; Offset: Integer = 1; Len: Integer = MaxInt): String;
function StrEquals(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrContains(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrStartsWith(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrEndsWith(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrLessThan(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrLessOrEquals(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrMoreThan(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrMoreOrEquals(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer = MaxInt): Boolean;
function StrCompare(const S1, S2: String; CaseInsensitive: Boolean; Len1: Integer = MaxInt; Len2: Integer = MaxInt): Integer;
function StrAfter(const S, SPrefix: String; CaseInsensitive: Boolean; Offset: Integer = 1; Len: Integer = MaxInt): String;
function StrBefore(const S, SPostfix: String; CaseInsensitive: Boolean; Offset: Integer = 1; Len: Integer = MaxInt): String;
function StrBetween(const S, SPrefix, SPostfix: String; CaseInsensitive: Boolean; Offset: Integer = 1; Len: Integer = MaxInt): String;
function QuoteString(const S: String; QuoteEmpty: Boolean = False; QChar: Char = '"'; EscChar: Char = '"'; Offset: Integer = 1; Len: Integer = MaxInt): String;
function DequoteString(const S: String; QChar: Char = #0; EscChar: Char = #0; Offset: Integer = 1; Len: Integer = MaxInt): String;
function QStrPosEx(const SubStr, S: String; QChar: Char = #0; EscChar: Char = #0; Offset: Integer = 1; Len: Integer = MaxInt): Integer;
procedure StrToUpperCase(var S: String);
procedure StrToLowerCase(var S: String);
function SensStrIndex(const AStr: string; const AValues: array of String): Integer;
function InsensStrIndex(const AStr: string; const AValues: array of String): Integer;
function AppendString(const AStr, AAppend, ADelimiter: String): String;
function ValuesToString(const Values: array of String; Delimiter: Char = ',';
  QChar: Char = '"'; EscChar: Char = '"'; QuoteAll: Boolean = False): String;
function IsSpaceChar(c: Char): Boolean;
function QPosEx(const SubStr, S: String; QChar: Char = '"'; EscChar: Char = '"';
  Offset: Integer = 1): Integer;

implementation

uses System.SysUtils, System.StrUtils;

{ string utils }

function LowerCase(Ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['A'..'Z'] then
    Dec(Result, Ord('A')-Ord('a'));
end;

function LowerCase(Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Ch of
    'A'..'Z':
      Result := WideChar(Word(Ch) or $0020);
  end;
end;

procedure InitCharUCaseTable(var Table: TArrayOfChar);
var
  n: Cardinal;
begin
  for n := 0 to Length(Table) - 1 do
    Table[Char(n)] := UpCase(Char(n));
end;

procedure InitCharLCaseTable(var Table: TArrayOfChar);
var
  n: Cardinal;
begin
  for n := 0 to Length(Table) - 1 do
    Table[Char(n)] := LowerCase(Char(n));
end;

function InsensPosEx(const SubStr, S: String; Offset, Len: Integer): Integer;
var
  n: Integer;
  SubStrLength: Integer;
  SLength: Integer;
label
  __fail;
begin
  if Offset > 0 then
  begin
    SLength := Length(S);
    if Len < SLength then
      SLength := Len;
    if (SLength > 0) and (Offset > 0) then
    begin
      SubStrLength := Length(SubStr);
      Result := Offset;
      while SubStrLength <= SLength - Result + 1 do
      begin
        for n := 1 to SubStrLength do
          if CharUCaseTable[SubStr[n]] <> CharUCaseTable[S[Result + n - 1]]
          then
            goto __fail;
        Exit;
__fail:
        Inc(Result);
      end;
    end;
  end;
  Result := 0;
end;

function SensPosEx(const SubStr, S: String; Offset, Len: Integer): Integer;
var
  n: Integer;
  SubStrLength: Integer;
  SLength: Integer;
label
  __fail;
begin
  if Offset > 0 then
  begin
    SLength := Length(S);
    if Len < SLength then
      SLength := Len;
    if (SLength > 0) and (Offset > 0) then
    begin
      SubStrLength := Length(SubStr);
      Result := Offset;
      while SubStrLength <= SLength - Result + 1 do
      begin
        for n := 1 to SubStrLength do
          if SubStr[n] <> S[Result + n - 1]
          then
            goto __fail;
        Exit;
__fail:
        Inc(Result);
      end;
    end;
  end;
  Result := 0;
end;

function Empty(const S: String): Boolean;
begin
  Result := (Length(S) = 0);
end;

function FirstChar(const S: String): Char;
begin
  if Length(S) > 0 then
    Result := S[1]
  else
    Result := #0;
end;

function LastChar(const S: String): Char;
var
  L: Integer;
begin
  L := Length(S);
  if L > 0 then
    Result := S[L]
  else
    Result := #0;
end;

function CharPos(Ch: Char; const S: String): Integer;
var
  I, L: Integer;
begin
  L := Length(S);
  for I := 1 to L do
  begin
    if S[I] = Ch then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := 0;
end;

function CharPosEx(Ch: Char; const S: String; Offset, Len: Integer): Integer;
var
  I, L: Integer;
label
  __not_found;
begin
  L := Length(S)-Offset+1;
  if Len < L then L := Len;
  if (Offset < 1) or (L < 1)
    then goto __not_found;
  for I := 0 to L-1 do
  begin
    if S[Offset+I] = Ch then
    begin
      Result := Offset+I;
      Exit;
    end;
  end;
__not_found:
  Result := 0;
end;

function InsensEquals(const S1, S2: String; Len: Integer): Boolean;
var
  I, L1, L2: Integer;
begin
  L2 := Length(S2);
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  Result := (L1 = L2);
  if not Result then Exit;
  for I := 1 to L2 do
  begin
    if CharUCaseTable[S1[I]] <> CharUCaseTable[S2[I]] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function InsensContains(const S1, S2: String; Len: Integer): Boolean;
begin
  Result := (InsensPosEx(S2, S1, 1, Len) > 0);
end;

function InsensStartsWith(const S1, S2: String; Len: Integer): Boolean;
var
  I, L1, L2: Integer;
begin
  L2 := Length(S2);
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  Result := (L1 >= L2);
  if not Result then Exit;
  for I := 1 to L2 do
  begin
    if CharUCaseTable[S1[I]] <> CharUCaseTable[S2[I]] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function InsensEndsWith(const S1, S2: String; Len: Integer): Boolean;
var
  I, L1, L2: Integer;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  Result := (L1 >= L2);
  if not Result then Exit;
  for I := 1 to L2 do
  begin
    if CharUCaseTable[S1[L1-L2+I]] <> CharUCaseTable[S2[I]] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function InsensLessThan(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := CharUCaseTable[S1[I]];
    C2 := CharUCaseTable[S2[I]];
    if C1 < C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 > C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 < L2);
end;

function InsensLessOrEquals(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := CharUCaseTable[S1[I]];
    C2 := CharUCaseTable[S2[I]];
    if C1 < C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 > C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 <= L2);
end;

function InsensMoreThan(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := CharUCaseTable[S1[I]];
    C2 := CharUCaseTable[S2[I]];
    if C1 > C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 < C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 > L2);
end;

function InsensMoreOrEquals(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := CharUCaseTable[S1[I]];
    C2 := CharUCaseTable[S2[I]];
    if C1 > C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 < C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 >= L2);
end;

function InsensCompare(const S1, S2: String; Len1, Len2: Integer): Integer;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len1 < L1 then L1 := Len1;
  L2 := Length(S2);
  if Len2 < L2 then L2 := Len2;
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := CharUCaseTable[S1[I]];
    C2 := CharUCaseTable[S2[I]];
    if C1 < C2 then
    begin
      Result := -1;
      Exit;
    end;
    if C1 > C2 then
    begin
      Result := 1;
      Exit;
    end;
  end;
__after_for:
   Result := L1-L2;
end;

function InsensStrAfter(const S, SPrefix: String; Offset, Len: Integer): String;
var
  I: Integer;
begin
  I := InsensPosEx(SPrefix, S, Offset, Len);
  if I > 0 then
    Result := Copy(S, I+Length(SPrefix), MaxInt)
  else
    Result := '';
end;

function InsensStrBefore(const S, SPostfix: String; Offset, Len: Integer): String;
var
  I: Integer;
begin
  I := InsensPosEx(SPostfix, S, Offset, Len);
  if I > 1 then
    Result := Copy(S, 1, I-1)
  else
    Result := '';
end;

function InsensStrBetween(const S, SPrefix, SPostfix: String; Offset, Len: Integer): String;
var
  I, J: Integer;
begin
  I := InsensPosEx(SPrefix, S, Offset, Len);
  if I > 0 then
  begin
    Inc(I, Length(SPrefix));
    J := InsensPosEx(SPostfix, S, I, Len);
    if J > I then
    begin
      Result := Copy(S, I, J-I);
      Exit;
    end;
  end;
  Result := '';
end;

function SensEquals(const S1, S2: String; Len: Integer): Boolean;
var
  I, L1, L2: Integer;
begin
  L2 := Length(S2);
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  Result := (L1 = L2);
  if not Result then Exit;
  for I := 1 to L2 do
  begin
    if S1[I] <> S2[I] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function SensContains(const S1, S2: String; Len: Integer): Boolean;
begin
  Result := (SensPosEx(S2, S1, 1, Len) > 0);
end;

function SensStartsWith(const S1, S2: String; Len: Integer): Boolean;
var
  I, L1, L2: Integer;
begin
  L2 := Length(S2);
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  Result := (L1 >= L2);
  if not Result then Exit;
  for I := 1 to L2 do
  begin
    if S1[I] <> S2[I] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function SensEndsWith(const S1, S2: String; Len: Integer): Boolean;
var
  I, L1, L2: Integer;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  Result := (L1 >= L2);
  if not Result then Exit;
  for I := 1 to L2 do
  begin
    if S1[L1-L2+I] <> S2[I] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function SensLessThan(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := S1[I];
    C2 := S2[I];
    if C1 < C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 > C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 < L2);
end;

function SensLessOrEquals(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := S1[I];
    C2 := S2[I];
    if C1 < C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 > C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 <= L2);
end;

function SensMoreThan(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := S1[I];
    C2 := S2[I];
    if C1 > C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 < C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 > L2);
end;

function SensMoreOrEquals(const S1, S2: String; Len: Integer): Boolean;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len < L1 then L1 := Len;
  L2 := Length(S2);
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := CharUCaseTable[S1[I]];
    C2 := CharUCaseTable[S2[I]];
    if C1 > C2 then
    begin
      Result := True;
      Exit;
    end;
    if C1 < C2 then
    begin
      Result := False;
      Exit;
    end;
  end;
__after_for:
  Result := (L1 >= L2);
end;

function SensCompare(const S1, S2: String; Len1, Len2: Integer): Integer;
var
  I, L, L1, L2: Integer;
  C1, C2: Char;
label
  __after_for;
begin
  L1 := Length(S1);
  if Len1 < L1 then L1 := Len1;
  L2 := Length(S2);
  if Len2 < L2 then L2 := Len2;
  if (L1 = 0) or (L2 = 0) then
    goto __after_for;
  if L2 < L1 then L := L2 else L := L1;
  for I := 1 to L do
  begin
    C1 := S1[I];
    C2 := S2[I];
    if C1 < C2 then
    begin
      Result := -1;
      Exit;
    end;
    if C1 > C2 then
    begin
      Result := 1;
      Exit;
    end;
  end;
__after_for:
   Result := L1-L2;
end;

function SensStrAfter(const S, SPrefix: String; Offset, Len: Integer): String;
var
  I: Integer;
begin
  I := SensPosEx(SPrefix, S, Offset, Len);
  if I > 0 then
    Result := Copy(S, I+Length(SPrefix), MaxInt)
  else
    Result := '';
end;

function SensStrBefore(const S, SPostfix: String; Offset, Len: Integer): String;
var
  I: Integer;
begin
  I := SensPosEx(SPostfix, S, Offset, Len);
  if I > 1 then
    Result := Copy(S, 1, I-1)
  else
    Result := '';
end;

function SensStrBetween(const S, SPrefix, SPostfix: String; Offset, Len: Integer): String;
var
  I, J: Integer;
begin
  I := SensPosEx(SPrefix, S, Offset, Len);
  if I > 0 then
  begin
    Inc(I, Length(SPrefix));
    J := SensPosEx(SPostfix, S, I, Len);
    if J > I then
    begin
      Result := Copy(S, I, J-I);
      Exit;
    end;
  end;
  Result := '';
end;

function StrEquals(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensEquals(S1, S2, Len)
  else
    Result := SensEquals(S1, S2, Len);
end;

function StrContains(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensContains(S1, S2, Len)
  else
    Result := SensContains(S1, S2, Len);
end;

function StrStartsWith(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensStartsWith(S1, S2, Len)
  else
    Result := SensStartsWith(S1, S2, Len);
end;

function StrEndsWith(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensEndsWith(S1, S2, Len)
  else
    Result := SensEndsWith(S1, S2, Len);
end;

function StrLessThan(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensLessThan(S1, S2, Len)
  else
    Result := SensLessThan(S1, S2, Len);
end;

function StrLessOrEquals(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensLessOrEquals(S1, S2, Len)
  else
    Result := SensLessOrEquals(S1, S2, Len);
end;

function StrMoreThan(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensMoreThan(S1, S2, Len)
  else
    Result := SensMoreThan(S1, S2, Len);
end;

function StrMoreOrEquals(const S1, S2: String; CaseInsensitive: Boolean; Len: Integer): Boolean;
begin
  if CaseInsensitive then
    Result := InsensMoreOrEquals(S1, S2, Len)
  else
    Result := SensMoreOrEquals(S1, S2, Len);
end;

function StrCompare(const S1, S2: String; CaseInsensitive: Boolean; Len1, Len2: Integer): Integer;
begin
  if CaseInsensitive then
    Result := InsensCompare(S1, S2, Len1, Len2)
  else
    Result := SensCompare(S1, S2, Len1, Len2);
end;

function StrAfter(const S, SPrefix: String; CaseInsensitive: Boolean; Offset, Len: Integer): String;
begin
  if CaseInsensitive then
    Result := InsensStrAfter(S, SPrefix, Offset, Len)
  else
    Result := SensStrAfter(S, SPrefix, Offset, Len);
end;

function StrBefore(const S, SPostfix: String; CaseInsensitive: Boolean; Offset, Len: Integer): String;
begin
  if CaseInsensitive then
    Result := InsensStrBefore(S, SPostfix, Offset, Len)
  else
    Result := SensStrBefore(S, SPostfix, Offset, Len);
end;

function StrBetween(const S, SPrefix, SPostfix: String; CaseInsensitive: Boolean; Offset, Len: Integer): String;
begin
  if CaseInsensitive then
    Result := InsensStrBetween(S, SPrefix, SPostfix, Offset, Len)
  else
    Result := SensStrBetween(S, SPrefix, SPostfix, Offset, Len);
end;

function QuoteString(const S: String; QuoteEmpty: Boolean; QChar, EscChar: Char;
  Offset, Len: Integer): String;
var
  I, K, L, N, NQuotes: Integer;
  Ch: Char;
begin
  if QChar = #0 then
  begin
    Result := Copy(S, Offset, Len);
    Exit;
  end;
  L := Length(S)-Offset+1;
  if Len < L then L := Len;
  if (Offset < 1) or (L < 1) then
  begin
    if QuoteEmpty then
      Result := QChar+QChar
    else
      Result := '';
    Exit;
  end;
  NQuotes := 0;
  if EscChar <> #0 then
  begin
    for I := 0 to L-1 do
     if (S[Offset+I] = QChar) or
        (S[Offset+I] = EscChar)
     then
       Inc(NQuotes);
  end;
  N := L + NQuotes + 2;
  SetLength(Result, N);
  Result[1] := QChar;
  K := 2;
  for I := 0 to L-1 do
  begin
    Ch := S[Offset+I];
    if (EscChar <> #0) and
       ((Ch = QChar) or (Ch = EscChar)) then
    begin
      Result[K] := EscChar;
      Inc(K);
    end;
    Result[K] := Ch;
    Inc(K);
  end;
  Result[N] := QChar;
end;

function DequoteString(const S: String; QChar, EscChar: Char; Offset, Len: Integer): String;
var
  I, L: Integer;
begin
  L := Length(S)-Offset+1;
  if Len < L then L := Len;
  if (Offset < 1) or (L < 1) then
  begin
    Result := '';
    Exit;
  end;
  if L >= 2 then
  begin
    if (QChar = #0) and
       ((S[Offset] = '"') or (S[Offset] = ''''))
    then
      QChar := S[Offset];
    if S[Offset] = QChar then
    begin
      I := 1;
      if EscChar = #0
        then EscChar := QChar;
      while I < L do
      begin
        if (I+1 < L) and
           (S[Offset+I] = EscChar) and
           ((S[Offset+I+1] = QChar) or
            (S[Offset+I+1] = EscChar))
        then
          Inc(I)
        else if S[Offset+I] = QChar then
        begin
          Result := Copy(S, Offset+1, I-1);
          Exit;
        end;
        Inc(I);
      end;
    end;
  end;
  Result := Copy(S, Offset, L);
end;

function QStrPosEx(const SubStr, S: String; QChar, EscChar: Char; Offset, Len: Integer): Integer;
var
  I, J: Integer;
begin
  I := PosEx(SubStr, S, Offset);
  while I > 0 do
  begin
    J := I;
    if QChar = #0 then
    begin
      while J >= Offset do
      begin
        if (S[J] = '"') or (S[J] = '''') then
        begin
          if J = Offset then Break;
          if EscChar <> #0 then
          begin
            if S[J-1] <> EscChar then Break;
          end else
          begin
            if S[J-1] <> S[J] then Break;
          end;
        end;
        Dec(J);
      end;
    end else
    begin
      while J >= Offset do
      begin
        if S[J] = QChar then
        begin
          if J = Offset then Break;
          if EscChar <> #0 then
          begin
            if S[J-1] <> EscChar then Break;
          end else
          begin
            if S[J-1] <> S[J] then Break;
          end;
        end;
        Dec(J);
      end;
    end;
    if J >= Offset then
    begin
      Result := J;
      Exit;
    end;
    I := PosEx(SubStr, S, I+1);
  end;
  Result := 0;
end;

procedure StrToUpperCase(var S: String);
var
  I, L: Integer;
begin
  L := Length(S);
  for I := 1 to L do
    S[I] := CharUCaseTable[S[I]];
end;

procedure StrToLowerCase(var S: string);
var
  I, L: Integer;
begin
  L := Length(S);
  for I := 1 to L do
    S[I] := CharLCaseTable[S[I]];
end;

function SensStrIndex(const AStr: string; const AValues: array of string): Integer;
var
  I: Integer;
begin
  for I := Low(AValues) to High(AValues) do
    if SensEquals(AStr, AValues[I]) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function InsensStrIndex(const AStr: string; const AValues: array of string): Integer;
var
  I: Integer;
begin
  for I := Low(AValues) to High(AValues) do
    if InsensEquals(AStr, AValues[I]) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function AppendString(const AStr, AAppend, ADelimiter: String): String;
begin
  if Length(AStr) = 0 then
    Result := AAppend
  else
    Result := AStr + ADelimiter + AAppend;
end;

function ValuesToString(const Values: array of String; Delimiter,
  QChar, EscChar: Char; QuoteAll: Boolean): String;
var
  I0, I: Integer;
begin
  I0 := Low(Values);
  if I0 > High(Values) then
    Result := ''
  else begin
    Result := '';
    for I := I0 to High(Values) do
    begin
      if I > I0 then Result := Result + Delimiter;
      if (QChar <> #0) and
         (QuoteAll or
          (CharPos(QChar, Values[I]) > 0) or
          (CharPos(Delimiter, Values[I]) > 0))
      then
        Result := Result +
          QChar + StringReplace(Values[I], QChar, EscChar+QChar, [rfReplaceAll]) + QChar
      else
        Result := Result + Values[I];
    end;
  end;
end;

function IsSpaceChar(c: Char): Boolean;
begin
  Result := (c = ' ') or (c = #9) or (c = #13) or (c = #10);
end;

function QPosEx(const SubStr, S: String; QChar, EscChar: Char; Offset: Integer): Integer;
//look System.StrUtils.PosEx
var
  I, LIterCnt, L, J: Integer;
  PSubStr, PS: PChar;
  Q: Boolean;
begin
  if SubStr = '' then
    Exit(0);

  { Calculate the number of possible iterations. Not valid if Offset < 1. }
  LIterCnt := Length(S) - Offset - Length(SubStr) + 1;

  { Only continue if the number of iterations is positive or zero (there is space to check) }
  if (Offset > 0) and (LIterCnt >= 0) then
  begin
    L := Length(SubStr);
    PSubStr := PChar(SubStr);
    PS := PChar(S);
    Inc(PS, Offset - 1);
    Q := False;

    for I := 0 to LIterCnt do
    begin
      if (PS+I)^ = QChar then
      begin
        if //(I = 0) or
           (not Q) or
           (EscChar = QChar) or
           ((PS+I-1)^ <> EscChar) then
        begin
          Q := not Q;
          Continue;
        end;
      end;
      if Q then Continue;
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        if (PS + I + J)^ = (PSubStr + J)^ then
          Inc(J)
        else
          J := -1;
      end;
      if J >= L then
        Exit(I + Offset);
    end;
  end;

  Result := 0;
end;

initialization
  InitCharUCaseTable(CharUCaseTable);
  InitCharLCaseTable(CharLCaseTable);
end.
