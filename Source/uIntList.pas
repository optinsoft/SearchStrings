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
{ Date: June 09, 2022                                                          }
{ Version: 1.2                                                                 }
{                                                                              }
{ v1.2:                                                                        }
{  - Delphi 7 compatibility                                                    }
{  - Grow Delta argument                                                       }
{  - Shuffle, ShuffleRange                                                     }
{  - AddRange, AddN                                                            }
{  - AddFromString                                                             }
{  - PInt                                                                      }
{ v1.1:                                                                        }
{  - copyright                                                                 }
{ v1.0:                                                                        }
{ - Initial release                                                            }
{                                                                              }
{ **************************************************************************** }

unit uIntList;

interface

uses Classes, SysUtils;

type
  TIntArray = array of Integer;

{$IF CompilerVersion >= 25.0 } //Delphi XE4
  {$LEGACYIFEND ON}
{$IFEND}

{$IF not Declared(TExchangeItemsEvent)}
  TExchangeItemsEvent = procedure (Index1, Index2: LongInt) of object;
{$IFEND}

  TIntList = class(TObject)
  private
    FList: TIntArray;
    FCount: LongInt;
    FCapacity: Integer;
    FSorted: Boolean;
    FNoDuplicates: Boolean;
    FOnExchangeItems: TExchangeItemsEvent;

    function GetValue(Index: LongInt): Integer;
    procedure SetValue(Index: LongInt; Value: Integer);
    function GetPInt(Index: LongInt): PInteger;
    procedure SetSorted(const Value: Boolean);
    procedure SetNoDuplicates(const Value: Boolean);

  protected
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure SetCapacity(NewCapacity: Integer);
    procedure Grow(Delta: Integer = 0);
    procedure InsertItem(Index: LongInt; const Value: Integer);
    procedure QuickSort(L, R: LongInt); // L < R
    procedure QuickSortExchange(L, R: LongInt); // L < R
    procedure RemoveDuplicates; // FCount > 1
    procedure ShuffleRange(L, R: LongInt);

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Value: Integer): LongInt;
    function AddSorted(const Value: Integer; Unique: Boolean): LongInt;
    procedure AddRange(const LowValue, HighValue: Integer);
    procedure AddN(const Value: Integer; N: Integer);
    procedure Exchange(Index1, Index2: LongInt);
    procedure Insert(Index: LongInt; const Value: Integer);
    procedure Delete(Index: LongInt);
    procedure Clear;
    function IndexOf(const Value: Integer): LongInt;
    procedure Assign(lst: TIntList);
    procedure Sort;
    procedure SortRange(L, R: Integer);
    procedure Shuffle;
    function Find(const Value: Integer): LongInt;
    function FindSorted(const Value: Integer): LongInt;
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure IncValue(Index: LongInt; N: Integer = 1);
    procedure DecValue(Index: LongInt; N: Integer = 1);
    procedure AddFromString(const AStr: String);

    property Count: LongInt read FCount;
    property Value[Index: LongInt]: Integer read GetValue write SetValue; default;
    property PInt[Index: LongInt]: PInteger read GetPInt;
    property Sorted: Boolean read FSorted write SetSorted;
    property NoDuplicates: Boolean read FNoDuplicates write SetNoDuplicates;
  end;

  EIntListError = class(Exception);

implementation

uses RTLConsts;

constructor TIntList.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FNoDuplicates := False;
  FOnExchangeItems := nil;
end;

destructor TIntList.Destroy;
begin
  FCount := 0;
  SetCapacity(0);
  inherited Destroy;
end;

{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF O+}
procedure TIntList.Error(Msg: PResStringRec; Data: Integer);
{$IFDEF VER150} //Delphi 7
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
{$ENDIF}
begin
  raise EIntListError.CreateFmt(LoadResString(Msg), [Data]) at {$IFDEF VER150}ReturnAddr;{$ELSE}
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;{$ENDIF}
end;

procedure TIntList.Error(const Msg: string; Data: Integer);
{$IFDEF VER150} //Delphi 7
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
{$ENDIF}
begin
  raise EIntListError.CreateFmt(Msg, [Data]) at {$IFDEF VER150}ReturnAddr;{$ELSE}
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;{$ENDIF}
end;
{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF OPTIMIZATIONSON}

procedure TIntList.Exchange(Index1, Index2: Integer);
var
  T: Integer;
begin
  if (Index1 < 0) or (Index1 > FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 > FCount) then
    Error(@SListIndexError, Index2);
  if Index1 <> Index2 then
  begin
    T := FList[Index1];
    FList[Index1] := FList[Index2];
    FList[Index2] := T;
    if Assigned(FOnExchangeItems) then
      FOnExchangeItems(Index1, Index2);
  end;
end;

function TIntList.Find(const Value: Integer): LongInt;
var
  I: LongInt;
begin
  if FSorted then
  begin
    Result := FindSorted(Value);
    Exit;
  end;
  for I := 0 to FCount-1 do
  begin
    if FList[I] = Value then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TIntList.FindSorted(const Value: Integer): LongInt;
var
  L, H, I: LongInt;
  bFound: Boolean;
  X: Integer;
begin
  bFound := False;
  L := 0;
  H := FCount-1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    X := FList[I];
    if X < Value then L := I+1 else
    begin
      H := I-1;
      if X = Value then
        bFound := True;
    end;
  end;
  if bFound then
    Result := L
  else
    Result := -1;
end;

function TIntList.GetPInt(Index: LongInt): PInteger;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := @FList[Index];
end;

function TIntList.GetValue(Index: LongInt): Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

procedure TIntList.Grow;
begin
  if Delta < 4 then
  begin
    if FCapacity > 64 then Delta := FCapacity div 4 else
      if FCapacity > 8 then Delta := 16 else
        Delta := 4;
  end;
  SetCapacity(FCapacity + Delta);
end;

procedure TIntList.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIntList.SaveToStream(Stream: TStream);
begin
  if FCount > 0 then Stream.WriteBuffer(FList[0], FCount * SizeOf(Integer));
end;

procedure TIntList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TIntList.SetNoDuplicates(const Value: Boolean);
begin
  if FNoDuplicates = Value then Exit;
  if FSorted and Value then
  begin
    if FCount > 1 then RemoveDuplicates;
  end;
  FNoDuplicates := Value;
end;

procedure TIntList.SetSorted(const Value: Boolean);
begin
  if FSorted = Value then Exit;
  if Value then Sort;
  FSorted := Value;
end;

procedure TIntList.SetValue(Index: LongInt; Value: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  FList[Index] := Value;
end;

procedure TIntList.Shuffle;
begin
  FSorted := False;
  if FCount > 1 then
    ShuffleRange(0, FCount-1);
end;

procedure TIntList.ShuffleRange(L, R: LongInt);
var
  I, J: LongInt;
  T: Integer;
begin
  //FSorted := False;
  for I := L to R-1 do
  begin
    J := I + Random(R+1-I);
    if J <> I then
    begin
      T := FList[I];
      FList[I] := FList[J];
      FList[J] := T;
    end;
  end;
end;

procedure TIntList.Sort;
begin
  if FCount > 1 then
  begin
    QuickSort(0, FCount-1);
    if FNoDuplicates then RemoveDuplicates;
  end;
end;

procedure TIntList.SortRange(L, R: Integer);
begin
 if (L < 0) or (L >= FCount) then
   Error(@SListIndexError, L);
 if (R < 0) or (R >= FCount) then
   Error(@SListIndexError, R);
 if R > L then
 begin
   QuickSort(L, R);
 end;
end;

function TIntList.Add(const Value: Integer): LongInt;
begin
  if (FCount > 0) and FSorted then
  begin
    Result := AddSorted(Value, FNoDuplicates);
    Exit;
  end;
  if FCount = FCapacity then Grow;
  FList[FCount] := Value;
  Result := FCount;
  Inc(FCount);
end;

procedure TIntList.IncValue(Index, N: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Inc(FList[Index], N);
end;

function TIntList.IndexOf(const Value: Integer): LongInt;
var
  J: LongInt;
begin
  for J := 0 to FCount-1 do
  begin
    if FList[J] = Value then
    begin
      Result := J;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TIntList.InsertItem(Index: LongInt; const Value: Integer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index+1],
      (FCount-Index) * SizeOf(Integer));
  FList[Index] := Value;
  Inc(FCount);
end;

procedure TIntList.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TIntList.LoadFromStream(Stream: TStream);
var
  Size, NewCount: LongInt;
begin
  Size := Stream.Size - Stream.Position;
  NewCount := Size div SizeOf(Integer);
  FCount := 0;
  SetCapacity(NewCount);
  if NewCount > 0 then Stream.Read(FList[0], NewCount * SizeOf(Integer));
  FCount := FCapacity;
end;

procedure TIntList.QuickSort(L, R: LongInt); // L < R
var
  I, J, M: LongInt;
  T: Integer;
begin
  repeat
    I := L;
    J := R;
    M := (L + R) shr 1;
    repeat
      while FList[I] < FList[M] do Inc(I);
      while FList[J] > FList[M] do Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := FList[I];
          FList[I] := FList[J];
          FList[J] := T;
        end;
        if M = I then
          M := J
        else if M = J then
          M := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TIntList.QuickSortExchange(L, R: LongInt); // L < R
var
  I, J, M: LongInt;
  T: Integer;
begin
  repeat
    I := L;
    J := R;
    M := (L + R) shr 1;
    repeat
      while FList[I] < FList[M] do Inc(I);
      while FList[J] > FList[M] do Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := FList[I];
          FList[I] := FList[J];
          FList[J] := T;
          FOnExchangeItems(I, J);
        end;
        if M = I then
          M := J
        else if M = J then
          M := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortExchange(L, J);
    L := I;
  until I >= R;
end;

procedure TIntList.RemoveDuplicates; // FCount > 1
var
  I, J, R: LongInt;
begin
  R := FCount-1;
  J := 0;
  for I := 1 to R do
  begin
    if FList[I] = FList[J] then Continue;
    Inc(J);
    if I <> J then FList[J] := FList[I];
  end;
  if J < R then FCount := J+1;
end;

procedure TIntList.Insert(Index: LongInt; const Value: Integer);
begin
  if FSorted then Error(@SSortedListError, 0);
  InsertItem(Index, Value);
end;

procedure TIntList.DecValue(Index, N: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Dec(FList[Index], N);
end;

procedure TIntList.Delete(Index: LongInt);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index+1], FList[Index],
      (FCount-Index) * SizeOf(Integer));
end;

procedure TIntList.Assign(lst: TIntList);
begin
  FCount := lst.FCount;
  SetCapacity(lst.FCapacity);
  if FCount > 0 then
    System.Move(lst.FList[0], FList[0], FCount * SizeOf(Integer));
end;

procedure TIntList.AddFromString(const AStr: String);
var
  I, N, CN, CN2, SLen: Integer;
begin
  SLen := Length(AStr);
  I := 1;
  while (I <= SLen) and (AStr[I] = ' ')
    do Inc(I);
  while I <= SLen do
  begin
    if AStr[I] = ',' then
    begin
      Inc(I);
      Continue;
    end;
{$IF CompilerVersion >= 20.0 } // Delphi 2009 and above
    if not CharInSet(AStr[I], ['1'..'9']) then Break;
{$ELSE}
    if not (AStr[I] in ['1'..'9']) then Break;
{$IFEND}
    CN := Ord(AStr[I])-Ord('0');
    N := 1;
{$IF CompilerVersion >= 20.0 } // Delphi 2009 and above
    while (I+N <= SLen) and CharInSet(AStr[I+N], ['0'..'9']) do
{$ELSE}
    while (I+N <= SLen) and (AStr[I+N] in ['0'..'9']) do
{$IFEND}
    begin
      CN := CN * 10 + (Ord(AStr[I+N])-Ord('0'));
      Inc(N);
    end;
    Inc(I, N);
    while (I <= SLen) and (AStr[I] = ' ')
      do Inc(I);
    if (I < SLen) and (AStr[I] = '-') then
    begin
      Inc(I);
      while (I <= SLen) and (AStr[I] = ' ')
        do Inc(I);
{$IF CompilerVersion >= 20.0 } // Delphi 2009 and above
      if (I > SLen) or not CharInSet(AStr[I], ['1'..'9']) then Break;
{$ELSE}
      if (I > SLen) or not (AStr[I] in ['1'..'9']) then Break;
{$IFEND}
      CN2 := Ord(AStr[I])-Ord('0');
      N := 1;
{$IF CompilerVersion >= 20.0 } // Delphi 2009 and above
      while (I+N <= SLen) and CharInSet(AStr[I+N], ['0'..'9']) do
{$ELSE}
      while (I+N <= SLen) and (AStr[I+N] in ['0'..'9']) do
{$IFEND}
      begin
        CN2 := CN2 * 10 + (Ord(AStr[I+N])-Ord('0'));
        Inc(N);
      end;
      Inc(I, N);
      while (I <= SLen) and (AStr[I] = ' ')
        do Inc(I);
      AddRange(CN, CN2);
    end else
      Add(CN);
    if (I > SLen) or (AStr[I] <> ',') then Break;
  end;
end;

procedure TIntList.AddN(const Value: Integer; N: Integer);
var
  I, Delta: Integer;
begin
  if N < 1 then Exit;
  if FSorted then
  begin
    for I := 0 to N-1 do
      AddSorted(Value, FNoDuplicates);
    Exit;
  end;
  Delta := FCount + N - FCapacity;
  if Delta > 0 then Grow(Delta);
  for I := 0 to N-1 do
    FList[FCount+I] := Value;
  Inc(FCount, N);
end;

procedure TIntList.AddRange(const LowValue, HighValue: Integer);
var
  I, N, Delta: Integer;
begin
  if HighValue < LowValue then Exit;
  if FSorted then
  begin
    for I := LowValue to HighValue do
      AddSorted(I, FNoDuplicates);
    Exit;
  end;
  N := HighValue + 1 - LowValue;
  Delta := FCount + N - FCapacity;
  if Delta > 0 then Grow(Delta);
  for I := 0 to N-1 do
    FList[FCount+I] := LowValue + I;
  Inc(FCount, N);
end;

function TIntList.AddSorted(const Value: Integer; Unique: Boolean): LongInt;
var
  L, H, I: LongInt;
  X: Integer;
begin
  L := 0;
  H := FCount-1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    X := FList[I];
    if X < Value then L := I+1 else
    begin
      H := I-1;
      if X = Value then
      begin
        if Unique then
        begin
          Result := -1;
          Exit;
        end;
        L := I;
      end;
    end;
  end;
  Result := L;
  InsertItem(Result, Value);
end;

procedure TIntList.Clear;
begin
  if FCount = 0 then Exit;
  FCount := 0;
  SetCapacity(0);
end;

end.
