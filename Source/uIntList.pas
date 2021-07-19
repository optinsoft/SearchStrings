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

unit uIntList;

interface

uses Classes, SysUtils;

type
  TIntArray = array of Integer;

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
    procedure SetSorted(const Value: Boolean);
    procedure SetNoDuplicates(const Value: Boolean);

  protected
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure SetCapacity(NewCapacity: Integer);
    procedure Grow;
    procedure InsertItem(Index: LongInt; const Value: Integer);
    procedure QuickSort(L, R: LongInt); // L < R
    procedure QuickSortExchange(L, R: LongInt); // L < R
    procedure RemoveDuplicates; // FCount > 1

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Value: Integer): LongInt;
    function AddSorted(const Value: Integer; Unique: Boolean): LongInt;
    procedure Exchange(Index1, Index2: LongInt);
    procedure Insert(Index: LongInt; const Value: Integer);
    procedure Delete(Index: LongInt);
    procedure Clear;
    function IndexOf(const Value: Integer): LongInt;
    procedure Assign(lst: TIntList);
    procedure Sort;
    procedure SortRange(L, R: Integer);
    function Find(const Value: Integer): LongInt;
    function FindSorted(const Value: Integer): LongInt;
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure IncValue(Index: LongInt; N: Integer = 1);
    procedure DecValue(Index: LongInt; N: Integer = 1);

    property Count: LongInt read FCount;
    property Value[Index: LongInt]: Integer read GetValue write SetValue; default;
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

procedure TIntList.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EIntListError.CreateFmt(LoadResString(Msg), [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

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

procedure TIntList.Error(const Msg: string; Data: Integer);
begin
  raise EIntListError.CreateFmt(Msg, [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

function TIntList.GetValue(Index: LongInt): Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

procedure TIntList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
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
