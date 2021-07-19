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

unit uSearchClass;

interface

uses Classes, SysUtils, uSearchStrings, uLineReaderWriter, uIntList;

type
  TProcessedEvent = procedure (Sender: TObject; nProcessed, nFound: Integer) of object;

  TSearchClass = class
  private
    fOutFormat: String;
    fTerminated: Boolean;
    fOnProcessed: TProcessedEvent;
  protected
    procedure Error(const Msg: String);
    function FormatOut(const OutFormat, S: String;
      //const SearchStr, FormattedSearchStr: String;
      lstSearch, lstFormattedSearch: TStrings;
      lstFoundIndexes: TIntList): String; virtual;
    procedure DoOnProcessed(nProcessed, nFound: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function DoSearch(st1, st2: TSearcher; SearchAll: Boolean;
      srcLines: TLineReader; lstSearch, lstFormattedSearch: TStrings;
      outLines: TLineWriter): Integer;

    property OutFormat: String read fOutFormat write fOutFormat;
    property Terminated: Boolean read fTerminated write fTerminated;
    property OnProcessed: TProcessedEvent read fOnProcessed write fOnProcessed;
  end;

  ESearchError = class(Exception);

implementation

uses DateUtils, StrUtils;

{ TSearchClass }

constructor TSearchClass.Create;
begin
  inherited;
end;

destructor TSearchClass.Destroy;
begin

  inherited;
end;

procedure TSearchClass.Error(const Msg: String);
begin
  raise ESearchError.Create(Msg) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

procedure TSearchClass.DoOnProcessed(nProcessed, nFound: Integer);
begin
  if Assigned(fOnProcessed) then
    fOnProcessed(Self, nProcessed, nFound);
end;

function TSearchClass.DoSearch(st1, st2: TSearcher; SearchAll: Boolean;
  srcLines: TLineReader; lstSearch, lstFormattedSearch: TStrings;
  outLines: TLineWriter): Integer;
var
  K, K1, K2: Integer;
  J, J1, J2: Integer;
  //C, I1, I2: Integer;
  t: TDateTime;
  X: Integer;
  bFound, bFormatOut: Boolean;
  S: String;
  idxList1, idxList2, idxErrList: TIntList;
begin
  Result := 0;
  fTerminated := False;
  bFormatOut := (Length(fOutFormat) > 0);
  t := Now;
  X := 0;
  idxList1 := TIntList.Create;
  idxList2 := TIntList.Create;
  idxErrList := TIntList.Create;
  if Assigned(st1) and Assigned(st2) then
  begin
    while not srcLines.EndOfLines do
    begin
      S := srcLines.ReadLine;
      if SearchAll then
      begin
        bFound := st1.FindAllMatches(S, idxList1);
        if bFound then
          K1 := idxList1.Count
        else
          K1 := 0;
        if st2.FindAllMatches(S, idxList2) then
          K2 := idxList2.Count
        else
          K2 := 0;
{
        idxList1.Sort;
        idxList2.Sort;
        idxErrList.Clear;
        I1 := 0; I2 := 0;
        while (I1 < idxList1.Count) or (I2 < idxList2.Count)  do
        begin
          if I1 < idxList1.Count then
          begin
            if I2 < idxList2.Count then
            begin
              C := (idxList1[I1] - idxList2[I2]);
              if C < 0 then
              begin
                idxErrList.Add(idxList1[I1]);
                Inc(I1);
              end else
              if C > 0 then
              begin
                idxErrList.Add(idxList2[I2]);
                Inc(I2);
              end else // if C = 0
              begin
                Inc(I1);
                Inc(I2);
              end;
            end else // if I >= idxList2.Count and I < idxList1.Count
            begin
              idxErrList.Add(idxList1[I1]);
              Inc(I1);
            end;
          end else // if I >= idxList1.Count and I < idxList2.Count
          begin
            idxErrList.Add(idxList2[I2]);
            Inc(I2);
          end;
        end;
}
      end else
      begin
        K1 := st1.FindMatch(S, J1);
        bFound := (K1 > 0);
        if bFound then
        begin
          if idxList1.Count = 1 then
            idxList1[0] := J1
          else begin
            idxList1.Clear;
            idxList1.Add(J1);
          end;
        end;
        K2 := st2.FindMatch(S, J2);
      end;
      if (K1 <> K2) or (idxErrList.Count > 0) then
      begin
        Inc(Result);
        if bFormatOut then
        begin
          if Assigned(lstFormattedSearch) then
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, lstFormattedSearch, idxErrList))
          else
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, nil, idxErrList));
        end
        else
          outLines.WriteLine(S);
        Error('Error!');
        Break;
      end;
      if bFound then
      begin
        Inc(Result);
        if bFormatOut then
        begin
          if Assigned(lstFormattedSearch) then
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, lstFormattedSearch, idxList1))
          else
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, nil, idxList1));
        end
        else
          outLines.WriteLine(S);
      end;
      Inc(X);
      if X >= 100 then
      begin
        X := 0;
        if SecondsBetween(t, Now) >= 1 then
        begin
          DoOnProcessed(srcLines.LineIndex, Result);
          t := Now;
          if fTerminated then Break;
        end;
      end;
    end;
  end else
  if Assigned(st1) then
  begin
    while not srcLines.EndOfLines do
    begin
      S := srcLines.ReadLine;
      if SearchAll then
      begin
        bFound := st1.FindAllMatches(S, idxList1);
        if bFound then J := idxList1[0];
      end else
      begin
        K := st1.FindMatch(S, J);
        bFound := (K > 0);
        if bFound then
        begin
          if idxList1.Count = 1 then
            idxList1[0] := J
          else begin
            idxList1.Clear;
            idxList1.Add(J);
          end;
        end;
      end;
      if bFound then
      begin
        Inc(Result);
        if bFormatOut then
        begin
          if Assigned(lstFormattedSearch) then
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, lstFormattedSearch, idxList1))
          else
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, nil, idxList1));
        end
        else
          outLines.WriteLine(S);
      end;
      Inc(X);
      if X >= 100 then
      begin
        X := 0;
        if SecondsBetween(t, Now) >= 1 then
        begin
          DoOnProcessed(srcLines.LineIndex, Result);
          t := Now;
          if fTerminated then Break;
        end;
      end;
    end;
  end else
  if Assigned(st2) then
  begin
    while not srcLines.EndOfLines do
    begin
      S := srcLines.ReadLine;
      if SearchAll then
      begin
        bFound := st2.FindAllMatches(S, idxList2);
        if bFound then J := idxList2[0];
      end else
      begin
        K := st2.FindMatch(S, J);
        bFound := (K > 0);
        if bFound then
        begin
          if idxList2.Count = 1 then
            idxList2[0] := J
          else begin
            idxList2.Clear;
            idxList2.Add(J);
          end;
        end;
      end;
      if bFound then
      begin
        Inc(Result);
        if bFormatOut then
        begin
          if Assigned(lstFormattedSearch) then
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, lstFormattedSearch, idxList2))
          else
            outLines.WriteLine(FormatOut(fOutFormat, S, lstSearch, nil, idxList2));
        end
        else
          outLines.WriteLine(S);
      end;
      Inc(X);
      if X >= 100 then
      begin
        X := 0;
        if SecondsBetween(t, Now) >= 1 then
        begin
          DoOnProcessed(srcLines.LineIndex, Result);
          t := Now;
          if fTerminated then Break;
        end;
      end;
    end;
  end;
  idxList1.Free;
  idxList2.Free;
  idxErrList.Free;
end;

function TSearchClass.FormatOut(const OutFormat, S: String;
  //const SearchStr, FormattedSearchStr: String;
  lstSearch, lstFormattedSearch: TStrings;
  lstFoundIndexes: TIntList): String;
var
  I, J, K, L, OLen: Integer;
begin
  Result := '';
  OLen := Length(OutFormat);
  L := 1;
  repeat
    K := PosEx('%', OutFormat, L);
    if K = 0 then
    begin
      Result := Result + Copy(OutFormat, L, MaxInt);
      Break;
    end else
    if (K < OLen) and ((OutFormat[K+1] = 'n') or (OutFormat[K+1] = 'N')) then
    begin
      Result := Result + Copy(OutFormat, L, K-L);
      for I := 0 to lstFoundIndexes.Count-1 do
      begin
        J := lstFoundIndexes[I];
        if I > 0 then
          Result := Result + ',' + IntToStr(J+1)
        else
          Result := Result + IntToStr(J+1);
      end;
      L := K+2;
    end else
    if (K < OLen) and ((OutFormat[K+1] = 'z') or (OutFormat[K+1] = 'Z')) then
    begin
      Result := Result + Copy(OutFormat, L, K-L)+S;
      L := K+2;
    end else
    if (K < OLen) and ((OutFormat[K+1] = 's') or (OutFormat[K+1] = 'S')) then
    begin
      Result := Result + Copy(OutFormat, L, K-L);
      for I := 0 to lstFoundIndexes.Count-1 do
      begin
        J := lstFoundIndexes[I];
        if I > 0 then
          Result := Result + ',' + lstSearch[J]
        else
          Result := Result + lstSearch[J];
      end;
      L := K+2;
    end else
    if (K < OLen) and ((OutFormat[K+1] = 'f') or (OutFormat[K+1] = 'F')) then
    begin
      Result := Result + Copy(OutFormat, L, K-L);
      if Assigned(lstFormattedSearch) then
      begin
        for I := 0 to lstFoundIndexes.Count-1 do
        begin
          J := lstFoundIndexes[I];
          if I > 0 then
            Result := Result + ',' + lstFormattedSearch[J]
          else
            Result := Result + lstFormattedSearch[J];
        end;
      end;
      L := K+2;
    end else
    begin
      Result := Result + Copy(OutFormat, L, K+1-L);
      L := K+1;
    end;
  until L > OLen;
end;

end.
