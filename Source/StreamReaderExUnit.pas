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

unit StreamReaderExUnit;

interface

uses
  System.SysUtils, System.Classes;

type
  TStringBuilderEx = class(TStringBuilder)
  public
    property Data: TCharArray read FData;
  end;

  TStreamReaderEx = class(TTextReader)
  private
    FBufferedData: TStringBuilderEx;
    FBufferedStartIndex: Integer;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FEncoding: TEncoding;
    FNoDataInStream: Boolean;
    FOwnsStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TStream;
    FTruncLineLength: Integer;
    FSkipLineNullChars: Boolean;
    function DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding;
      DiscardBufferedStart: Boolean);
    function GetEndOfStream: Boolean;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
    function GetNextLineIndex(out PostNewLineIndex: Integer): Integer;
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; DetectBOM: Boolean); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding;
      DetectBOM: Boolean = False; BufferSize: Integer = 1024); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string; Encoding: TEncoding;
      DetectBOM: Boolean = False; BufferSize: Integer = 1024); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure DiscardBufferedData;
    procedure OwnStream; inline;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
{$IFDEF VER230} //Delphi XE2
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
{$ELSE}
    function Read(var Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(var Buffer: TCharArray; Index, Count: Integer): Integer; override;
{$ENDIF}
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    procedure GotoNextLine;
    property BaseStream: TStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
    property TruncLineLength: Integer read FTruncLineLength write FTruncLineLength;
    property SkipLineNullChars: Boolean read FSkipLineNullChars write FSkipLineNullChars;
  end;

implementation

uses
{$IFDEF DEBUG}
  Winapi.Windows,
{$ENDIF}
  System.RTLConsts;

{ TStreamReaderEx }

constructor TStreamReaderEx.Create(Stream: TStream);
begin
  Create(Stream, TEncoding.UTF8, True);
end;

constructor TStreamReaderEx.Create(Stream: TStream; DetectBOM: Boolean);
begin
  Create(Stream, TEncoding.UTF8, DetectBOM);
end;

constructor TStreamReaderEx.Create(Stream: TStream; Encoding: TEncoding;
  DetectBOM: Boolean = False; BufferSize: Integer = 1024);
begin
  inherited Create;

  if not Assigned(Stream) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Stream']); // DO NOT LOCALIZE
  if not Assigned(Encoding) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  FBufferedData := TStringBuilderEx.Create;
  FBufferedStartIndex := 0;
  FEncoding := Encoding;
  FBufferSize := BufferSize;
  if FBufferSize < 128 then
    FBufferSize := 128;
  FNoDataInStream := False;
  FStream := Stream;
  FOwnsStream := False;
  FDetectBOM := DetectBOM;
  FSkipPreamble := not FDetectBOM;
  FTruncLineLength := 65535;
  FSkipLineNullChars := True;
end;

constructor TStreamReaderEx.Create(const Filename: string; Encoding: TEncoding;
  DetectBOM: Boolean = False; BufferSize: Integer = 1024);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), Encoding, DetectBOM, BufferSize);
  FOwnsStream := True;
end;

constructor TStreamReaderEx.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), DetectBOM);
  FOwnsStream := True;
end;

constructor TStreamReaderEx.Create(const Filename: string);
begin
  Create(TFileStream.Create(Filename, fmOpenRead));
  FOwnsStream := True;
end;

destructor TStreamReaderEx.Destroy;
begin
  Close;
  inherited;
end;

procedure TStreamReaderEx.Close;
begin
  if (FStream <> nil) and FOwnsStream then
  begin
    FStream.Free;
    FStream := nil;
  end;

  if FBufferedData <> nil then
  begin
    FBufferedData.Free;
    FBufferedData := nil;
    FBufferedStartIndex := 0;
  end;
end;

procedure TStreamReaderEx.DiscardBufferedData;
begin
  if FBufferedData <> nil then
  begin
    FBufferedData.Remove(0, FBufferedData.Length);
    FBufferedStartIndex := 0;
    FNoDataInStream := False;
  end;
end;

function TStreamReaderEx.DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
var
  LEncoding: TEncoding;
begin
  // try to automatically detect the buffer encoding
  LEncoding := nil;
  Result := TEncoding.GetBufferEncoding(Buffer, LEncoding);

  // detected encoding points to Default and param Encoding requests some other
  // type of Encoding; set the Encoding param to UTF8 as it can also read ANSI (Default)
  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
    Encoding := TEncoding.UTF8
  else
    Encoding := LEncoding;

  FDetectBOM := False;
end;

procedure TStreamReaderEx.FillBuffer(var Encoding: TEncoding;
  DiscardBufferedStart: Boolean);
const
  BufferPadding = 4;
var
  LString: string;
  LBuffer: TBytes;
  BytesRead: Integer;
  StartIndex: Integer;
  ByteCount: Integer;
  ByteBufLen: Integer;
  ExtraByteCount: Integer;
begin
  SetLength(LBuffer, FBufferSize + BufferPadding);

  // Read data from stream
  BytesRead := FStream.Read(LBuffer[0], FBufferSize);
  FNoDataInStream := BytesRead < FBufferSize;

  // Check for byte order mark and calc start index for character data
  if FDetectBOM then
    StartIndex := DetectBOM(Encoding, LBuffer)
  else if FSkipPreamble then
    StartIndex := SkipPreamble(Encoding, LBuffer)
  else
    StartIndex := 0;

  // Convert to string and calc byte count for the string
  ByteBufLen := BytesRead - StartIndex;
  LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
  ByteCount := FEncoding.GetByteCount(LString);

  // If byte count <> number of bytes read from the stream
  // the buffer boundary is mid-character and additional bytes
  // need to be read from the stream to complete the character
  ExtraByteCount := 0;
  while (ByteCount <> ByteBufLen) and (ExtraByteCount < FEncoding.GetMaxByteCount(1)) do
  begin
    // Expand buffer if padding is used
    if (StartIndex + ByteBufLen) = Length(LBuffer) then
      SetLength(LBuffer, Length(LBuffer) + BufferPadding);

    // Read one more byte from the stream into the
    // buffer padding and convert to string again
    BytesRead := FStream.Read(LBuffer[StartIndex + ByteBufLen], 1);
    if BytesRead = 0 then
      // End of stream, append what's been read and discard remaining bytes
      Break;

    Inc(ExtraByteCount);

    Inc(ByteBufLen);
    LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
    ByteCount := FEncoding.GetByteCount(LString);
  end;

  if DiscardBufferedStart and (FBufferedStartIndex > 0) then
  begin
    FBufferedData.Remove(0, FBufferedStartIndex);
    FBufferedStartIndex := 0;
  end;

  // Add string to character data buffer
  FBufferedData.Append(LString);
end;

function TStreamReaderEx.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (FBufferedData <> nil) and //(FBufferedData.Length < 1)
    (FBufferedStartIndex >= FBufferedData.Length)
  then
    FillBuffer(FEncoding, False);
  Result := FNoDataInStream and ((FBufferedData = nil) or //(FBufferedData.Length = 0));
    (FBufferedStartIndex >= FBufferedData.Length));
end;

function TStreamReaderEx.GetNextLineIndex(out PostNewLineIndex: Integer): Integer;
var
  NewLineIndex: Integer;
  NextLineIndex: Integer;
  PBufferedData: ^TCharArray;
  LBufferedData: Integer;
  Ch: Char;
begin
  Result := -1;
  if FBufferedData = nil then
    Exit;
  //NewLineIndex := 0;
  //PostNewLineIndex := 0;
  NewLineIndex := FBufferedStartIndex;
  PostNewLineIndex := FBufferedStartIndex;
  if FTruncLineLength > 0 then
    NextLineIndex := FBufferedStartIndex + FTruncLineLength
  else
    NextLineIndex := -1;

  PBufferedData := @FBufferedData.Data;
  LBufferedData := FBufferedData.Length;

  while True do
  begin
    if (NewLineIndex + 2 > LBufferedData) and (not FNoDataInStream) then
    begin
      Dec(NewLineIndex, FBufferedStartIndex);
      Dec(PostNewLineIndex, FBufferedStartIndex);
      FillBuffer(FEncoding, True);
      Inc(NewLineIndex, FBufferedStartIndex);
      Inc(PostNewLineIndex, FBufferedStartIndex);
      if FTruncLineLength > 0 then
        NextLineIndex := FBufferedStartIndex + FTruncLineLength;
      PBufferedData := @FBufferedData.Data;
      LBufferedData := FBufferedData.Length;
    end;

    if NewLineIndex >= LBufferedData then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        Dec(NewLineIndex, FBufferedStartIndex);
        Dec(PostNewLineIndex, FBufferedStartIndex);
        FillBuffer(FEncoding, True);
        Inc(NewLineIndex, FBufferedStartIndex);
        Inc(PostNewLineIndex, FBufferedStartIndex);
        if FTruncLineLength > 0 then
          NextLineIndex := FBufferedStartIndex + FTruncLineLength;
        PBufferedData := @FBufferedData.Data;
        LBufferedData := FBufferedData.Length;
        //if LBufferedData = 0 then
        if FBufferedStartIndex >= LBufferedData then
          Break;
      end;
    end;

    Ch := PBufferedData^[NewLineIndex];

    if Ch = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end
    else
    if Ch = #13 then
    begin
      if {(Ch = #13) and }(NewLineIndex + 1 < LBufferedData) and (PBufferedData^[NewLineIndex + 1] = #10) then
      begin
        PostNewLineIndex := NewLineIndex + 2;
        Break;
      end
      else
      //if Ch = #13 then
      begin
        PostNewLineIndex := NewLineIndex + 1;
        Break;
      end;
    end;

    if NewLineIndex = NextLineIndex then
    begin
      PostNewLineIndex := NewLineIndex;
      Break;
    end;

    if (NewLineIndex > FBufferedStartIndex) and
       FSkipLineNullChars then
    begin
      if Ch = #0 then
      begin
        if PBufferedData^[NewLineIndex-1] <> #0 then
        begin
          PostNewLineIndex := NewLineIndex;
          Break;
        end;
      end else
      //if Ch <> #0 then
      begin
        if PBufferedData^[NewLineIndex-1] = #0 then
        begin
          PostNewLineIndex := NewLineIndex;
          Break;
        end;
      end;
    end;

    Inc(NewLineIndex);
  end;

  Result := NewLineIndex;
end;

procedure TStreamReaderEx.GotoNextLine;
var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;
begin
  NewLineIndex := GetNextLineIndex(PostNewLineIndex);
  if NewLineIndex < 0 then Exit;
  FBufferedStartIndex := PostNewLineIndex;
end;

procedure TStreamReaderEx.OwnStream;
begin
  FOwnsStream := True;
end;

function TStreamReaderEx.Peek: Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    //if FBufferedData.Length < 1 then
    if (FBufferedStartIndex >= FBufferedData.Length) then
      FillBuffer(FEncoding, False);
    //Result := Integer(FBufferedData.Chars[0]);
    Result := Integer(FBufferedData.Chars[FBufferedStartIndex]);
  end;
end;

function TStreamReaderEx.Read(
{$IFDEF VER230} //Delphi XE2
  const Buffer: TCharArray;
{$ELSE}
  var Buffer: TCharArray;
{$ENDIF}
  Index, Count: Integer): Integer;
var
  LDiscardBufferedStart: Boolean;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    LDiscardBufferedStart := True;
    while //(FBufferedData.Length < Count) and
      (FBufferedData.Length-FBufferedStartIndex < Count) and
      (not EndOfStream) and (not FNoDataInStream) do
    begin
      FillBuffer(FEncoding, LDiscardBufferedStart);
      LDiscardBufferedStart := False;
    end;

    //if FBufferedData.Length > Count then
    //  Result := Count
    //else
    //  Result := FBufferedData.Length;

    //FBufferedData.CopyTo(0, Buffer, Index, Result);
    //FBufferedData.Remove(0, Result);

    if FBufferedData.Length-FBufferedStartIndex > Count then
      Result := Count
    else
      Result := FBufferedData.Length-FBufferedStartIndex;

    FBufferedData.CopyTo(FBufferedStartIndex, Buffer, Index, Result);
    Inc(FBufferedStartIndex, Result);
  end;
end;

function TStreamReaderEx.ReadBlock(
{$IFDEF VER230} //Delphi XE2
  const Buffer: TCharArray;
{$ELSE}
  var Buffer: TCharArray;
{$ENDIF}
  Index, Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TStreamReaderEx.Read: Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    //if FBufferedData.Length < 1 then
    if (FBufferedStartIndex >= FBufferedData.Length) then
      FillBuffer(FEncoding, True);
    //Result := Integer(FBufferedData.Chars[0]);
    //FBufferedData.Remove(0, 1);
    Result := Integer(FBufferedData.Chars[FBufferedStartIndex]);
    Inc(FBufferedStartIndex);
  end;
end;

function TStreamReaderEx.ReadLine: string;
var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;
  ResultLength: Integer;
begin
  NewLineIndex := GetNextLineIndex(PostNewLineIndex);
  if NewLineIndex < 0 then
  begin
    Result := '';
    Exit;
  end;
  ResultLength := NewLineIndex-FBufferedStartIndex;
  if (FTruncLineLength > 0) and (ResultLength > FTruncLineLength) then
    ResultLength := FTruncLineLength;
  Result := FBufferedData.ToString(FBufferedStartIndex, ResultLength);
  FBufferedStartIndex := PostNewLineIndex;
end;

function TStreamReaderEx.ReadToEnd: string;
var
  LDiscardBufferedStart: Boolean;
begin
  Result := '';
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    LDiscardBufferedStart := True;
    repeat
      FillBuffer(FEncoding, LDiscardBufferedStart);
      LDiscardBufferedStart := False;
    until FNoDataInStream;
    //Result := FBufferedData.ToString;
    //FBufferedData.Remove(0, FBufferedData.Length);
    Result := FBufferedData.ToString(FBufferedStartIndex, FBufferedData.Length-FBufferedStartIndex);
    FBufferedStartIndex := FBufferedData.Length;
  end;
end;

function TStreamReaderEx.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
var
  I: Integer;
  LPreamble: TBytes;
  BOMPresent: Boolean;
begin
  Result := 0;
  LPreamble := Encoding.GetPreamble;
  if (Length(LPreamble) > 0) then
  begin
    if Length(Buffer) >= Length(LPreamble) then
    begin
      BOMPresent := True;
      for I := 0 to Length(LPreamble) - 1 do
        if LPreamble[I] <> Buffer[I] then
        begin
          BOMPresent := False;
          Break;
        end;
      if BOMPresent then
        Result := Length(LPreamble);
    end;
  end;
  FSkipPreamble := False;
end;

end.
