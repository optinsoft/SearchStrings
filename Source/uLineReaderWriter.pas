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

unit uLineReaderWriter;

{$IF not Defined(FPC) and Defined(MSWINDOWS) }
{$DEFINE STEX}
{$IFEND}

interface

uses
  {$IFDEF FPC}
  streamex,
  {$ENDIF}
  Classes,
  {$IFDEF STEX}
  StreamReaderExUnit,
  {$ENDIF}
  SysUtils,
  BufferedFileStreamUnit;

type
  PRejectChars = ^TRejectChars;
  TRejectChars = array [Char] of Char;

  TLineReader = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FLines: TStrings;
    {$IFDEF STEX}
    FStreamReader: TStreamReaderEx;
    {$ELSE}
    FStreamReader: TStreamReader;
    {$ENDIF}
    FLineIndex: Integer;
{$IF not Defined(FPC) and Defined(MSWINDOWS) }
    FEncoding: TEncoding;
{$IFEND}
    FData: Pointer;
    procedure InitReader;
    function GetEndOfLines: Boolean;
    function GetStreamPosition: Int64;
    function GetStreamSize: Int64;
  public
    class procedure SetRejectChars(const AllowedChars: String);
    class procedure ResetRejectChars;
    constructor Create(Stream: TStream; BufferSize: Integer = 1024); overload;
    constructor Create(const Filename: String; BufferSize: Integer = 1024); overload;
    constructor Create(Lines: TStrings); overload;
    destructor Destroy; override;
    function ReadLine: String;
    procedure Close;
    procedure GotoToBegin;
    procedure GotoNextLine;
    property EndOfLines: Boolean read GetEndOfLines;
    property LineIndex: Integer read FLineIndex;
    property StreamSize: Int64 read GetStreamSize;
    property StreamPosition: Int64 read GetStreamPosition;
    property Data: Pointer read FData write FData;
  end;

  TLineWriter = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FLines: TStrings;
    FEncoding: TEncoding;
    FOwnsEncoding: Boolean;
    FLineIndex: Integer;
    FAutoFlush: Boolean;
    FBuffer: TBytes;
    FBufferIndex: Integer;
    FLineBreak: String;
    FWriteBOM: Boolean;
    FLastWriteTime: TDateTime;
    FFileName: String;
    FTempFileName: String;
    FData: Pointer;
    procedure InitWriter(Encoding: TEncoding);
    procedure WriteBytes(Bytes: TBytes);
  public
    constructor Create(Stream: TStream; Append: Boolean = False; Encoding: TEncoding = nil); overload;
    constructor Create(const Filename: String; Append: Boolean = False; const TempFileExt: String = '';
      Encoding: TEncoding = nil); overload;
    constructor Create(Lines: TStrings; Append: Boolean = False; Encoding: TEncoding = nil); overload;
    destructor Destroy; override;
    procedure WriteLine(const S: String);
    procedure WriteLines(Lines: TStrings);
    procedure Flush;
    procedure Close;
    procedure Clear;
    property LineIndex: Integer read FLineIndex;
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush;
    property LineBreak: String read FLineBreak write FLineBreak;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
    property LastWriteTime: TDateTime read FLastWriteTime;
    property Data: Pointer read FData write FData;
  end;

  TCachedFileLineReader = class(TLineReader)
  public
    constructor Create(const Filename: String; CacheSize: Integer = 0;
      BufferSize: Integer = 1024); overload;
  end;

  TCachedFileLineWriter = class(TLineWriter)
  public
    constructor Create(const Filename: String; Append: Boolean = False; const TempFileExt: String = '';
      Encoding: TEncoding = nil; CacheSize: Integer = 0); overload;
  end;

implementation

//Fix: Remove MB_ERR_INVALID_CHARS flag from TUTF8Encoding
//See: http://qc.embarcadero.com/wc/qcmain.aspx?d=79042
//http://stackoverflow.com/questions/35855459/default-tencoding-utf8-discarding-invalid-blocks-of-data-in-tstreamreader-input

{$IF not Defined(FPC) and Defined(MSWINDOWS) }
uses
  Winapi.Windows;

type
  TFixedUTF8Encoding = class(TUTF7Encoding)
  public
    constructor Create; override;
    function Clone: TEncoding; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;

{ TUTF8EncodingEx }

constructor TFixedUTF8Encoding.Create;
begin
  inherited Create(CP_UTF8, 0, 0);
  FIsSingleByte := False;
end;

function TFixedUTF8Encoding.Clone: TEncoding;
begin
  Result := TUTF8Encoding.Create;
end;

function TFixedUTF8Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 3;
end;

function TFixedUTF8Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount + 1;
end;

function TFixedUTF8Encoding.GetPreamble: TBytes;
begin
  Result := TBytes.Create($EF, $BB, $BF);
end;
{$IFEND}

{ Global Variables }

var
  FDoRejectChars: Boolean = False;
  FRejectChars: TRejectChars;

function DoRejectChars(var S: String): Integer;
var
  I, J, L: Integer;
begin
  J := 0;
  L := Length(S);
  for I := 1 to L do
  begin
    if FRejectChars[S[I]] = #0 then
    begin
      Inc(J);
      if J <> I then S[J] := S[I];
    end;
  end;
  Result := L-J;
  if Result > 0 then
    SetLength(S, J);
end;

{ TLineReader }

class procedure TLineReader.SetRejectChars(const AllowedChars: String);
var
  I: Integer;
  c, c1, c2: Char;
  arrAllowedChars: array[Char] of Char;
begin
  FDoRejectChars := True;
  FillChar(FRejectChars, sizeof(FRejectChars), 0);
  if Length(AllowedChars) = 0 then Exit;
  FillChar(arrAllowedChars, sizeof(arrAllowedChars), 0);
  I := 1;
  while I <= Length(AllowedChars) do
  begin
    c1 := AllowedChars[I];
    Inc(I);
    if c1 = '\' then
    begin
      if I > Length(AllowedChars) then Break;
      c1 := AllowedChars[I];
      case c1 of
        't': c1 := #9;
        'n': c1 := #10;
        'r': c1 := #13;
      end;
      Inc(I);
    end;
    c2 := c1;
    if (I < Length(AllowedChars)) and
       (AllowedChars[I] = '-') then
    begin
      Inc(I); //skip '-', I <= Length(AStr)
      c2 := AllowedChars[I];
      Inc(I);
      if c2 = '\' then
      begin
        if I > Length(AllowedChars) then Break;
        c2 := AllowedChars[I];
        case c2 of
          't': c2 := #9;
          'n': c2 := #10;
          'r': c2 := #13;
        end;
        Inc(I);
      end;
    end;
    for c := c1 to c2 do
    begin
      arrAllowedChars[c] := #1;
    end;
  end;
  for c := Low(arrAllowedChars) to High(arrAllowedChars) do
  begin
    if arrAllowedChars[c] = #0 then
    begin
      //reject
      FRejectChars[c] := #1;
    end;
  end;
end;

procedure TLineReader.Close;
begin
  FLines := nil;
  if Assigned(FStreamReader) then
    FreeAndNil(FStreamReader);
  if FOwnsStream and
     Assigned(FStream)
  then
    FreeAndNil(FStream);
end;

constructor TLineReader.Create(Stream: TStream; BufferSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FOwnsStream := False;
{$IF not Defined(FPC) and Defined(MSWINDOWS) }
  FEncoding := TFixedUTF8Encoding.Create;
{$IFDEF STEX}
  FStreamReader := TStreamReaderEx.Create(FStream, FEncoding, False, BufferSize);
{$ELSE}
  FStreamReader := TStreamReader.Create(FStream, FEncoding, False, BufferSize);
{$ENDIF}
{$ELSE}
  FStreamReader := TStreamReader.Create(FStream);
{$IFEND}
  FLines := nil;
  InitReader;
end;

constructor TLineReader.Create(const Filename: String; BufferSize: Integer);
begin
  inherited Create;
  FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  FOwnsStream := True;
{$IF not Defined(FPC) and Defined(MSWINDOWS) }
  FEncoding := TFixedUTF8Encoding.Create;
{$IFDEF STEX}
  FStreamReader := TStreamReaderEx.Create(FStream, FEncoding, False, BufferSize);
{$ELSE}
  FStreamReader := TStreamReader.Create(FStream, FEncoding, False, BufferSize);
{$ENDIF}
{$ELSE}
  FStreamReader := TStreamReader.Create(FStream);
{$IFEND}
  FLines := nil;
  InitReader;
end;

constructor TLineReader.Create(Lines: TStrings);
begin
  inherited Create;
  FStream := nil;
  FOwnsStream := False;
{$IF not Defined(FPC) and Defined(MSWINDOWS) }
  FEncoding := TFixedUTF8Encoding.Create;
{$IFEND}
  FStreamReader := nil;
  FLines := Lines;
  InitReader;
end;

destructor TLineReader.Destroy;
begin
  Close;
{$IF not Defined(FPC) and Defined(MSWINDOWS) }
  FEncoding.Free;
{$IFEND}
  inherited;
end;

function TLineReader.GetEndOfLines: Boolean;
begin
  if Assigned(FStreamReader) then
    {$IFDEF FPC}
    Result := FStreamReader.IsEof
    {$ELSE}
    Result := FStreamReader.EndOfStream
    {$ENDIF}
  else if Assigned(FLines) then
    Result := (FLineIndex >= FLines.Count)
  else
    Result := True;
end;

function TLineReader.GetStreamPosition: Int64;
begin
  if Assigned(FStream) then
    Result := FStream.Position
  else
    Result := 0;
end;

function TLineReader.GetStreamSize: Int64;
begin
  if Assigned(FStream) then
    Result := FStream.Size
  else
    Result := 0;
end;

procedure TLineReader.GotoNextLine;
begin
  if Assigned(FStreamReader) then
  begin
    {$IFDEF FPC}
    if not FStreamReader.IsEof
    {$ELSE}
    if not FStreamReader.EndOfStream
    {$ENDIF}
    then begin
      FStreamReader.GotoNextLine;
      Inc(FLineIndex);
      Exit;
    end;
  end else
  if Assigned(FLines) then
  begin
    if FLineIndex < FLines.Count then
    begin
      Inc(FLineIndex);
      Exit;
    end;
  end;
end;

procedure TLineReader.GotoToBegin;
begin
  if Assigned(FStream) then
    FStream.Seek(0, soBeginning)
  else if Assigned(FLines) then
    FLineIndex := 0;
  if Assigned(FStreamReader) then
    FStreamReader.DiscardBufferedData;
end;

procedure TLineReader.InitReader;
begin
  FLineIndex := 0;
  FData := nil;
end;

function TLineReader.ReadLine: String;
begin
  if Assigned(FStreamReader) then
  begin
    {$IFDEF FPC}
    if not FStreamReader.IsEof
    {$ELSE}
    if not FStreamReader.EndOfStream
    {$ENDIF}
    then begin
      Result := FStreamReader.ReadLine;
      if FDoRejectChars then
        DoRejectChars(Result);
      Inc(FLineIndex);
      Exit;
    end;
  end else
  if Assigned(FLines) then
  begin
    if FLineIndex < FLines.Count then
    begin
      Result := FLines[FLineIndex];
      if FDoRejectChars then
        DoRejectChars(Result);
      Inc(FLineIndex);
      Exit;
    end;
  end;
  Result := '';
end;

class procedure TLineReader.ResetRejectChars;
begin
  FDoRejectChars := False;
end;

{ TLineWriter }

procedure TLineWriter.Clear;
begin
  if Assigned(FStream) then
    FStream.Size := 0
  else if Assigned(FLines) then
    FLines.Clear;
end;

procedure TLineWriter.Close;
begin
  Flush;
  FLines := nil;
  if FOwnsStream and
     Assigned(FStream) then
  begin
    FreeAndNil(FStream);
    if (Length(FTempFileName) > 0) and
       (Length(FFileName) > 0) then
    begin
      if FileExists(FFileName) then
        DeleteFile(PChar(FFileName));
      if FileExists(FTempFileName) then
        RenameFile(FTempFileName, FFileName);
      FTempFileName := '';
    end;
  end;
end;

constructor TLineWriter.Create(Stream: TStream; Append: Boolean; Encoding: TEncoding);
begin
  inherited Create;
  FFileName := '';
  FTempFileName := '';
  FStream := Stream;
  FOwnsStream := False;
  FLines := nil;
  InitWriter(Encoding);
  if not Assigned(FStream) then Exit;
  if not Append then
  begin
    if FStream.Size > 0 then
      FStream.Size := 0;
  end;
end;

constructor TLineWriter.Create(const Filename: String; Append: Boolean;
  const TempFileExt: String; Encoding: TEncoding);
var
  sFileName: String;
begin
  inherited Create;
  FFileName := Filename;
  if Length(TempFileExt) > 0 then
  begin
    FTempFileName := Filename+TempFileExt;
    if FileExists(FTempFileName) then
      DeleteFile(PChar(FTempFileName));
    if Append then
    begin
      if FileExists(FFileName) then
        CopyFile(PChar(FFileName), PChar(FTempFileName), False);
    end;
    sFileName := FTempFileName;
  end else
  begin
    FTempFileName := '';
    sFileName := FFileName;
  end;
  if Append and FileExists(sFilename) then
  begin
    FStream := TFileStream.Create(sFileName, fmOpenReadWrite or fmShareDenyWrite);
    FStream.Seek(0, soFromEnd);
  end else
    FStream := TFileStream.Create(sFileName, fmCreate or fmShareDenyWrite);
  FOwnsStream := True;
  FLines := nil;
  InitWriter(Encoding);
end;

constructor TLineWriter.Create(Lines: TStrings; Append: Boolean; Encoding: TEncoding);
begin
  inherited Create;
  FFileName := '';
  FTempFileName := '';
  FStream := nil;
  FOwnsStream := False;
  FLines := Lines;
  InitWriter(Encoding);
  if not Assigned(FLines) then Exit;
  if not Append then
  begin
    if FLines.Count > 0 then
      FLines.Clear;
  end;
end;

destructor TLineWriter.Destroy;
begin
  Close;
  if FOwnsEncoding then
  begin
    if Assigned(FEncoding) then
      FEncoding.Free;
  end;
  inherited;
end;

procedure TLineWriter.Flush;
var
  Preamble: TBytes;
begin
  if FBufferIndex = 0 then Exit;
  if FStream = nil then Exit;
  if FWriteBOM and (FStream.Position = 0) then
  begin
    Preamble := FEncoding.GetPreamble;
    if Length(Preamble) > 0 then
      FStream.WriteBuffer(Preamble[0], Length(Preamble));
  end;
  FStream.WriteBuffer(FBuffer[0], FBufferIndex);
  FBufferIndex := 0;
end;

procedure TLineWriter.InitWriter(Encoding: TEncoding);
begin
  if Assigned(Encoding) then
  begin
    FEncoding := Encoding;
    FOwnsEncoding := False;
  end else begin
{$IF not Defined(FPC) and Defined(MSWINDOWS) }
    FEncoding := TFixedUTF8Encoding.Create;
    FOwnsEncoding := True;
{$ELSE}
    FEncoding := TEncoding.UTF8;
    FOwnsEncoding := False;
{$IFEND}
  end;
  FLineIndex := 0;
  FAutoFlush := False;
  SetLength(FBuffer, 1024);
  FBufferIndex := 0;
  FLineBreak := sLineBreak;
  FWriteBOM := False;
  FLastWriteTime := Now;
  FData := nil;
end;

procedure TLineWriter.WriteBytes(Bytes: TBytes);
var
  ByteIndex: Integer;
  WriteLen: Integer;
begin
  ByteIndex := 0;

  while ByteIndex < Length(Bytes) do
  begin
    WriteLen := Length(Bytes) - ByteIndex;
    if WriteLen > Length(FBuffer) - FBufferIndex then
      WriteLen := Length(FBuffer) - FBufferIndex;

    Move(Bytes[ByteIndex], FBuffer[FBufferIndex], WriteLen);

    Inc(FBufferIndex, WriteLen);
    Inc(ByteIndex, WriteLen);

    if FBufferIndex >= Length(FBuffer) then
      Flush;
  end;

  if FAutoFlush then
    Flush;
end;

procedure TLineWriter.WriteLine(const S: String);
begin
  if Assigned(FStream) then
  begin
    WriteBytes(FEncoding.GetBytes(S + FLineBreak));
    Inc(FLineIndex);
  end else
  if Assigned(FLines) then
  begin
    FLines.Add(S);
    Inc(FLineIndex);
  end;
  FLastWriteTime := Now;
end;

procedure TLineWriter.WriteLines(Lines: TStrings);
var
  I, N: Integer;
begin
  N := Lines.Count;
  for I := 0 to N-1 do
    WriteLine(Lines[I]);
end;

{ TCachedFileLineReader }

constructor TCachedFileLineReader.Create(const Filename: String;
  CacheSize, BufferSize: Integer);
var
  CachedFileStream: TReadOnlyCachedFileStream;
begin
  CachedFileStream := TReadOnlyCachedFileStream.Create(Filename, CacheSize);
  inherited Create(CachedFileStream, BufferSize);
  FOwnsStream := True;
end;

{ TCachedFileLineWriter }

constructor TCachedFileLineWriter.Create(const Filename: String;
  Append: Boolean; const TempFileExt: String; Encoding: TEncoding;
  CacheSize: Integer);
var
  CachedFileStream: TWriteCachedFileStream;
  sFileName: String;
  bTempFile: Boolean;
begin
  bTempFile := (Length(TempFileExt) > 0);
  if bTempFile then
  begin
    sFileName := Filename+TempFileExt;
    if FileExists(sFileName) then
      DeleteFile(PChar(sFileName));
    if Append and FileExists(FileName) then
      CopyFile(PChar(FileName), PChar(sFileName), False);
  end else
    sFileName := FileName;
  CachedFileStream := TWriteCachedFileStream.Create(sFilename, Append, CacheSize);
  inherited Create(CachedFileStream, Append, Encoding);
  FOwnsStream := True;
  FFileName := Filename;
  if bTempFile then
    FTempFileName := sFileName;
end;

end.
