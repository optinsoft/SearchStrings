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

unit uAppFolder;

interface

uses Winapi.Windows, System.SysUtils, Winapi.SHFolder, Vcl.Forms, System.StrUtils;

function GetAppDataDir(
  const SubFolder: String;
  const ConfigFiles: String = ''): String;
function GetAppConfigFile(
  const DefAppConfigFile: String;
  const AppDataDir: String): String;

implementation

var
  //0 - undefined, 1 - use app settings dir, 2 - custom settings dir, -1 - use user app data settings dir
  AppSettingsDir: Integer = 0;
  AppConfigFile: PChar = '';
  AppSettingsFolder: PChar = '';
  bufAppConfigFile: array[0..MAX_PATH] of Char;
  bufAppSettingsFolder: array[0..MAX_PATH] of Char;

function GetAppConfigFile(const DefAppConfigFile, AppDataDir: String): String;
var
  i: Integer;
begin
  if AppSettingsDir = 0 then
  begin
    AppSettingsDir := -1;
    i := 1;
    while i <= ParamCount do
    begin
      if ParamStr(i) = '-appsettingsdir' then
        AppSettingsDir := 1
      else
      if (ParamStr(i) = '-settingsdir') and
         (i+1 <= ParamCount) then
      begin
        Inc(i);
        bufAppSettingsFolder[0] := #0;
        StrPLCopy(bufAppSettingsFolder, ParamStr(i), MAX_PATH);
        bufAppSettingsFolder[MAX_PATH] := #0;
        AppSettingsFolder := bufAppSettingsFolder;
        AppSettingsDir := 2;
      end else
      if (ParamStr(i) = '-config') and
         (i+1 <= ParamCount) then
      begin
        Inc(i);
        bufAppConfigFile[0] := #0;
        StrPLCopy(bufAppConfigFile, ParamStr(i), MAX_PATH);
        bufAppConfigFile[MAX_PATH] := #0;
        AppConfigFile := bufAppConfigFile;
        if (AppSettingsDir = -1) and (AppConfigFile^ <> #0) then
          AppSettingsDir := 1;
      end;
      Inc(i);
    end;
  end;
  if AppConfigFile^ <> #0 then
    Result := AppConfigFile
  else
    Result := DefAppConfigFile;
  if Pos(':', Result) = 0 then
  begin
    if Length(AppDataDir) > 0 then
      Result := AppDataDir + '\' + Result
    else if (AppSettingsDir = 2) and (AppSettingsFolder^ <> #0) then
      Result := AppSettingsFolder + '\' + Result;
  end;
end;

function GetAppDataDir(const SubFolder: String;
  const ConfigFiles: String = ''): String;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0..MAX_PATH] of char;
  t, sAppDataFolder: String;
  sSrcFile, sDstFile: String;
  sr: TSearchRec;
  i, k, l: Integer;
begin
  Result := ExtractFileDir(Application.ExeName);
  if AppSettingsDir = 0 then
  begin
    AppSettingsDir := -1;
    i := 1;
    while i <= ParamCount do
    begin
      if ParamStr(i) = '-appsettingsdir' then
        AppSettingsDir := 1
      else
      if (ParamStr(i) = '-settingsdir') and
         (i+1 <= ParamCount) then
      begin
        Inc(i);
        bufAppSettingsFolder[0] := #0;
        StrPLCopy(bufAppSettingsFolder, ParamStr(i), MAX_PATH);
        bufAppSettingsFolder[MAX_PATH] := #0;
        AppSettingsFolder := bufAppSettingsFolder;
        AppSettingsDir := 2;
      end else
      if (ParamStr(i) = '-config') and
         (i+1 <= ParamCount) then
      begin
        Inc(i);
        bufAppConfigFile[0] := #0;
        StrPLCopy(bufAppConfigFile, ParamStr(i), MAX_PATH);
        bufAppConfigFile[MAX_PATH] := #0;
        AppConfigFile := bufAppConfigFile;
        if (AppSettingsDir = -1) and (AppConfigFile^ <> #0) then
          AppSettingsDir := 1;
      end;
      Inc(i);
    end;
  end;
  if AppSettingsDir = 1 then Exit;
  if AppSettingsDir = 2 then
  begin
    Result := AppSettingsFolder;
    Exit;
  end;
  if SUCCEEDED(SHGetFolderPath(0,CSIDL_APPDATA,0,SHGFP_TYPE_CURRENT,@path[0])) then
  begin
    sAppDataFolder := path;
    if Length(SubFolder) > 0 then
    begin
      if SubFolder[1] = '\' then
        sAppDataFolder := sAppDataFolder + SubFolder
      else
        sAppDataFolder := sAppDataFolder + '\' + SubFolder;
      if not DirectoryExists(sAppDataFolder) then
      begin
        if not CreateDir(sAppDataFolder) then Exit;
        if Length(ConfigFiles) > 0 then
        begin
          k := 1;
          repeat
            l := PosEx(';', ConfigFiles, k);
            if l >= k then
            begin
              t := Trim(Copy(ConfigFiles, k, l-k));
              k := l+1;
            end else
            begin
              t := Trim(Copy(ConfigFiles, k, MaxInt));
              k := Length(ConfigFiles)+1;
            end;
            if Length(t) > 0 then
            begin
              if FindFirst(Result+'\'+t, 0, sr) = 0 then
              begin
                repeat
                  sSrcFile := Result+'\'+sr.Name;
                  sDstFile := sAppDataFolder+'\'+sr.Name;
                  if not FileExists(sDstFile) then
                  begin
                    CopyFile(PChar(sSrcFile), PChar(sDstFile), True);
                  end;
                until FindNext(sr) <> 0;
                FindClose(sr);
              end;
            end;
          until (k > Length(ConfigFiles));
        end;
      end;
      Result := sAppDataFolder;
    end;
  end;
end;

end.
