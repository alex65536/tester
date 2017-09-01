{
  This file is part of Tester

  Copyright (C) 2017 Kernozhitsky Alexander <sh200105@mail.ru>

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit versioninfo;

{$mode objfpc}{$H+}

// Thanks to lazresexplorer (a Lazarus example) for the way to get the version
// info and for code to do it. The code was taken from there and modified.

interface

uses
  SysUtils;

type
  EVersionInfo = class(Exception);

  { TFileVersion }

  TFileVersion = class
  private
    FBuild: integer;
    FMajor: integer;
    FMinor: integer;
    FRelease: integer;
    FTag: string;
    procedure SetBuild(AValue: integer);
    procedure SetMajor(AValue: integer);
    procedure SetMinor(AValue: integer);
    procedure SetRelease(AValue: integer);
    procedure SetTag(AValue: string);
  public
    constructor Create;
    procedure FillZero;
    function ToString: ansistring; override;
    class function Current: TFileVersion;
  published
    property Major: integer read FMajor write SetMajor;
    property Minor: integer read FMinor write SetMinor;
    property Release: integer read FRelease write SetRelease;
    property Build: integer read FBuild write SetBuild;
    property Tag: string read FTag write SetTag;
  end;

function StrToFileVersion(Str: string): TFileVersion;
function CompareFileVersions(Ver1, Ver2: TFileVersion): integer;

function GetAppVersion: string;
function GetAppFileVersion: string;

procedure InitVersionInfo;

implementation

uses
{$IfDef Windows}
  winpeimagereader,
{$Else}
  elfreader,
{$EndIf}
  Classes, Forms, resource, versionresource, strconsts;

const
  AppVersionKey = 'ProductVersion';
  AppFileVersionKey = 'FileVersion';

var
  AppVersion: string = '';
  AppFileVersion: string = '';

procedure LoadVersionInfo;
var
  AResources: TResources;
  AReader: TAbstractResourceReader;
  AVersion: TVersionResource;
  I: integer;

  function SeekForVersion(const AKey: string): string;
  var
    I: integer;
    Str: string;
  begin
    Result := '';
    for I := 0 to AVersion.StringFileInfo.Count - 1 do
    begin
      try
        Str := AVersion.StringFileInfo.Items[I].Values[AKey];
      except
        Str := '';
      end;
      if Str <> '' then
        Result := Str;
    end;
  end;

begin
  {$IfDef Windows}
  AReader := TWinPEImageResourceReader.Create;
  {$Else}
  AReader := TElfResourceReader.Create;
  {$EndIf}
  try
    AResources := TResources.Create;
    try
      AResources.LoadFromFile(ParamStr(0), AReader);
      AVersion := nil;
      for I := 0 to AResources.Count - 1 do
        if AResources[i] is TVersionResource then
          AVersion := AResources[i] as TVersionResource;
      AppVersion := SDefaultVersion;
      if AVersion <> nil then
      begin
        AppVersion := SeekForVersion(AppVersionKey);
        AppFileVersion := SeekForVersion(AppFileVersionKey);
      end;
    finally
      FreeAndNil(AResources);
    end;
  finally
    FreeAndNil(AReader);
  end;
end;

function StrToFileVersion(Str: string): TFileVersion;

  procedure RaiseException;
  begin
    raise EConvertError.CreateFmt(SCoundNotConvert, ['string', 'TFileVersion']);
  end;

  function CutBySeparator(const Separator: string; var S: string): integer;
  var
    P: integer;
  begin
    P := Pos(Separator, S);
    if P = 0 then
      RaiseException;
    Result := StrToInt(Copy(S, 1, P-1));
    Delete(S, 1, P);
  end;

begin
  Result := TFileVersion.Create;
  try
    Result.Major := CutBySeparator('.', Str);
    Result.Minor := CutBySeparator('.', Str);
    Result.Release := CutBySeparator('.', Str);
    if Pos('-', Str) = 0 then
      Str := Str + '-';
    Result.Build := CutBySeparator('-', Str);
    Result.Tag := Str;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function CompareFileVersions(Ver1, Ver2: TFileVersion): integer;

  function CompareInt(Int1, Int2: integer): integer; inline;
  begin
    if Int1 < Int2 then
      Result := -1
    else if Int1 > Int2 then
      Result := +1
    else
      Result := 0;
  end;

begin
  Result := 0;
  // major
  Result := CompareInt(Ver1.Major, Ver2.Major);
  if Result <> 0 then
    Exit;
  // minor
  Result := CompareInt(Ver1.Minor, Ver2.Minor);
  if Result <> 0 then
    Exit;
  // release
  Result := CompareInt(Ver1.Release, Ver2.Release);
  if Result <> 0 then
    Exit;
  // we do not compare by build!!!
  // tag
  Result := CompareStr(Ver1.Tag, Ver2.Tag);
end;

function GetAppVersion: string;
begin
  Result := AppVersion;
end;

function GetAppFileVersion: string;
begin
  Result := AppFileVersion;
end;

procedure InitVersionInfo;
begin
  try
    LoadVersionInfo;
  finally
    if (AppVersion = '') or (AppFileVersion = '') then
      raise EVersionInfo.Create(SUnableToLoadVersionInfo);
  end;
end;

{ TFileVersion }

procedure TFileVersion.SetBuild(AValue: integer);
begin
  if FBuild = AValue then Exit;
  FBuild := AValue;
end;

procedure TFileVersion.SetMajor(AValue: integer);
begin
  if FMajor = AValue then Exit;
  FMajor := AValue;
end;

procedure TFileVersion.SetMinor(AValue: integer);
begin
  if FMinor = AValue then Exit;
  FMinor := AValue;
end;

procedure TFileVersion.SetRelease(AValue: integer);
begin
  if FRelease = AValue then Exit;
  FRelease := AValue;
end;

procedure TFileVersion.SetTag(AValue: string);
begin
  if FTag = AValue then Exit;
  FTag := AValue;
end;

constructor TFileVersion.Create;
begin
  FillZero;
end;

procedure TFileVersion.FillZero;
begin
  Major := 0;
  Minor := 0;
  Release := 0;
  Build := 0;
  Tag := '';
end;

function TFileVersion.ToString: ansistring;
begin
  Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
  if Tag <> '' then
    Result := Result + '-' + Tag;
end;

class function TFileVersion.Current: TFileVersion;
begin
  Result := StrToFileVersion(GetAppFileVersion);
end;

end.
