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

type

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
function GetAppFullName: string;
function GetAppBuildDate: string;
function GetAppTarget: string;
function GetFileVersion: string;

implementation

uses
{$IfDef Windows}
  winpeimagereader,
{$Else}
  elfreader,
{$EndIf}
  Classes, SysUtils, resource, versionresource, Forms, InterfaceBase, strconsts;

const
  WidgetSetNames: array [TLCLPlatform] of string = (
    'gtk',
    'gtk2',
    'gtk3',
    'win32/win64',
    'wince',
    'carbon',
    'qt',
    'fpgui',
    'nogui',
    'cocoa',
    'customdrawn'
    );
  AppVersionKey = 'ProductVersion';
  FileVersionKey = 'FileVersion';
  BuildDate = {$I %DATE%};
  BuildTime = {$I %TIME%};
  TargetOS = {$I %FPCTARGETOS%};
  TargetCPU = {$I %FPCTARGETCPU%};
  TargetFmt = '%s-%s-%s';

var
  AppVersion: string = '';
  FileVersion: string = '';

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
      AResources.LoadFromFile(Application.ExeName, AReader);
      AVersion := nil;
      for I := 0 to AResources.Count - 1 do
        if AResources[i] is TVersionResource then
          AVersion := AResources[i] as TVersionResource;
      AppVersion := SDefaultVersion;
      if AVersion <> nil then
      begin
        AppVersion := SeekForVersion(AppVersionKey);
        FileVersion := SeekForVersion(FileVersionKey);
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
  // Major
  Result := CompareInt(Ver1.Major, Ver2.Major);
  if Result <> 0 then
    Exit;
  // Minor
  Result := CompareInt(Ver1.Minor, Ver2.Minor);
  if Result <> 0 then
    Exit;
  // Release
  Result := CompareInt(Ver1.Release, Ver2.Release);
  if Result <> 0 then
    Exit;
  // Build
  Result := CompareInt(Ver1.Build, Ver2.Build);
  if Result <> 0 then
    Exit;
  // Tag
  Result := CompareStr(Ver1.Tag, Ver2.Tag);
end;

function GetAppVersion: string;
begin
  if AppVersion = '' then
    LoadVersionInfo;
  Result := AppVersion;
end;

function GetAppFullName: string;
begin
  Result := Format(SFullAppNameFmt, [Application.Title, GetAppVersion]);
end;

function GetAppBuildDate: string;
var
  DateFmt: TFormatSettings;
begin
  DateFmt := DefaultFormatSettings;
  DateFmt.ShortDateFormat := 'y/m/d';
  DateFmt.DateSeparator := '/';
  Result := FormatDateTime(SDateViewFmt, StrToDateTime(BuildDate, DateFmt)) +
    ' ' + BuildTime;
end;

function GetAppTarget: string;
begin
  Result := LowerCase(Format(TargetFmt, [TargetCPU, TargetOS,
    WidgetSetNames[WidgetSet.LCLPlatform]]));
end;

function GetFileVersion: string;
begin
  if FileVersion = '' then
    LoadVersionInfo;
  Result := FileVersion;
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
  Result := StrToFileVersion(GetFileVersion);
end;

end.
