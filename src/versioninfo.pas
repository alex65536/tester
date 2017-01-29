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

// Thanks to lazresexplorer (a Lazarus exampler) for the way to get the version
// info and for code to do it. The code was taken from there and modified.

interface

function GetAppVersion: string;
function GetAppFullName: string;
function GetAppBuildDate: string;
function GetAppTarget: string;

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
  BuildDate = {$I %DATE%};
  BuildTime = {$I %TIME%};
  TargetOS = {$I %FPCTARGETOS%};
  TargetCPU = {$I %FPCTARGETCPU%};
  TargetFmt = '%s-%s-%s';

var
  AppVersion: string = '';

procedure LoadAppVersion;
var
  AResources: TResources;
  AReader: TAbstractResourceReader;
  AVersion: TVersionResource;
  I: integer;
  Str: string;
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
        for I := 0 to AVersion.StringFileInfo.Count - 1 do
        begin
          try
            Str := AVersion.StringFileInfo.Items[I].Values[AppVersionKey];
          except
            Str := '';
          end;
          if Str <> '' then
            AppVersion := Str;
        end;
      end;
    finally
      FreeAndNil(AResources);
    end;
  finally
    FreeAndNil(AReader);
  end;
end;

function GetAppVersion: string;
begin
  if AppVersion = '' then
    LoadAppVersion;
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

end.
