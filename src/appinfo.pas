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
unit appinfo;

{$mode objfpc}{$H+}

interface

function GetAppFullName: string;
function GetAppBuildDate: string;
function GetAppTarget: string;

implementation

uses
  SysUtils, Forms, InterfaceBase, strconsts, versioninfo;

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
  BuildDate = {$I %DATE%};
  BuildTime = {$I %TIME%};
  TargetOS = {$I %FPCTARGETOS%};
  TargetCPU = {$I %FPCTARGETCPU%};
  TargetFmt = '%s-%s-%s';

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

