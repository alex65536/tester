{
  This file is part of Tester

  Copyright (C) 2017-2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit parserutils;

{$mode objfpc}{$H+}{$coperators on}{$inline on}

interface

uses
  Classes, SysUtils, testerprimitives;

function FromCppFormat(const S: string): string;
function FromRoiFormat(const S: string): string;
function StrToTimeLimit(S: string; const DefaultSuff: string = 'ms'): TProblemTime;
function StrToMemoryLimit(S: string; const DefaultSuff: string = 'B'): TProblemMemory;

implementation

// Made to parse Polygon's test templates
// For example: %02d.in => %.2d.in
function FromCppFormat(const S: string): string;
var
  I: integer;
begin
  Result := S;
  for I := 2 to Length(S) do
    if (S[I] = '0') and (S[I - 1] = '%') then
      Result[I] := '.';
end;

// Made to parse ROI's test templates
// For example: input##.txt => input%.3d.txt
function FromRoiFormat(const S: string): string;
var
  I: integer;
  Sharps: integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if S[I] = '#' then
    begin
      Sharps := 0;
      while (S[I] = '#') and (I <= Length(S)) do
      begin
        Inc(I);
        Inc(Sharps);
      end;
      Result += Format('%%.%dd', [Sharps]);
    end
    else if S[I] = '%' then
    begin
      Result += '%%';
      Inc(I);
    end
    else
    begin
      Result += S[I];
      Inc(I);
    end;
  end;
end;

// Suffix utils, necessary for StrToTimeLimit and StrToMemoryLimit.

function HasSuffix(const Suff, Str: string): boolean; inline;
begin
  if Length(Suff) > Length(Str) then
    Result := False
  else
    Result := Copy(Str, Length(Str) - Length(Suff) + 1, Length(Suff)) = Suff;
end;

procedure CutSuffix(const Suff: string; var Str: string); inline;
begin
  if Suff = '' then
    Exit;
  if HasSuffix(Suff, Str) then
    Delete(Str, Length(Str) - Length(Suff) + 1, Length(Suff))
  else
    raise Exception.Create('Internal Tester Error : CutSuffix has nothing to cut');
end;

procedure TrySuffix(var Suff: string; const TrySuff, Str: string); inline;
begin
  if (Suff = '') and (HasSuffix(TrySuff, Str)) then
    Suff := TrySuff;
end;

function StrToTimeLimit(S: string; const DefaultSuff: string): TProblemTime;
var
  Suff: string;
  FloatTime: double;
begin
  S := Trim(LowerCase(S));
  // parse suffix
  Suff := '';
  TrySuffix(Suff, 'ms', S); // milliseconds
  TrySuffix(Suff, 's', S); // seconds
  CutSuffix(Suff, S);
  if Suff = '' then
    Suff := DefaultSuff;
  // extract floating point time value
  FloatTime := StrToFloat(S);
  // return time (in milliseconds)
  if Suff = 'ms' then
    Result := Round(FloatTime)
  else if Suff = 's' then
    Result := Round(FloatTime * 1000)
  else
    raise Exception.Create('Internal Tester Error : StrToTimeLimit got bad suffix');
end;

function StrToMemoryLimit(S: string; const DefaultSuff: string): TProblemMemory;
var
  Suff: string;
  FloatMemory: double;
begin
  S := Trim(UpperCase(S));
  // parse suffix
  Suff := '';
  TrySuffix(Suff, 'K', S); // in KBytes
  TrySuffix(Suff, 'M', S); // in MBytes
  TrySuffix(Suff, 'G', S); // in GBytes
  CutSuffix(Suff, S);
  if Suff = '' then
    Suff := DefaultSuff;
  // extract floating point time value
  FloatMemory := StrToFloat(S);
  // return memory (in Kbytes)
  if Suff = 'B' then
    Result := Round(FloatMemory / 1024)
  else if Suff = 'K' then
    Result := Round(FloatMemory)
  else if Suff = 'M' then
    Result := Round(FloatMemory * 1024)
  else if Suff = 'G' then
    Result := Round(FloatMemory * 1048576)
  else
    raise Exception.Create('Internal Tester Error : StrToMemoryLimit got bad suffix');
end;

end.

