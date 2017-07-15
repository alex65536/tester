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
unit parserutils;

{$mode objfpc}{$H+}{$coperators on}

interface

uses
  Classes, SysUtils, testerprimitives;

function FromCppFormat(const S: string): string;
function FromRoiFormat(const S: string): string;
function StrToTimeLimit(S: string): TProblemTime;

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

function StrToTimeLimit(S: string): TProblemTime;
var
  Postfix: string;
  FloatTime: double;
begin
  // parse postfix
  Postfix := 'ms';
  S := LowerCase(S);
  if Pos('ms', S) = Length(S) - 1 then
  begin
    Postfix := 'ms';
    Delete(S, Length(S) - 1, 2);
  end
  else if Pos('s', S) = Length(S) then
  begin
    Postfix := 's';
    Delete(S, Length(S), 1);
  end;
  // extract floating point time value
  FloatTime := StrToFloat(S);
  // return time (in milliseconds)
  if Postfix = 'ms'
  then
    Result := Round(FloatTime)
  else
    Result := Round(FloatTime * 1000);
end;

end.

