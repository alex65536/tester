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
unit verdictcolors;

{$mode objfpc}{$H+}

interface

uses
  Graphics, testerprimitives;

const
  CompilerVerdictColors: array [TCompilerVerdict] of TColor =
    (clGreen, clRed, clBlack, clGray);
  TestVerdictColors: array [TTestVerdict] of TColor = (clGreen, clRed, clMaroon,
    clBlack, clPurple, clBlue, clTeal, clNavy, clBlack, clBlack, clGray);

function GetTotalScoreColor(Cur, Max: double): TColor;
function GetTotalScoreColor(Ratio: double): TColor;

implementation

function GetTotalScoreColor(Cur, Max: double): TColor;
begin
  if Max = 0 then
    Result := clGray
  else
    Result := GetTotalScoreColor(Cur / Max);
end;

function GetTotalScoreColor(Ratio: double): TColor;
begin
  if Ratio < 1 / 3 then
    Result := RGBToColor(255, round(128 * Ratio * 3), 0)
  else
    Result := RGBToColor(round(255 * (1 - Ratio) * 1.5), 128, 0);
end;

end.

