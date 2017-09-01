{
  This file is part of Tester

  Copyright (C) 2017 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit fcutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, strconsts;

const
  DiffViewDistance = 20;

procedure StripList(List: TStringList; StripSpaces: boolean);
function ExtractTextPart(const Text: string; Col: integer): string;
function CompareLine(const OutLine, AnsLine: string; out Col: integer;
  out Reason: string): boolean;
function CompareText(OutText, AnsText: TStringList; out Line, Col: integer;
  out Reason: string): boolean;

implementation

procedure StripList(List: TStringList; StripSpaces: boolean);
var
  I: integer;
  Pos: integer;
  S: string;
begin
  // strip trailing spaces
  if StripSpaces then
  begin
    for I := 0 to List.Count - 1 do
    begin
      S := List[I];
      Pos := Length(S);
      while (Pos > 0) and (S[Pos] = ' ') do
        Dec(Pos);
      List[I] := Copy(S, 1, Pos);
    end;
  end;
  // strip trailing newlines
  while (List.Count > 0) and (List[List.Count - 1] = '') do
    List.Delete(List.Count - 1);
end;

function ExtractTextPart(const Text: string; Col: integer): string;
var
  L, R: integer;
  DotsBeg, DotsEnd: boolean;
begin
  // calc L
  L := Col - DiffViewDistance;
  DotsBeg := L > 1;
  if L < 1 then
    L := 1;
  // calc R
  R := Col + DiffViewDistance;
  DotsEnd := R < Length(Text);
  if R > Length(Text) then
    R := Length(Text);
  // copy string
  Result := Copy(Text, L, R - L + 1);
  if DotsBeg then
    Result := '...' + Result;
  if DotsEnd then
    Result := Result + '...';
end;

function CompareLine(const OutLine, AnsLine: string; out Col: integer;
  out Reason: string): boolean;
var
  I: integer;
  OutLen, AnsLen: integer;
begin
  Col := 0;
  Reason := '';
  Result := False;
  OutLen := Length(OutLine);
  AnsLen := Length(AnsLine);
  // check differences character by character
  for I := 1 to Min(OutLen, AnsLen) do
    if OutLine[I] <> AnsLine[I] then
    begin
      Col := I;
      Reason := Format(SFilesCharsDiffer, [ExtractTextPart(AnsLine, Col),
        ExtractTextPart(OutLine, Col)]);
      Exit;
    end;
  // check if the line lengths are equal
  if OutLen < AnsLen then
  begin
    Col := OutLen + 1;
    Reason := SFilesLineTooShort;
    Exit;
  end;
  if OutLen > AnsLen then
  begin
    Col := AnsLen + 1;
    Reason := SFilesLineTooLong;
    Exit;
  end;
  // it's OK, the lines are equal
  Result := True;
end;

function CompareText(OutText, AnsText: TStringList; out Line, Col: integer;
  out Reason: string): boolean;
var
  I: integer;
  OutLen, AnsLen: integer;
begin
  Line := 0;
  Col := 0;
  Reason := '';
  Result := False;
  OutLen := OutText.Count;
  AnsLen := AnsText.Count;
  // compare line by line
  for I := 0 to Min(OutLen, AnsLen) - 1 do
    if not CompareLine(OutText[I], AnsText[I], Col, Reason) then
    begin
      Line := I + 1;
      Exit;
    end;
  // check if the line counts are equal
  if OutLen < AnsLen then
  begin
    Line := OutLen + 1;
    Col := 1;
    Reason := SFilesFileTooShort;
    Exit;
  end;
  if OutLen > AnsLen then
  begin
    Line := AnsLen + 1;
    Col := 1;
    Reason := SFilesFileTooLong;
    Exit;
  end;
  // it's OK, the lines are equal
  Result := True;
end;

end.
