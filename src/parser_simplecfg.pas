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
unit parser_simplecfg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemprops, checkers, propsparserbase, FileUtil,
  LazFileUtils, testerfileutil, compilers;

type

  { TSimpleCfgPropertiesParser }

  TSimpleCfgPropertiesParser = class(TPropertiesParserBase)
  private
    function DelSeparators(const S: string): string;
    procedure ParseChecker(ACheckerSrc: string; var Success: boolean);
    function Parser(ALines: TStringList): boolean;
  protected
    function DoParse: boolean; override;
  end;

implementation

{ TSimpleCfgPropertiesParser }

function TSimpleCfgPropertiesParser.DelSeparators(const S: string): string;
const
  Letters = ['A' .. 'Z', 'a' .. 'z', '0' .. '9'];
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in Letters then
      Result := Result + S[I];
end;

procedure TSimpleCfgPropertiesParser.ParseChecker(ACheckerSrc: string;
  var Success: boolean);
var
  CheckerExe: string;
begin
  ACheckerSrc := CorrectFileName(AppendPathDelim(WorkingDir) + ACheckerSrc);
  CheckerExe := CompileChecker(ACheckerSrc);
  if CheckerExe <> '' then
    Properties.Checker :=
      TTestlibChecker.Create(CreateRelativePath(CheckerExe, WorkingDir))
  else
    Success := False;
end;

function TSimpleCfgPropertiesParser.Parser(ALines: TStringList): boolean;
var
  I, P: integer;
  AName, AValue: string;
  TestCount: integer;
  TestDir: string;
begin
  Result := True;
  TestCount := 0;
  for I := 0 to ALines.Count - 1 do
  begin
    // determine name and value
    P := Pos('=', ALines[I]);
    if P = 0 then
      Continue;
    AName := Copy(ALines[I], 1, P - 1);
    AValue := Copy(ALines[I], P + 1, Length(ALines[I]) - P);
    AName := LowerCase(DelSeparators(AName));
    AValue := Trim(AValue);
    // parse value by name
    try
      if AName = 'timelimit' then
        Properties.TimeLimit := StrToInt(AValue)
      else if AName = 'memorylimit' then
        Properties.MemoryLimit := StrToInt(AValue)
      else if AName = 'inputfile' then
        Properties.InputFile := AValue
      else if AName = 'outputfile' then
        Properties.OutputFile := AValue
      else if AName = 'checker' then
        ParseChecker(AValue, Result)
      else if AName = 'testscount' then
        TestCount := StrToInt(AValue);
    except
      Result := False;
    end;
    if IsTerminated then
      Break;
  end;
  TestDir := AppendPathDelim('tests');
  if not IsTerminated then
    AddTestsFmt(TestDir + '%d.in', TestDir + '%d.ans', TestCount);
end;

function TSimpleCfgPropertiesParser.DoParse: boolean;
var
  Lines: TStringList;
  CfgFileName: string;
begin
  Result := True;
  CfgFileName := CorrectFileName(AppendPathDelim(WorkingDir) + 'package.cfg');
  if not FileExistsUTF8(CfgFileName) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(CfgFileName);
    Result := Parser(Lines);
  finally
    FreeAndNil(Lines);
  end;
end;

end.
