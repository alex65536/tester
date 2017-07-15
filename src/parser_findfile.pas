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
unit parser_findfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testtemplates, problemprops, propsparserbase, FileUtil,
  LazFileUtils, logfile, testerfileutil, checkers, compilers;

type

  { TFindFilePropertiesParser }

  TFindFilePropertiesParser = class(TPropertiesParserBase)
  private
    procedure AddTest(ATest: TProblemTest);
    function ParseFromDir(const Dir: string): boolean;
    function FindTests(const Dir: string; StartFrom: integer = 1): boolean;
    function FindChecker(const Dir: string): boolean;
  protected
    function DoParse: boolean; override;
  end;


procedure AddTestTemplate(const InputFile, OutputFile: string);

implementation

var
  InputTemplate, OutputTemplate: TStringList;

procedure AddTestTemplate(const InputFile, OutputFile: string);
begin
  InputTemplate.Add(InputFile);
  OutputTemplate.Add(OutputFile);
end;

{ TFindFilePropertiesParser }

procedure TFindFilePropertiesParser.AddTest(ATest: TProblemTest);
begin
  Properties.AddTest(ATest);
end;

function TFindFilePropertiesParser.ParseFromDir(const Dir: string): boolean;
var
  WasTests: integer;
  AList: TStringList;
  I: integer;
  CurDir: string;
  LowerCurDir: string;
begin
  Result := True;
  WasTests := Properties.TestCount;
  if not FindChecker(Dir) then
    Result := False;
  if IsTerminated then
    Exit;
  if not FindTests(Dir, Properties.TestCount - WasTests + 1) then
    Result := False;
  if IsTerminated then
    Exit;
  AList := FindAllDirectories(AppendPathDelim(WorkingDir) + Dir, False);
  try
    AList.Sort;
    for I := 0 to AList.Count - 1 do
    begin
      CurDir := AppendPathDelim(Dir) + ExtractFileName(AList[I]);
      WriteLog('curInsideDir = ' + CurDir);
      LowerCurDir := LowerCase(ExtractFileName(AList[I]));
      if (Pos('group', LowerCurDir) = 1) or (Pos('subtask', LowerCurDir) = 1) or
        (Pos('subproblem', LowerCurDir) = 1) then
      begin
        if not FindTests(CurDir, Properties.TestCount - WasTests + 1) then
          Result := False;
      end;
      if IsTerminated then
        Break;
    end;
  finally
    FreeAndNil(AList);
  end;
end;

function TFindFilePropertiesParser.FindTests(const Dir: string;
  StartFrom: integer): boolean;
var
  ATemplate: TProblemTestTemplate;
  I: integer;
begin
  Result := True;
  ATemplate := TProblemTestTemplate.Create('', '', 1, 1);
  try
    for I := 0 to InputTemplate.Count - 1 do
    begin
      ATemplate.InputFile := AppendPathDelim(Dir) + InputTemplate[I];
      ATemplate.OutputFile := AppendPathDelim(Dir) + OutputTemplate[I];
      ATemplate.StartFrom := StartFrom;
      WriteLogFmt('Gen by template: %s %s', [ATemplate.InputFile, ATemplate.OutputFile]);
      try
        ATemplate.GenerateTests(WorkingDir, @AddTest);
      except
        on E: ETestTemplate do
          // mute the exception
        else
          Result := False;
      end;
      if IsTerminated then
        Break;
    end;
  finally
    FreeAndNil(ATemplate);
  end;
end;

function TFindFilePropertiesParser.FindChecker(const Dir: string): boolean;
var
  AList: TStringList;
  I: integer;
  CurFile, CompiledFile: string;
begin
  Result := True;
  AList := FindAllFiles(nil, AppendPathDelim(WorkingDir) + Dir, '*', False);
  try
    // first, search for EXE checker
    for I := 0 to AList.Count - 1 do
    begin
      CurFile := LowerCase(ExtractFileName(AList[I]));
      // check if it's checker
      if (CurFile = 'check.exe') or (CurFile = 'checker.exe')
      {$IfNDef Windows}
        or (CurFile = 'check') or (CurFile = 'checker')
      {$EndIf}
      then
      begin
        Properties.Checker :=
          TTextChecker.Create(CreateRelativePath(AList[I], WorkingDir));
        Properties.Checker.Replaceable := True;
      end;
      // if checker was found - we break
      if Properties.Checker <> nil then
        Break;
      if IsTerminated then
        Break;
    end;
    // otherwise, we should build the checker from sources
    if Properties.Checker = nil then
    begin
      for I := 0 to AList.Count - 1 do
      begin
        CurFile := LowerCase(ExtractFileNameWithoutExt(ExtractFileName(AList[I])));
        // check if it's checker
        if (CurFile = 'check') or (CurFile = 'checker') then
        begin
          // we build it from sources
          CompiledFile := CompileChecker(AList[I]);
          if CompiledFile <> '' then
          begin
            CompiledFile := CreateRelativePath(CompiledFile, WorkingDir);
            Properties.Checker := TTextChecker.Create(CompiledFile);
            Properties.Checker.Replaceable := True;
          end;
        end;
        // if checker was found - we break
        if Properties.Checker <> nil then
          Break;
        if IsTerminated then
          Break;
      end;
    end;
  finally
    FreeAndNil(AList);
  end;
end;

function TFindFilePropertiesParser.DoParse: boolean;
var
  AList: TStringList;
  I: integer;
  CurDir: string;
begin
  Properties.TestList.LowPriority := True;
  Result := ParseFromDir('');
  AList := FindAllDirectories(WorkingDir, False);
  try
    for I := 0 to AList.Count - 1 do
    begin
      CurDir := ExtractFileName(AList[I]);
      WriteLog('curDir = ' + CurDir);
      if (LowerCase(CurDir) = 'tests') or (LowerCase(CurDir) = 'test') or
        (LowerCase(CurDir) = 'testset') then
      begin
        if not ParseFromDir(CurDir) then
          Result := False;
      end;
      if IsTerminated then
        Break;
    end;
  finally
    FreeAndNil(AList);
  end;
end;

initialization
  InputTemplate := TStringList.Create;
  OutputTemplate := TStringList.Create;
  // 01.in - 01.out
  AddTestTemplate('%d.in', '%d.out');
  AddTestTemplate('%.2d.in', '%.2d.out');
  // 01 - 01.a
  AddTestTemplate('%d', '%d.a');
  AddTestTemplate('%.2d', '%.2d.a');
  AddTestTemplate('%.3d', '%.3d.a');
  // 01.in - 01.ans
  AddTestTemplate('%d.in', '%d.ans');
  AddTestTemplate('%.2d.in', '%.2d.ans');
  // 01.dat - 01.ans
  AddTestTemplate('%d.dat', '%d.ans');
  AddTestTemplate('%.2d.dat', '%.2d.ans');
  // 01.dat - 01.sol
  AddTestTemplate('%d.dat', '%d.sol');
  AddTestTemplate('%.2d.dat', '%.2d.sol');
  // input01.txt - output01.txt
  AddTestTemplate('input%d.txt', 'output%d.txt');
  AddTestTemplate('input%.2d.txt', 'output%.2d.txt');
  // input01.txt - answer01.txt
  AddTestTemplate('input%d.txt', 'answer%d.txt');
  AddTestTemplate('input%.2d.txt', 'answer%.2d.txt');
  // in.1 - out.1
  AddTestTemplate('in.%d', 'out.%d');
  AddTestTemplate('in.%.2d', 'out.%.2d');

finalization
  FreeAndNil(InputTemplate);
  FreeAndNil(OutputTemplate);

end.
