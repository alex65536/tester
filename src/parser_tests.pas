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
unit parser_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testtemplates, problemprops, propsparserbase, FileUtil,
  LazFileUtils, logfile;

type

  { TTestProblemPropertiesParser }

  TTestProblemPropertiesParser = class(TPropertiesParserBase)
  private
    procedure AddTest(ATest: TProblemTest);
    function ParseFromDir(const Dir: string): boolean;
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

{ TTestProblemPropertiesParser }

procedure TTestProblemPropertiesParser.AddTest(ATest: TProblemTest);
begin
  Properties.AddTest(ATest);
end;

function TTestProblemPropertiesParser.ParseFromDir(const Dir: string): boolean;
var
  ATemplate: TProblemTestTemplate;
  I: integer;
  WasEmpty: boolean;
begin
  Result := True;
  ATemplate := TProblemTestTemplate.Create('', '', 1, 1);
  try
    for I := 0 to InputTemplate.Count - 1 do
    begin
      ATemplate.InputFile := AppendPathDelim(Dir) + InputTemplate[I];
      ATemplate.OutputFile := AppendPathDelim(Dir) + OutputTemplate[I];
      try
        WasEmpty := Properties.TestCount = 0;
        ATemplate.GenerateTests(WorkingDir, @AddTest);
        if (Properties.TestCount <> 0) and (not WasEmpty) then
          Result := False;
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

function TTestProblemPropertiesParser.DoParse: boolean;
var
  AList: TStringList;
  I: integer;
  CurDir: string;
begin
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
  // in.1 - out.1
  AddTestTemplate('in.%d', 'out.%d');
  AddTestTemplate('in.%.2d', 'out.%.2d');

finalization
  FreeAndNil(InputTemplate);
  FreeAndNil(OutputTemplate);

end.

