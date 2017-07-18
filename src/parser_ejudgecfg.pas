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
unit parser_ejudgecfg;

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, propsparserbase, IniFiles, testerfileutil, FileUtil,
  LazFileUtils, logfile, parserutils, checkers, compilers, testtemplates,
  problemprops;

type

  { TEjudgeCfgPropertiesParser }

  TEjudgeCfgPropertiesParser = class(TPropertiesParserBase)
  private
    procedure AddTest(ATest: TProblemTest);
    procedure SearcherFileFound(AIterator: TFileIterator);
    function DetectChecker(const CheckerName: string): boolean;
    function DetectTests(InputTemplate, OutputTemplate: string): boolean;
    function Unquote(const S: string): string;
    function PreprocessIniFile(const AFileName: string): TStream;
    function Parser(IniFile: TIniFile): boolean;
  protected
    function DoParse: boolean; override;
  end;

implementation

{ TEjudgeCfgPropertiesParser }

procedure TEjudgeCfgPropertiesParser.AddTest(ATest: TProblemTest);
begin
  Properties.AddTest(ATest);
end;

procedure TEjudgeCfgPropertiesParser.SearcherFileFound(AIterator: TFileIterator);
begin
  if IsTerminated then
    AIterator.Stop;
end;

function TEjudgeCfgPropertiesParser.DetectChecker(const CheckerName: string): boolean;
var
  Files: TStringList;
  CurFile, CurFileNoExt, Extension: string;
  CheckerFileName: string;

  function Detector(CanCompile: boolean): boolean;
  begin
    Result := False;
    for CurFile in Files do
    begin
      CurFileNoExt := ExtractFileNameWithoutExt(ExtractFileNameOnly(CurFile));
      Extension := LowerCase(ExtractFileExt(CurFile));
      WriteLog('checker detector: *' + Extension);
      WriteLog('checker detector: ' + CurFile + ' ' + CurFileNoExt);
      // if the file hs wrong name - it's not checker
      if LowerCase(CurFileNoExt) <> LowerCase(CheckerName) then
        Continue;
      // if it has right name - try to use it as a checker or compile it
      if {$IfNDef Windows} (Extension = '') or {$EndIf}
        (Extension = '.exe') then
        CheckerFileName := CurFile
      else if CanCompile then
        CheckerFileName := CompileChecker(CurFile)
      else
        CheckerFileName := '';
      WriteLog('Found ' + CheckerFileName);
      // if checker detected - save it and exit
      if CheckerFileName <> '' then
      begin
        CheckerFileName := CreateRelativePath(CheckerFileName, WorkingDir);
        Result := True;
        Properties.Checker := TTestlibChecker.Create(CheckerFileName);
        Break;
      end;
      if IsTerminated then
        Break;
    end;
  end;

begin
  Files := FindAllFiles(@SearcherFileFound, WorkingDir, '*', False);
  try
    Result := Detector(False);
    if (not IsTerminated) and (not Result) then
      Result := Detector(True);
  finally
    FreeAndNil(Files);
  end;
end;

function TEjudgeCfgPropertiesParser.DetectTests(InputTemplate,
  OutputTemplate: string): boolean;
var
  Template: TProblemTestTemplate;
begin
  if (InputTemplate = '') or (OutputTemplate = '') then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  InputTemplate := FromCppFormat(InputTemplate);
  OutputTemplate := FromCppFormat(OutputTemplate);
  Template := TProblemTestTemplate.Create('', '', 1, 1);
  try
    Template.InputFile := 'tests' + PathDelim + InputTemplate;
    Template.OutputFile := 'tests' + PathDelim + OutputTemplate;
    try
      Template.GenerateTests(WorkingDir, @AddTest);
    except
      on E: ETestTemplate do
        Result := False;
      else
        raise;
    end;
  finally
    FreeAndNil(Template);
  end;
end;

function TEjudgeCfgPropertiesParser.Unquote(const S: string): string;
begin
  Result := S;
  if Length(S) < 2 then
    Exit;
  if ((S[1] = '''') and (S[Length(S)] = '''')) or
    ((S[1] = '"') and (S[Length(S)] = '"')) then
    Result := Copy(S, 2, Length(S) - 2);
end;

function TEjudgeCfgPropertiesParser.PreprocessIniFile(
  const AFileName: string): TStream;
var
  Lines, NewLines: TStringList;
  Line: string;
begin
  Result := nil;
  Lines := TStringList.Create;
  NewLines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    for Line in Lines do
      if (Line <> '') and (Line[1] <> '#') then
        NewLines.Add(Line);
    Result := TMemoryStream.Create;
    try
      WriteLog('%---------text is--------%');
      for Line in NewLines do
        WriteLog(Line);
      WriteLog('%-------end of text------%');
      NewLines.SaveToStream(Result);
      Result.Position := 0;
    except
      Result := nil;
    end;
  finally
    FreeAndNil(Lines);
    FreeAndNil(NewLines);
  end;
end;

function TEjudgeCfgPropertiesParser.Parser(IniFile: TIniFile): boolean;
const
  ProblemSection = 'problem';

  function ReadStrIfExists(const Section, Ident, Default: string;
  var Success: boolean): string;
  begin
    if IniFile.ValueExists(Section, Ident) then
      Result := Unquote(IniFile.ReadString(Section, Ident, Default))
    else
    begin
      Result := Default;
      Success := False;
    end;
  end;

  function ReadFloatIfExists(const Section, Ident: string; Default: double;
  var Success: boolean): double;
  begin
    if IniFile.ValueExists(Section, Ident) then
      Result := IniFile.ReadFloat(Section, Ident, Default)
    else
    begin
      Result := Default;
      Success := False;
    end;
  end;

var
  TLFloat: double;
  MLString: string;
  CheckerStr: string;
  InputTemplate, OutputTemplate: string;
begin
  Result := True;
  // parse input file
  if IniFile.ReadInteger(ProblemSection, 'use_stdin', 1) = 0 then
    Properties.InputFile := ReadStrIfExists(ProblemSection, 'input_file',
      'stdin', Result)
  else
    Properties.InputFile := 'stdin';
  if IsTerminated then
    Exit;
  // parse output file
  if IniFile.ReadInteger(ProblemSection, 'use_stdout', 1) = 0 then
    Properties.OutputFile := ReadStrIfExists(ProblemSection, 'output_file',
      'stdout', Result)
  else
    Properties.OutputFile := 'stdout';
  if IsTerminated then
    Exit;
  // parse time limit
  try
    TLFloat := ReadFloatIfExists(ProblemSection, 'time_limit', 2, Result);
    Properties.TimeLimit := Round(TLFloat * 1000);
  except
    Result := False;
  end;
  if IsTerminated then
    Exit;
  // parse memory limit
  try
    MLString := ReadStrIfExists(ProblemSection, 'max_vm_size', '64M', Result);
    Properties.MemoryLimit := StrToMemoryLimit(MLString);
  except
    Result := False;
  end;
  if IsTerminated then
    Exit;
  // parse checker
  if IniFile.ValueExists(ProblemSection, 'standard_checker') then
    Properties.Checker := TFileCompareChecker.Create
  else
  begin
    CheckerStr := ReadStrIfExists(ProblemSection, 'check_cmd', '', Result);
    if CheckerStr <> '' then
    begin
      CheckerStr := ExtractFileNameWithoutExt(CheckerStr);
      WriteLog('Checker Str is ' + CheckerStr);
      if not DetectChecker(CheckerStr) then
        Result := False;
    end
    else
      Result := False;
  end;
  if IsTerminated then
    Exit;
  // parse tests
  InputTemplate := ReadStrIfExists(ProblemSection, 'test_pat', '', Result);
  OutputTemplate := ReadStrIfExists(ProblemSection, 'corr_pat', '', Result);
  if not DetectTests(InputTemplate, OutputTemplate) then
    Result := False;
end;

function TEjudgeCfgPropertiesParser.DoParse: boolean;
var
  CfgFileName: string;
  IniStream: TStream;
  IniFile: TIniFile;
begin
  Result := True;
  CfgFileName := CorrectFileName(AppendPathDelim(WorkingDir) + 'problem.cfg');
  if not FileExistsUTF8(CfgFileName) then
    Exit;
  IniStream := PreprocessIniFile(CfgFileName);
  try
    IniFile := TIniFile.Create(IniStream);
    try
      Result := Parser(IniFile);
    finally
      FreeAndNil(IniFile);
    end;
  finally
    FreeAndNil(IniStream);
  end;
end;

end.
