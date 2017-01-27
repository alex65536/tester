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
unit parser_alltest;

{$mode objfpc}{$H+}{$inline on}

interface

uses
  Classes, SysUtils, propsparserbase, FileUtil, LazFileUtils, testerfileutil,
  logfile, process, problemprops, checkers;

type

  { TAllTestPropertiesParser }

  TAllTestPropertiesParser = class(TPropertiesParserBase)
  private
    FInFormat: string;
    FOutFormat: string;
    FTestIndices: TStringList;
    procedure SearcherFileFound(Iterator: TFileIterator);
    function FindAllFiles(const SearchPath, SearchMask: string;
      SearchSubDirs: boolean): TStringList;
    function ParseStringList(const WorkDir: string; AList: TStringList): boolean;
    function AddTests: boolean;
  protected
    function DoParse: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TAllTestPropertiesParser }

procedure TAllTestPropertiesParser.SearcherFileFound(Iterator: TFileIterator);
begin
  if IsTerminated then
    Iterator.Stop;
end;

function TAllTestPropertiesParser.FindAllFiles(const SearchPath, SearchMask: string;
  SearchSubDirs: boolean): TStringList;
var
  ASearcher: TListFileSearcher;
begin
  Result := TStringList.Create;
  ASearcher := TListFileSearcher.Create(Result);
  try
    ASearcher.OnFileFound := @SearcherFileFound;
    ASearcher.Search(SearchPath, SearchMask, SearchSubDirs);
  finally
    FreeAndNil(ASearcher);
  end;
end;

function TAllTestPropertiesParser.ParseStringList(const WorkDir: string;
  AList: TStringList): boolean;
var
  CmdList: TStringList;
  Success: boolean;

  function GetCmd(Index: integer): string; inline;
  begin
    if Index >= CmdList.Count then
      Result := ''
    else
      Result := CmdList[Index];
  end;

  function CompletePath(FileName: string): string; inline;
  begin
    Result := AppendPathDelim(WorkDir) + FileName;
  end;

  function CompleteAndCorrectPath(FileName: string): string; inline;
  begin
    FileName := CompletePath(FileName);
    Result := CorrectFileNameCase(FileName);
    if Result = '' then
      Result := FileName;
  end;

  function BatParamToFmtStr(S: string): string;
  var
    I: integer;
  begin
    for I := 1 to Length(S) - 1 do
      if (S[I] = '%') and (S[I + 1] <> '%') then
        S[I + 1] := 's';
    Result := CompletePath(S);
  end;

  procedure TryReplaceChecker(NewChecker: TProblemChecker);
  begin
    with TProblemPropsCollector, Self.Properties do
    begin
      Checker := MergeChecker(Checker, NewChecker, Success);
      if NewChecker <> Checker then
        FreeAndNil(NewChecker);
    end;
  end;

  procedure AddFromString(const S: string);
  const
    LetterSet = ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_'];
    DelimiterSet = [#0 .. #255] - LetterSet;
  var
    I, Pos: integer;
  begin
    I := 1;
    while I <= Length(S) do
    begin
      if S[I] in DelimiterSet then
        Inc(I)
      else
      begin
        Pos := I;
        while (I <= Length(S)) and (S[I] in LetterSet) do
          Inc(I);
        FTestIndices.Add(Copy(S, Pos, I - Pos));
      end;
    end;
    WriteLog('WasStr = ' + S);
    WriteLog('Parsed to ' + LineEnding + FTestIndices.Text);
  end;

  procedure UpdateTimer;
  begin
    with TProblemPropsCollector, Self.Properties do
    begin
      TimeLimit := MergeInt(TimeLimit, StrToInt(GetCmd(2)), Success);
      MemoryLimit := MergeInt(MemoryLimit, StrToInt(GetCmd(3)), Success);
    end;
  end;

  procedure UpdateInput;
  begin
    with TProblemPropsCollector, Self.Properties do
    begin
      FInFormat := MergeStr(FInFormat, BatParamToFmtStr(GetCmd(1)), Success);
      InputFile := MergeStr(InputFile, GetCmd(2), Success);
    end;
  end;

  procedure UpdateOutputFile(const S: string);
  begin
    with TProblemPropsCollector, Self.Properties do
    begin
      if Pos('%', S) = 0 then
        OutputFile := MergeStr(OutputFile, S, Success)
      else
        FOutFormat := MergeStr(FOutFormat, BatParamToFmtStr(S), Success);
    end;
  end;

  procedure UpdateFCChecker;
  begin
    TryReplaceChecker(TFileCompareChecker.Create);
    UpdateOutputFile(GetCmd(1));
    UpdateOutputFile(GetCmd(2));
  end;

  procedure UpdateChecker;
  var
    CheckerPath: string;
  begin
    CheckerPath := CreateRelativePath(CompleteAndCorrectPath(GetCmd(0)), WorkingDir);
    TryReplaceChecker(TTextChecker.Create(CheckerPath));
    UpdateOutputFile(GetCmd(2));
    UpdateOutputFile(GetCmd(3));
  end;

  procedure UpdateFor;
  var
    P: integer;
    CurCmd: string;
    Str: string;
  begin
    if FTestIndices.Count <> 0 then
      Success := False;
    // search for "in"
    P := 0;
    CurCmd := '';
    while True do
    begin
      CurCmd := LowerCase(GetCmd(P));
      if (CurCmd = '') or (CurCmd = 'in') then
        Break;
      Inc(P);
    end;
    if CurCmd = '' then
    begin
      Success := False;
      Exit;
    end;
    // read params before ")" (these ones will be numbers)
    Inc(P);
    Str := '';
    while True do
    begin
      CurCmd := GetCmd(P);
      Str := ' ' + CurCmd;
      if (CurCmd = '') or (Pos(')', CurCmd) <> 0) then
        Break;
      Inc(P);
    end;
    if CurCmd = '' then
    begin
      Success := False;
      Exit;
    end;
    // now, parse the string
    AddFromString(Str);
  end;

var
  I: integer;
  CurCmd: string;
begin
  WriteLog('Parsing BAT = ' + LineEnding + AList.Text);
  Success := True;
  CmdList := TStringList.Create;
  try
    // iterate through the lines and parse the commands
    for I := 0 to AList.Count - 1 do
    begin
      CmdList.Clear;
      CommandToList(AList[I], CmdList);
      CurCmd := ExtractFileName(LowerCase(GetCmd(0)));
      WriteLog('Parsing line = "' + AList[I] + '"');
      WriteLog('CurCmd = "' + CurCmd + '"');
      if (CurCmd = 'timer') or (CurCmd = 'timer.exe') or (CurCmd = 'runexe') or
        (CurCmd = 'runexe.exe') then
        UpdateTimer;
      if CurCmd = 'copy' then
        UpdateInput;
      if (CurCmd = 'fc') or (CurCmd = 'fc.exe') then
        UpdateFCChecker;
      if (CurCmd = 'checker') or (CurCmd = 'checker.exe') or
        (CurCmd = 'check.exe') or (CurCmd = 'check') then
        UpdateChecker;
      if CurCmd = 'for' then
        UpdateFor;
      if IsTerminated then
        Break;
    end;
  finally
    FreeAndNil(CmdList);
    Result := Success;
  end;
end;

function TAllTestPropertiesParser.AddTests: boolean;
var
  NewList: TProblemTestList;
  I: integer;
begin
  Result := True;
  NewList := TProblemTestList.Create;
  WriteLog('Formats: ' + FInFormat + ' ' + FOutFormat);
  try
    for I := 0 to FTestIndices.Count - 1 do
      with NewList.Add do
      begin
        InputFile := CorrectFileNameCase(Format(FInFormat, [FTestIndices[I]]));
        InputFile := CreateRelativePath(InputFile, WorkingDir);
        OutputFile := CorrectFileNameCase(Format(FOutFormat, [FTestIndices[I]]));
        OutputFile := CreateRelativePath(OutputFile, WorkingDir);
        Cost := 1;
        WriteLog('WorkDir = ' + WorkingDir);
        WriteLog('Test! ' + CreateRelativePath(InputFile, WorkingDir));
        WriteLog('Add files: ' + InputFile + ' ' + OutputFile);
        if (InputFile = '') or (OutputFile = '') then
        begin
          Result := False;
          Free;
        end;
      end;
    with TProblemPropsCollector, Self.Properties do
      MergeTests(TestList, NewList, Result);
  finally
    FreeAndNil(NewList);
  end;
end;

function TAllTestPropertiesParser.DoParse: boolean;
var
  FileList, BatFile: TStringList;
  I: integer;
begin
  Result := True;
  // we search for bat files (like All_Test.bat or Test.bat)
  FileList := FindAllFiles(WorkingDir, '*.bat;*.BAT', True);
  try
    // cleanup
    with TProblemPropsCollector do
    begin
      FInFormat := UnknownStr;
      FOutFormat := UnknownStr;
    end;
    FTestIndices.Clear;
    // iterate through bat files
    for I := 0 to FileList.Count - 1 do
    begin
      if IsTerminated then
        Break;
      if Pos('test', LowerCase(ExtractFileName(FileList[I]))) <> 0 then
      begin
        // parse this bat file
        BatFile := TStringList.Create;
        try
          BatFile.LoadFromFile(FileList[I]);
          if not ParseStringList(ExtractFilePath(FileList[I]), BatFile) then
            Result := False;
        finally
          FreeAndNil(BatFile);
        end;
      end;
    end;
    // add tests from format and numbers
    if not IsTerminated then
      if not AddTests then
        Result := False;
  finally
    FreeAndNil(FileList);
  end;
end;

constructor TAllTestPropertiesParser.Create;
begin
  inherited Create;
  FTestIndices := TStringList.Create;
end;

destructor TAllTestPropertiesParser.Destroy;
begin
  FreeAndNil(FTestIndices);
  inherited Destroy;
end;

end.
