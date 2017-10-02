{
  This file is part of TsRun

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
program tsrun;

{$mode objfpc}{$H+}

{$R *.res}

uses
  {$IfDef Unix}
  cwstring,
  {$EndIf}
  {$IfDef Windows}
  Windows,
  {$EndIf}
  ts_testerutil, ts_testerbase, SysUtils, Classes, versioninfo, tsrunstrconsts,
  LazUTF8, jsonsaver, LazFileUtils, problemtesting, problemprops, strverdicts;

type
  ETsRun = class(Exception);

  { TTesterWatcher }

  TTesterWatcher = class
  public
    procedure Start(Sender: TObject);
    procedure Compile(Sender: TObject);
    procedure Test(Sender: TObject; TestIndex: integer);
    procedure TestSkip(Sender: TObject; TestIndex: integer);
    procedure Finish(Sender: TObject);
  end;

var
  ProblemWorkDir: string;
  ProblemPropsFile: string;
  TestSrc: string;
  ResFile: string;
  TestDirName: string;
  Timeout: integer;

  Properties: TProblemProperties;
  Tester: TProblemTester;

function WaitForFileOpen(const FileName: string): TFileStream;
const
  TriesCount = 300;
  TriesTimeout = 15;
var
  I: integer;
begin
  I := 0;
  while True do
    try
      Result := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite, fmShareExclusive);
      Exit;
    except
      if I < TriesCount then
      begin
        Inc(I);
        Sleep(TriesTimeout);
      end
      else
        raise ETsRun.CreateFmt(SFileOpenTimeout, [FileName]);
    end;
end;

procedure WriteResultsToFile;
var
  FileStream: TFileStream;
  S: string;
begin
  FileStream := WaitForFileOpen(ResFile);
  try
    S := SaveTestedProblemToJSONStr(Tester.Results);
    FileStream.Write(S[1], Length(S));
  finally
    FreeAndNil(FileStream);
  end;
  if Timeout <> 0 then
    Sleep(Timeout);
end;

procedure ParseParameters;
begin
  // check params count
  if ParamCount < 4 then
    raise ETsRun.CreateFmt(STooFewParams, [SUsage]);
  if ParamCount > 6 then
    raise ETsRun.CreateFmt(STooManyParams, [SUsage]);
  // parse params
  ProblemWorkDir := ExpandFileNameUTF8(ParamStrUTF8(1));
  ProblemPropsFile := ParamStrUTF8(2);
  TestSrc := ExpandFileNameUTF8(ParamStrUTF8(3));
  ResFile := ExpandFileNameUTF8(ParamStrUTF8(4));
  TestDirName := '';
  Timeout := 0;
  if ParamCount >= 5 then
    TestDirName := ParamStrUTF8(5);
  if ParamCount >= 6 then
    Timeout := StrToInt(ParamStrUTF8(6));
end;

function LoadPropertiesFromFile(const FileName: string): TProblemProperties;
var
  StrList: TStringList;
  Version: TFileVersion;
begin
  Result := TProblemProperties.Create;
  try
    StrList := TStringList.Create;
    try
      StrList.LoadFromFile(FileName);
      Version := TFileVersion.Create;
      try
        LoadFromJSONStr(StrList.Text, Result, Version);
      finally
        FreeAndNil(Version);
      end;
    finally
      FreeAndNil(StrList);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure Process;
var
  Watcher: TTesterWatcher;
begin
  // parse parameters
  ParseParameters;
  // go to the problem directory
  SetCurrentDirUTF8(ProblemWorkDir);
  ProblemPropsFile := ExpandFileNameUTF8(ProblemPropsFile);
  // load properties from file
  Properties := LoadPropertiesFromFile(ProblemPropsFile);
  try
    // create problem tester
    Tester := TProblemTester.Create;
    try
      Watcher := TTesterWatcher.Create;
      try
        // fill the parameters & launch
        Tester.Properties := Properties;
        Tester.TestDirName := TestDirName;
        with Tester do
        begin
          SourceFile := TestSrc;
          OnStart := @Watcher.Start;
          OnCompile := @Watcher.Compile;
          OnTest := @Watcher.Test;
          OnTestSkip := @Watcher.TestSkip;
          OnFinish := @Watcher.Finish;
          Prepare;
          Launch;
        end;
      finally
        FreeAndNil(Watcher);
      end;
    finally
      FreeAndNil(Tester);
    end;
  finally
    FreeAndNil(Properties);
  end;
end;

{ TTesterWatcher }

procedure TTesterWatcher.Start(Sender: TObject);
begin
  WriteLn(Format(SStartTesting, [TestSrc]));
  WriteResultsToFile;
end;

procedure TTesterWatcher.Compile(Sender: TObject);
begin
  WriteLn(Format(SCompiled, [SCompilerVerdicts[Tester.Results.CompileVerdict]]));
  WriteResultsToFile;
end;

procedure TTesterWatcher.Test(Sender: TObject; TestIndex: integer);
begin
  with Tester.Results[TestIndex] do
    WriteLn(Format(STestPassed, [TestIndex + 1, STestVerdicts[Verdict], Score,
      Properties.Tests[TestIndex].Cost]));
  WriteResultsToFile;
end;

procedure TTesterWatcher.TestSkip(Sender: TObject; TestIndex: integer);
begin
  WriteLn(Format(STestSkipped, [TestIndex + 1]));
end;

procedure TTesterWatcher.Finish(Sender: TObject);
begin
  WriteLn(Format(SFinished, [Tester.Results.TotalScore, Properties.MaxScore]));
  WriteResultsToFile;
end;

begin
  {$IfDef Windows}
  SetConsoleOutputCP(CP_UTF8);
  {$EndIf}
  InitVersionInfo;
  WriteLn(Format(SThisIsTsRun, [GetAppVersion]));
  WriteLn(SLegalCopyright);
  WriteLn('---------------------------------------------------------');
  try
    Process;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, Format(SErrorFmt, [E.ClassName, E.Message]));
      Halt(1);
    end;
  end;
end.

