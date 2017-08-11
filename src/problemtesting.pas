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
unit problemtesting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemprops, compilers, runtimers, testresults,
  FileUtil, LazFileUtils, testerprimitives, randomname, strconsts, logfile;

type
  EProblemTester = class(Exception);

  TTestEvent = procedure(Sender: TObject; TestIndex: integer) of object;

  { TProblemTester }

  TProblemTester = class(TPersistent)
  private
    FIsTerminated: boolean;
    FOnCompile: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnTest: TTestEvent;
    FOnTestSkip: TTestEvent;
    FProperties: TProblemProperties;
    FResults: TTestedProblem;
    FSourceFile: string;
    FTag: PtrInt;
    procedure SetOnCompile(AValue: TNotifyEvent);
    procedure SetOnFinish(AValue: TNotifyEvent);
    procedure SetOnStart(AValue: TNotifyEvent);
    procedure SetOnTest(AValue: TTestEvent);
    procedure SetOnTestSkip(AValue: TTestEvent);
    procedure SetProperties(AValue: TProblemProperties);
    procedure SetSourceFile(AValue: string);
    procedure SetTag(AValue: PtrInt);
  protected
    procedure DoStart; virtual;
    procedure DoCompile; virtual;
    procedure DoTest(Index: integer); virtual;
    procedure DoTestSkip(Index: integer); virtual;
    procedure DoFinish; virtual;
  public
    property IsTerminated: boolean read FIsTerminated;
    property Properties: TProblemProperties read FProperties write SetProperties;
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;
    property OnCompile: TNotifyEvent read FOnCompile write SetOnCompile;
    property OnTest: TTestEvent read FOnTest write SetOnTest;
    property OnTestSkip: TTestEvent read FOnTestSkip write SetOnTestSkip;
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property Tag: PtrInt read FTag write SetTag;
    procedure Prepare;
    procedure Launch;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
  published
    property SourceFile: string read FSourceFile write SetSourceFile;
    property Results: TTestedProblem read FResults;
  end;

implementation

{ TProblemTester }

procedure TProblemTester.SetOnCompile(AValue: TNotifyEvent);
begin
  if FOnCompile = AValue then
    Exit;
  FOnCompile := AValue;
end;

procedure TProblemTester.SetOnFinish(AValue: TNotifyEvent);
begin
  if FOnFinish = AValue then
    Exit;
  FOnFinish := AValue;
end;

procedure TProblemTester.SetOnStart(AValue: TNotifyEvent);
begin
  if FOnStart = AValue then
    Exit;
  FOnStart := AValue;
end;

procedure TProblemTester.SetOnTest(AValue: TTestEvent);
begin
  if FOnTest = AValue then
    Exit;
  FOnTest := AValue;
end;

procedure TProblemTester.SetOnTestSkip(AValue: TTestEvent);
begin
  if FOnTestSkip = AValue then Exit;
  FOnTestSkip := AValue;
end;

procedure TProblemTester.SetProperties(AValue: TProblemProperties);
begin
  if FProperties = AValue then
    Exit;
  FProperties := AValue;
end;

procedure TProblemTester.SetSourceFile(AValue: string);
begin
  if FSourceFile = AValue then
    Exit;
  FSourceFile := AValue;
end;

procedure TProblemTester.SetTag(AValue: PtrInt);
begin
  if FTag = AValue then
    Exit;
  FTag := AValue;
end;

procedure TProblemTester.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TProblemTester.DoCompile;
begin
  if Assigned(FOnCompile) then
    FOnCompile(Self);
end;

procedure TProblemTester.DoTest(Index: integer);
begin
  if Assigned(FOnTest) then
    FOnTest(Self, Index);
end;

procedure TProblemTester.DoTestSkip(Index: integer);
begin
  if Assigned(FOnTestSkip) then
    FOnTestSkip(Self, Index);
end;

procedure TProblemTester.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TProblemTester.Prepare;
var
  I: integer;
begin
  FResults.Items.Clear;
  for I := 0 to FProperties.TestCount - 1 do
    FResults.Items.Add;
  FResults.CompileVerdict := cvWaiting;
  FResults.CompilerOutput := '';
end;

procedure TProblemTester.Launch;

  procedure InternalFileDel(const FileName: string);
  begin
    if FileExistsUTF8(FileName) then
    begin
      if not DeleteFileUTF8(FileName) then
        raise EProblemTester.CreateFmt(SFileDeleteError, [FileName]);
    end;
  end;

  procedure InternalDirMake(const DirName: string);
  begin
    if not DirectoryExistsUTF8(DirName) then
    begin
      if not CreateDirUTF8(DirName) then
        raise EProblemTester.CreateFmt(SDirCreateError, [DirName]);
    end;
  end;

  procedure InternalDirDel(const DirName: string);
  const
    MaxTries = 25;
  var
    I: integer;
  begin
    if not DirectoryExistsUTF8(DirName) then
      Exit;
    for I := 0 to MaxTries - 1 do
    begin
      WriteLogFmt('Directory deletion try %d', [I]);
      if DeleteDirectory(DirName, False) then
        Exit;
      Sleep(40);
    end;
    raise EProblemTester.CreateFmt(SDirRemoveError, [DirName]);
  end;

  procedure InternalCopyFiles(const Src, Dst: string);
  begin
    if not CopyFile(Src, Dst) then
      raise EProblemTester.CreateFmt(SFileCopyError, [Src, Dst]);
  end;

var
  WorkingDir: string;
  ExeName: string;
  InputFile: string;
  OutputFile: string;
  InputRedir: string;
  OutputRedir: string;

  procedure ChooseNames;
  begin
    // create storage for temp files
    WorkingDir := AppendPathDelim(GetTempDir) + 'tstr-' + GetRandomName(8);
    // choose names for executable, input and output files
    ExeName := AppendPathDelim(WorkingDir) + GetRandomName(8);
    {$IfDef Windows}
      ExeName := ExeName + '.exe';
    {$EndIf}
    if FProperties.InputFile = 'stdin' then
    begin
      InputFile := AppendPathDelim(WorkingDir) + GetRandomName(8) + '.txt';
      InputRedir := InputFile;
    end
    else
    begin
      InputFile := AppendPathDelim(WorkingDir) + FProperties.InputFile;
      InputRedir := '';
    end;
    if FProperties.OutputFile = 'stdout' then
    begin
      OutputFile := AppendPathDelim(WorkingDir) + GetRandomName(8) + '.txt';
      OutputRedir := OutputFile;
    end
    else
    begin
      OutputFile := AppendPathDelim(WorkingDir) + FProperties.OutputFile;
      OutputRedir := '';
    end;
  end;

var
  CompilerOutput: string;
  I: integer;
  Timer: TBaseRunTimer;
  MustSkip: boolean;
begin
  FIsTerminated := False;
  try
    // start the tester
    DoStart;
    if IsTerminated then
    begin
      DoFinish;
      Exit;
    end;
    try
      // compilation stage
      ChooseNames;
      begin
        try
          // create the working directory
          InternalDirMake(WorkingDir);
          // compile the solution
          FResults.CompileVerdict := CompileFile(FSourceFile, ExeName,
            CompilerOutput, FProperties.MemoryLimit, WorkingDir);
          FResults.CompilerOutput := CompilerOutput;
        except
          on E: EProblemTester do
          begin
            FResults.CompileVerdict := cvCompileFail;
            FResults.CompilerOutput := E.Message;
          end
          else
            raise;
        end;
        // say that we compiled the solution
        DoCompile;
      end;
      // check the compilation results
      if FResults.CompileVerdict = cvSuccess then
      // successful compilation - we can continue testing
      begin
        MustSkip := False;
        // iterate over the tests
        for I := 0 to FProperties.TestCount - 1 do
        begin
          if IsTerminated then
            Break;
          // if we must skip the rest of the tests, we skip them
          if MustSkip then
          begin
            FResults.TestResults[I].Verdict := veSkipped;
            DoTestSkip(I);
            Continue;
          end;
          // otherwise, just do testing
          try
            // delete temp files
            InternalFileDel(InputFile);
            InternalFileDel(OutputFile);
            // copy input file
            InternalCopyFiles(FProperties.Tests[I].InputFile, InputFile);
            // launch the program (with timer)
            Timer := TRunTimer.Create;
            try
              Timer.StdinRedir := InputRedir;
              Timer.StdoutRedir := OutputRedir;
              Timer.StderrRedir := '';
              Timer.WorkingDir := WorkingDir;
              Timer.ExeName := ExeName;
              Timer.Properties := FProperties;
              with FResults.TestResults[I] do
              begin
                Verdict := Timer.Run;
                Time := Timer.Time;
                Memory := Timer.Memory;
                if Verdict = veRuntimeError then
                  CheckerOutput := Format(SExitWithExitcode, [Timer.ExitCode]);
              end;
            finally
              FreeAndNil(Timer);
            end;
            // if the program ran successfully, launch the checker
            if Assigned(FProperties.Checker) and
              (FResults.TestResults[I].Verdict = veAccepted) then
            begin
              FProperties.Checker.InputFile := FProperties.Tests[I].InputFile;
              FProperties.Checker.OutputFile := OutputFile;
              FProperties.Checker.AnswerFile := FProperties.Tests[I].OutputFile;
              with FResults.TestResults[I] do
              begin
                Verdict := FProperties.Checker.Check;
                CheckerOutput := FProperties.Checker.CheckerOutput;
              end;
            end;
            // count the score for the test
            if FResults.TestResults[I].Verdict = veAccepted then
              FResults.TestResults[I].Score := FProperties.Tests[I].Cost;
            // if we must stop after first fail, we say that we skip the rest of the tests
            if FProperties.StopAfterFirstFail and
              (FResults.TestResults[I].Verdict <> veAccepted) then
              MustSkip := True;
          except
            on E: EProblemTester do
            begin
              FResults.TestResults[I].Verdict := veRunFail;
              FResults.TestResults[I].CheckerOutput := E.Message;
            end
            else
              raise;
          end;
          // that's all with this test, let's go further
          DoTest(I);
        end;
      end
      else
      // otherwise, skip all the tests
      begin
        for I := 0 to FProperties.TestCount - 1 do
        begin
          if IsTerminated then
            Break;
          FResults.TestResults[I].Verdict := veSkipped;
          DoTestSkip(I);
        end;
      end;
    finally
      // delete working dir
      try
        InternalDirDel(WorkingDir);
      except
        on E: EProblemTester do
          // mute the exception, we don't need to handle it.
          WriteLog('Dir delete exception: ' + E.Message);
        else
          raise;
      end;
    end;
  finally
    // finish him! :)
    DoFinish;
    FIsTerminated := True;
  end;
end;

procedure TProblemTester.AssignTo(Dest: TPersistent);
begin
  with Dest as TProblemTester do
  begin
    SourceFile := Self.SourceFile;
    Properties := Self.Properties;
    Results.Assign(Self.Results);
  end;
end;

constructor TProblemTester.Create;
begin
  FResults := TTestedProblem.Create;
end;

destructor TProblemTester.Destroy;
begin
  FreeAndNil(FResults);
  inherited Destroy;
end;

procedure TProblemTester.Terminate;
begin
  FIsTerminated := True;
end;

end.
