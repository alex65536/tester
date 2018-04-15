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
unit runtimers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, problemprops, timerlib, LazUTF8, testerprimitives, math,
  LazFileUtils;

type

  { TBaseRunTimer }

  TBaseRunTimer = class
  private
    FExeName: string;
    FInputFile: string;
    FOutputFile: string;
    FProperties: TProblemProperties;
    FStderrRedir: string;
    FStdinRedir: string;
    FStdoutRedir: string;
    FWorkingDir: string;
    procedure SetExeName(AValue: string);
    procedure SetInputFile(AValue: string);
    procedure SetOutputFile(AValue: string);
    procedure SetProperties(AValue: TProblemProperties);
    procedure SetStderrRedir(AValue: string);
    procedure SetStdinRedir(AValue: string);
    procedure SetStdoutRedir(AValue: string);
    procedure SetWorkingDir(AValue: string);
  protected
    FExitCode: integer;
    FMemory: TProblemMemory;
    FTime: TProblemTime;
  public
    property StdinRedir: string read FStdinRedir write SetStdinRedir;
    property StdoutRedir: string read FStdoutRedir write SetStdoutRedir;
    property StderrRedir: string read FStderrRedir write SetStderrRedir;
    property InputFile: string read FInputFile write SetInputFile;
    property OutputFile: string read FOutputFile write SetOutputFile;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property ExeName: string read FExeName write SetExeName;
    property Properties: TProblemProperties read FProperties write SetProperties;
    property Time: TProblemTime read FTime;
    property Memory: TProblemMemory read FMemory;
    property ExitCode: integer read FExitCode;
    function Run: TTestVerdict; virtual; abstract;
    constructor Create; virtual;
  end;

  TBaseRunTimerClass = class of TBaseRunTimer;

  { TTimerlibRunTimer }

  TTimerlibRunTimer = class(TBaseRunTimer)
  public
    function Run: TTestVerdict; override;
  end;

var
  TRunTimer: TBaseRunTimerClass = TTimerlibRunTimer;

implementation

{ TTimerlibRunTimer }

function TTimerlibRunTimer.Run: TTestVerdict;
var
  Temp: integer;
  Res: TTimerResult;
  AExeName: string;

  function UTF8ToSystem(const S: string): string;
  begin
    {$IfDef Windows}
       Result := UTF8ToWinCP(s);
    {$Else}
       Result := UTF8ToSys(s);
    {$EndIf}
  end;

var
  RealTimeLimit: integer;
begin
  {$IfDef Windows}
    AExeName := ExeName;
  {$Else}
    AExeName := ExpandFileNameUTF8(ExeName);
  {$EndIf}
  RealTimeLimit := Properties.TimeLimit * 5;
  // TODO: Test new RealTimeLimit on Windows!
  Res := LaunchTimer(PChar(UTF8ToSystem(WorkingDir)), PChar(UTF8ToSystem(AExeName)),
    PChar(UTF8ToSystem(StdinRedir)), PChar(UTF8ToSystem(StdoutRedir)),
    PChar(UTF8ToSystem(StderrRedir)), PChar(UTF8ToSystem(InputFile)),
    PChar(UTF8ToSystem(OutputFile)), Properties.TimeLimit, Max(1000,
    RealTimeLimit), Properties.MemoryLimit * 2048 - 1,
    Properties.MemoryLimit * 1024, @FTime, @Temp, @FMemory, @FExitCode);
  FMemory := Round(FMemory / 1024); // we show memory in kBytes instead of bytes!
  case Res of
    trOK: Result := veAccepted;
    trTimeLimit: Result := veTimeLimit;
    trRealTimeLimit: Result := veIdlenessLimit;
    trMemoryLimit: Result := veMemoryLimit;
    trRuntimeError: Result := veRuntimeError;
    trRunFail: Result := veRunFail;
  end;
end;

{ TBaseRunTimer }

procedure TBaseRunTimer.SetProperties(AValue: TProblemProperties);
begin
  if FProperties = AValue then
    Exit;
  FProperties := AValue;
end;

procedure TBaseRunTimer.SetExeName(AValue: string);
begin
  if FExeName = AValue then
    Exit;
  FExeName := AValue;
end;

procedure TBaseRunTimer.SetInputFile(AValue: string);
begin
  if FInputFile = AValue then Exit;
  FInputFile := AValue;
end;

procedure TBaseRunTimer.SetOutputFile(AValue: string);
begin
  if FOutputFile = AValue then Exit;
  FOutputFile := AValue;
end;

procedure TBaseRunTimer.SetStderrRedir(AValue: string);
begin
  if FStderrRedir = AValue then
    Exit;
  FStderrRedir := AValue;
end;

procedure TBaseRunTimer.SetStdinRedir(AValue: string);
begin
  if FStdinRedir = AValue then
    Exit;
  FStdinRedir := AValue;
end;

procedure TBaseRunTimer.SetStdoutRedir(AValue: string);
begin
  if FStdoutRedir = AValue then
    Exit;
  FStdoutRedir := AValue;
end;

procedure TBaseRunTimer.SetWorkingDir(AValue: string);
begin
  if FWorkingDir = AValue then
    Exit;
  FWorkingDir := AValue;
end;

constructor TBaseRunTimer.Create;
begin
  StdinRedir := '';
  StdoutRedir := '';
  StderrRedir := '';
  WorkingDir := '';
  ExeName := '';
  Properties := nil;
end;

end.
