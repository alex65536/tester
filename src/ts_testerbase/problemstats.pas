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
unit problemstats;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, testresults, testerprimitives, typinfo, strconsts, Math;

type
  EProblemStats = class(Exception);

  { TProblemStats }

  TProblemStats = class
  private
    FTestedProblem: TTestedProblem;

    // tests
    FCanCountTests: boolean;
    FTotalTests: integer;
    FTestedTests: integer;
    FPassedTests: integer;
    FFailedTests: integer;
    FSkippedTests: integer;
    FWaitingTests: integer;

    // time
    FCanCountTime: boolean;
    FMinTime: TProblemTime;
    FMaxTime: TProblemTime;
    FAverageTime: TProblemTime;
    FTotalTime: TProblemTime;

    // memory
    FCanCountMemory: boolean;
    FMinMemory: TProblemMemory;
    FMaxMemory: TProblemMemory;
    FAverageMemory: TProblemMemory;

    procedure CountTests;
    procedure CountTimeMemory;
    procedure RequiresTests;
    procedure RequiresTime;
    procedure RequiresMemory;
  public
    property TestedProblem: TTestedProblem read FTestedProblem;

    // Tests-related stuff
    function CanCountTests: boolean;
    function TestsPassed: integer;
    function TestsFailed: integer;
    function TestsSkipped: integer;
    function TestsWaiting: integer;
    function TestsTotal: integer;

    // Time-related stuff
    function CanCountTime: boolean;
    function MinTime: TProblemTime;
    function MaxTime: TProblemTime;
    function AverageTime: TProblemTime;
    function TotalTime: TProblemTime;

    // Memory-related stuff
    function CanCountMemory: boolean;
    function MinMemory: TProblemMemory;
    function MaxMemory: TProblemMemory;
    function AverageMemory: TProblemMemory;

    constructor Create(ATestedProblem: TTestedProblem);
  end;

implementation

const
  PassedSet = [veAccepted];
  FailedSet = [veWrongAnswer, vePresentationError, veCheckError,
    veRuntimeError, veTimeLimit, veIdlenessLimit, veMemoryLimit, veRunFail];
  SkippedSet = [veSkipped];
  WaitingSet = [veWaiting];
  TestedSet = PassedSet + FailedSet;

{ TProblemStats }

procedure TProblemStats.CountTests;
var
  I: integer;
  V: TTestVerdict;
begin
  if FTestedProblem.TestResultsCount = 0 then
  begin
    FCanCountTests := False;
    Exit;
  end;
  FCanCountTests := True;
  // assign values
  FTotalTests := FTestedProblem.TestResultsCount;
  FPassedTests := 0;
  FFailedTests := 0;
  FSkippedTests := 0;
  FWaitingTests := 0;
  // iterate over tests
  for I := 0 to FTotalTests - 1 do
  begin
    V := FTestedProblem[I].Verdict;
    if V in PassedSet then
      Inc(FPassedTests)
    else if V in FailedSet then
      Inc(FFailedTests)
    else if V in SkippedSet then
      Inc(FSkippedTests)
    else if V in WaitingSet then
      Inc(FWaitingTests)
    else
      raise EProblemStats.CreateFmt(SStatsUnableVerdict,
        [GetEnumName(TypeInfo(TTestVerdict), Ord(V))]);
  end;
  FTestedTests := FPassedTests + FFailedTests;
end;

procedure TProblemStats.CountTimeMemory;
var
  I: integer;
  Size: integer;
  Times: array of TProblemTime;
  Memories: array of TProblemMemory;

  function Sum(A: array of integer): integer;
  var
    I: integer;
  begin
    Result := 0;
    for I := Low(A) to High(A) do
      Result := Result + A[I];
  end;

  function Average(A: array of integer): double;
  var
    I: integer;
  begin
    Result := 0.0;
    for I := Low(A) to High(A) do
      Result := Result + A[I];
    Result := Result / Length(A);
  end;

begin
  if FTestedTests = 0 then
  begin
    FCanCountTime := False;
    FCanCountMemory := False;
    Exit;
  end;
  FCanCountTime := True;
  FCanCountMemory := True;
  SetLength(Times, FTestedTests);
  SetLength(Memories, FTestedTests);
  Size := 0;
  // add elements to Times & Memories
  for I := 0 to FTotalTests - 1 do
    with FTestedProblem[I] do
    begin
      if Verdict in TestedSet then
      begin
        Times[Size] := Time;
        Memories[Size] := Memory;
        Inc(Size);
      end;
    end;
  // assign time values
  FMinTime := MinValue(Times);
  FMaxTime := MaxValue(Times);
  FTotalTime := Sum(Times);
  FAverageTime := Round(Average(Times));
  // assign memory values
  FMinMemory := MinValue(Memories);
  FMaxMemory := MaxValue(Memories);
  FAverageMemory := Round(Average(Memories));
end;

procedure TProblemStats.RequiresTests;
begin
  if not CanCountTests then
    raise EProblemStats.Create(SStatsCannotTestsInfo);
end;

procedure TProblemStats.RequiresTime;
begin
  if not FCanCountTime then
    raise EProblemStats.Create(SStatsCannotTimeInfo);
end;

procedure TProblemStats.RequiresMemory;
begin
  if not CanCountMemory then
    raise EProblemStats.Create(SStatsCannotMemoryInfo);
end;

function TProblemStats.CanCountTests: boolean;
begin
  Result := FCanCountTests;
end;

function TProblemStats.TestsPassed: integer;
begin
  RequiresTests;
  Result := FPassedTests;
end;

function TProblemStats.TestsFailed: integer;
begin
  RequiresTests;
  Result := FFailedTests;
end;

function TProblemStats.TestsSkipped: integer;
begin
  RequiresTests;
  Result := FSkippedTests;
end;

function TProblemStats.TestsWaiting: integer;
begin
  RequiresTests;
  Result := FWaitingTests;
end;

function TProblemStats.TestsTotal: integer;
begin
  RequiresTests;
  Result := FTotalTests;
end;

function TProblemStats.CanCountTime: boolean;
begin
  Result := FCanCountTime;
end;

function TProblemStats.MinTime: TProblemTime;
begin
  RequiresTime;
  Result := FMinTime;
end;

function TProblemStats.MaxTime: TProblemTime;
begin
  RequiresTime;
  Result := FMaxTime;
end;

function TProblemStats.AverageTime: TProblemTime;
begin
  RequiresTime;
  Result := FAverageTime;
end;

function TProblemStats.TotalTime: TProblemTime;
begin
  RequiresTime;
  Result := FTotalTime;
end;

function TProblemStats.CanCountMemory: boolean;
begin
  Result := FCanCountMemory;
end;

function TProblemStats.MinMemory: TProblemMemory;
begin
  RequiresMemory;
  Result := FMinMemory;
end;

function TProblemStats.MaxMemory: TProblemMemory;
begin
  RequiresMemory;
  Result := FMaxMemory;
end;

function TProblemStats.AverageMemory: TProblemMemory;
begin
  RequiresMemory;
  Result := FAverageMemory;
end;

constructor TProblemStats.Create(ATestedProblem: TTestedProblem);
begin
  FTestedProblem := ATestedProblem;
  CountTests;
  CountTimeMemory;
end;

end.
