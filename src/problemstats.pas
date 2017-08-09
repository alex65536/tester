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
unit problemstats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemprops, testresults, testerprimitives, typinfo,
  strconsts, Math;

type
  EProblemStats = class(Exception);

  { TProblemStats }

  TProblemStats = class
  private
    FTestedProblem: TTestedProblem;
    FTestsCounted: boolean;
    FTimeMemoryCounted: boolean;

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
  if FTestsCounted then
    Exit;
  if FTestedProblem.TestResultsCount = 0 then
    FCanCountTests := False
  else
  begin
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
  FTestsCounted := True;
end;

procedure TProblemStats.CountTimeMemory;
var
  I: integer;
  Start: integer;
  Time: TProblemTime;
  Memory: TProblemMemory;
  TotMemory: double; // double is used to avoid overflow
begin
  if FTimeMemoryCounted then
    Exit;
  CountTests;
  if FTestedTests = 0 then
  begin
    FCanCountTime := False;
    FCanCountMemory := False;
  end
  else
  begin
    FCanCountTime := True;
    FCanCountMemory := True;
    // find first tested element
    Start := 0;
    while not (FTestedProblem[Start].Verdict in TestedSet) do
      Inc(Start);
    // add it
    Time := FTestedProblem[Start].Time;
    FMaxTime := Time;
    FMinTime := Time;
    FTotalTime := Time;
    Memory := FTestedProblem[Start].Memory;
    FMaxMemory := Memory;
    FMinMemory := Memory;
    TotMemory := Memory;
    // iterate over others
    for I := Start + 1 to FTotalTests - 1 do
    begin
      if not (FTestedProblem[I].Verdict in TestedSet) then
        Continue;
      Time := FTestedProblem[I].Time;
      FMaxTime := Max(FMaxTime, Time);
      FMinTime := Min(FMinTime, Time);
      FTotalTime := FTotalTime + Time;
      Memory := FTestedProblem[I].Memory;
      FMaxMemory := Max(FMaxMemory, Memory);
      FMinMemory := Min(FMinMemory, Memory);
      TotMemory := TotMemory + Memory;
    end;
    // assign averages
    FAverageTime := Round(FTotalTime / FTestedTests);
    FAverageMemory := Round(TotMemory / FTestedTests);
  end;
  FTimeMemoryCounted := True;
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
  FTestsCounted := False;
  FTimeMemoryCounted := False;
  FTestedProblem := ATestedProblem;
  CountTests;
  CountTimeMemory;
end;

end.
