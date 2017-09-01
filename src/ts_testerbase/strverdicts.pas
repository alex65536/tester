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
unit strverdicts;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, testerprimitives;

// Messages for time/memory consumed
resourcestring
  STimeConsumed = '%.2f s';
  STimeConsumedEx = '%.3f seconds';
  SMemConsumed = '%.2f MB';
  SMemConsumedEx = '%d KBytes';
  STestsOfInfo = '%d of %d (%.2f%%)';

// Compilation verdicts
resourcestring
  SCompileSuccess = 'Compilation successful';
  SCompileSuccessS = 'OK';
  SCompileError = 'Compilation error';
  SCompileErrorS = 'CE';
  SCompileFail = 'Compilation fail';
  SCompileFailS = 'FL';
  SCompileWaiting = 'Waiting';
  SCompileWaitingS = '?';

const
  SCompilerVerdicts: array [TCompilerVerdict] of string =
    (SCompileSuccess, SCompileError, SCompileFail, SCompileWaiting);
  SCompilerVerdictsS: array [TCompilerVerdict] of string =
    (SCompileSuccessS, SCompileErrorS, SCompileFailS, SCompileWaitingS);

// Testing verdicts
resourcestring
  STestAccepted = 'Accepted';
  STestAcceptedS = 'AC';
  STestWrongAnswer = 'Wrong answer';
  STestWrongAnswerS = 'WA';
  STestPresentationError = 'Presentation error';
  STestPresentationErrorS = 'PE';
  STestCheckError = 'Checker error';
  STestCheckErrorS = 'FL';
  STestRuntimeError = 'Runtime error';
  STestRuntimeErrorS = 'RE';
  STestTimeLimit = 'Time limit exceeded';
  STestTimeLimitS = 'TL';
  STestIdlenessLimit = 'Idleness limit exceeded';
  STestIdlenessLimitS = 'IL';
  STestMemoryLimit = 'Memory limit exceeded';
  STestMemoryLimitS = 'ML';
  STestRunFail = 'Run failed';
  STestRunFailS = 'RF';
  STestSkipped = 'Skipped';
  STestSkippedS = '-';
  STestWaiting = 'Waiting';
  STestWaitingS = '?';

const
  STestVerdicts: array [TTestVerdict] of string =
    (STestAccepted, STestWrongAnswer, STestPresentationError, STestCheckError,
    STestRuntimeError, STestTimeLimit, STestIdlenessLimit, STestMemoryLimit,
    STestRunFail, STestSkipped, STestWaiting);
  STestVerdictsS: array [TTestVerdict] of string =
    (STestAcceptedS, STestWrongAnswerS, STestPresentationErrorS, STestCheckErrorS,
    STestRuntimeErrorS, STestTimeLimitS, STestIdlenessLimitS, STestMemoryLimitS,
    STestRunFailS, STestSkippedS, STestWaitingS);

function ProblemTimeToStr(ATime: TProblemTime): string;
function ProblemMemoryToStr(AMemory: TProblemMemory): string;

function ProblemTimeToStrEx(ATime: TProblemTime): string;
function ProblemMemoryToStrEx(AMemory: TProblemMemory): string;

function GetTestsOfCaption(AHave, ATotal: integer): string;

implementation

function ProblemTimeToStr(ATime: TProblemTime): string;
begin
  Result := Format(STimeConsumed, [ATime / 1000]);
end;

function ProblemMemoryToStr(AMemory: TProblemMemory): string;
begin
  Result := Format(SMemConsumed, [AMemory / 1024]);
end;

function ProblemTimeToStrEx(ATime: TProblemTime): string;
begin
  Result := Format(STimeConsumedEx, [ATime / 1000]);
end;

function ProblemMemoryToStrEx(AMemory: TProblemMemory): string;
begin
  Result := Format(SMemConsumedEx, [AMemory]);
end;

function GetTestsOfCaption(AHave, ATotal: integer): string;
var
  Percent: double;
begin
  Percent := AHave / ATotal * 100;
  Result := Format(STestsOfInfo, [AHave, ATotal, Percent]);
end;

end.

