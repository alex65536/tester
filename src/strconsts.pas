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
unit strconsts;

{$mode objfpc}{$H+}

interface

uses
  testerprimitives;

// Common messages
resourcestring
  SUnknownChecker = 'Unknown checker "%s"';
  SCheckerNotFound = 'Checker "%s" not found';
  SCheckerNotRegistered = '%s is not registered';
  SCheckerRunError = 'Checker ran unsuccessfully';
  SFilesEqual = 'Files are equal :)';
  SFilesNotEqual = 'Files are not equal :(';
  SFileNotFound = 'File "%s" not found :(';
  SCheckerRunFail = 'Could not run checker "%s"';
  SCheckerExitCode = 'Checker exitcode: %d';
  SCheckerEditorNotFound = 'Checker editor not found';
  SCompilerError = 'Could not run compiler "%s"';
  SCompilerExitCode = 'Compiler exitcode: %d';
  SCompilerNotRegistered = 'No compiler registered for extension %s';
  SSourceNotSupported = 'Extension %s not supported';
  SDirCreateError = 'Cannot create dir "%s"';
  SDirRemoveError = 'Cannot remove dir "%s"';
  SFileDeleteError = 'Cannot delete file "%s"';
  SFileCopyError = 'Cannot copy file "%s" to "%s"';
  SExitWithExitcode = 'Program exited with exitcode = %d';
  SSourceName = 'Source name';
  SCompileStatus = 'Compile';
  STestIndex = 'Test %d';
  STotalScore = 'Score';
  STimeConsumed = '%.2f s';
  STimeConsumedEx = '%.3f seconds';
  SMemConsumed = '%.2f MB';
  SMemConsumedEx = '%.0f KBytes';
  SScoreDivide = '%.2f/%.2f';
  SAlreadyTesting = 'Testing is already started';
  SNoTestsAdded = 'No tests were added';
  SAddTest = 'Add test';
  SInsertTest = 'Insert test';
  SEditTest = 'Edit test';
  SMultiAddTest = 'Add tests from template';
  SThreadAlreadyRunning = 'ParserForm is already running a thread';
  SCheckRecommendation = 'Please check and edit (if necessary) the problem data manually';
  SParserFail = 'One or more parsers failed';
  SMergeConflict = 'Two or more parsers produced the conflicting output';
  SNotFullInfo = 'Parsing didn''t deduce all the problem info';
  SParserWarningFmt = '%s. %s.';
  SVersionFmt = 'version %s';
  SFullAppNameFmt = '%s v.%s';
  SBuildDateFmt = 'Build date: %s';
  SDefaultVersion = '<unknown>';
  SDateViewFmt = 'dd.mm.yyyy';

// Compilation verdicts
resourcestring
  SCompileSuccess = 'Compile successful';
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

implementation

end.
