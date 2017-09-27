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
unit strconsts;

{$mode objfpc}{$H+}

interface

// Common messages
resourcestring
  SUnknownChecker = 'Unknown checker "%s"';
  SCheckerNotFound = 'Checker "%s" not found';
  SCheckerNotRegistered = '%s is not registered';
  SCheckerRunError = 'Checker ran unsuccessfully';
  SFilesEqual = 'Files are equal! :)';
  SFilesNotEqualFmt = 'Error at line %d, column %d : %s.';
  SFilesCharsDiffer = 'expected "%s", found "%s"';
  SFilesLineTooShort = 'line too short';
  SFilesLineTooLong = 'extra data in line';
  SFilesFileTooShort = 'unexpected end of file';
  SFilesFileTooLong = 'extra data in file';
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
  SScoreFmt = '%.2f';
  SScoreDivide = '%.2f/%.2f';
  SAlreadyTesting = 'Testing is already started';
  SNoTestsAdded = 'No tests were added!';
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
  SFullAppNameFmt = '%s v%s';
  SBuildDateFmt = 'Build date: %s';
  SDefaultVersion = '<unknown>';
  SDateViewFmt = 'dd.mm.yyyy';
  SHTMLComment = 'This HTML file was exported by Tester v%s.';
  SHTMLTitle = 'Testing results';
  SSavePropsError = 'Unable to save properties file "%s"';
  STestFilesDontExist = 'Input or output file doesn''t exist. Do you really want to add this test?';
  SNotFoundWarning = 'Some files listed in the problem data were not found';
  SNotFoundFilesListBegin = 'The files are:';
  SNotFoundFileFormat = #9'%s';
  SNotFoundWarningFmt = '%s. %s.';
  SCoundNotConvert = 'Could not convert %s to %s';
  STooNewVersion = 'The file "%s" has a newer version (%s) that is supported (%s). Continue loading this file? If you continue, some problem data added in new versions will be lost.';
  SUnableToLoad = 'Unable to load file "%s"';
  SUnableToSave = 'Unable to save file "%s"';
  SUnableToLoadVersionInfo = 'Unable to load version info';
  SStatsUnableVerdict = 'Unable to process verdict "%s"';
  SStatsCannotTestsInfo = 'Cannot get test info';
  SStatsCannotTimeInfo = 'Cannot get time info';
  SStatsCannotMemoryInfo = 'Cannot get memory info';
  SNoAnswer = 'N/A';
  SCompilerFullVersion = '%s (%s %s)';
  SCouldNotCompilerVersion = 'Could not retrieve compiler version!';

implementation

end.
