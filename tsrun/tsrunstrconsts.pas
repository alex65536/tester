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
unit tsrunstrconsts;

{$mode objfpc}{$H+}

interface

resourcestring
  SThisIsTsRun = 'This is TsRun version %s';
  SLegalCopyright = 'Copyright © 2017 Alexander Kernozhitsky';
  SUsage =
    'Usage:' + LineEnding +
    '' + LineEnding +
    'tsrun <problem-work-dir> <problem-props-file> <test-src> <res-file> [<timeout>]' + LineEnding +
    '' + LineEnding +
    '  TsRun will load a problem from <problem-props-file> with working directory' + LineEnding +
    '<problem-work-dir>. It will test source file <test-src> and write the results' + LineEnding +
    'into <res-file>.' + LineEnding +
    '  TsRun will update file each time when something is changed, so you can try to' + LineEnding +
    'read the file permanently to retrieve the updates. When the changes are made,' + LineEnding +
    'the file is locked.' + LineEnding +
    '  <timeout> indicates what time TsRun will be idle after each change, so you can' + LineEnding +
    'read the results from the file.';
  SErrorFmt = 'ERROR %s : %s';
  STooFewParams = 'Too few parameters!' + LineEnding + '%s';
  STooManyParams = 'Too many parameters!' + LineEnding + '%s';
  SStartTesting = 'Testing "%s"...';
  SCompiled = 'Compiled.';
  STestPassed = 'Test #%d.';
  STestSkipped = 'Skipped test #%d.';
  SFinished = 'Finished.';

implementation

end.

