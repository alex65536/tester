{
  This file is part of Tester

  Copyright (C) 2017-2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
unit checkercompile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, compilers, FileUtil, LazFileUtils, logfile, testerprimitives;

function CompileChecker(const AFileName: string): string;

implementation

function DoCompileChecker(const AFileName: string; LibPaths: TStringList;
  CompilerClass: TCompilerClass): string;
var
  ShortFileName, CheckerExe, CompilerOutput: string;
  CompilerVerdict: TCompilerVerdict;
begin
  ShortFileName := ExtractFileNameWithoutExt(ExtractFileName(AFileName));
  CheckerExe := AppendPathDelim(ExtractFileDir(AFileName)) + ShortFileName;
  {$IfDef Windows}
  CheckerExe := CheckerExe + '.exe';
  {$EndIf}
  if FileExistsUTF8(CheckerExe) then
    // already compiled
    CompilerVerdict := cvSuccess
  else
    // compile it
    CompilerVerdict := CompileFile(AFileName, CheckerExe, CompilerOutput,
      DefaultStackSize, '', LibPaths, LibPaths, CompilerClass);
  if CompilerVerdict = cvSuccess then
    Result := CheckerExe
  else
    Result := '';
end;

function CompileChecker(const AFileName: string): string;
var
  LibPaths: TStringList;
  CheckPath: string;

  procedure AddLibPath(const ALibPath: string);
  begin
    if DirectoryExistsUTF8(ALibPath) then
    begin
      WriteLog('Added lib path ' + ALibPath);
      LibPaths.Add(ALibPath);
    end;
  end;

begin
  LibPaths := TStringList.Create;
  try
    CheckPath := AppendPathDelim(ExtractFilePath(ExpandFileNameUTF8(AFileName)));
    AddLibPath(CheckPath + 'files');
    Result := DoCompileChecker(AFileName, LibPaths, nil);
    if ExtractFileExt(AFileName) = '.cpp' then
    begin
      if Result = '' then
      begin
        WriteLog('Simple c++11 didn''t work, compiling ' + AFileName + ' with gnu++11');
        Result := DoCompileChecker(AFileName, LibPaths, TGnuCpp11GnuExtCompiler);
      end;
      if Result = '' then
      begin
        WriteLog('Gnu++11 didn''t work, compiling ' + AFileName + ' with default settings');
        Result := DoCompileChecker(AFileName, LibPaths, TGnuCppCompiler);
      end;
    end;
  finally
    FreeAndNil(LibPaths);
  end;
end;

end.

