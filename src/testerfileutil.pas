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
unit testerfileutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, logfile;

function CorrectFileName(FileName: string): string;
function CorrectSeparators(const FileName: string): string;
function CorrectFileNameCase(const FileDir, FileName: string): string;

function FindAllFiles(OnFileFound: TFileFoundEvent; const SearchPath: string;
  SearchMask: string = ''; SearchSubDirs: boolean = True): TStringList;

implementation

type

  { TListFileSearcher }

  TListFileSearcher = class(FileUtil.TListFileSearcher)
  protected
    procedure DoFileFound; override;
  end;

function CorrectFileName(FileName: string): string;
var
  FileDir, FileNameOnly: string;
begin
  // TODO: Correct file name in all path, not only in the last file!!!
  WriteLog('CorrectFileName = ' + FileName);
  FileName := CorrectSeparators(FileName);
  FileDir := ExtractFilePath(FileName);
  FileNameOnly := ExtractFileName(FileName);
  Result := AppendPathDelim(FileDir) + CorrectFileNameCase(FileDir, FileNameOnly);
  WriteLog('Result = ' + Result);
end;

function CorrectSeparators(const FileName: string): string;
var
  I: integer;
begin
  Result := FileName;
  for I := 1 to Length(Result) do
    if Result[I] in AllowDirectorySeparators then
      Result[I] := DirectorySeparator;
end;

function CorrectFileNameCase(const FileDir, FileName: string): string;
  // Nessesary for GNU/Linux version. Sometimes tests may be in different cases
  // (as 4.in, but 5.IN). So we need case-insensivity.
var
  AList: TStringList;
  I: integer;
  CurFileName: string;
begin
  Result := FileName;
  AList := FindAllFiles(nil, FileDir, '*', False);
  try
    for I := 0 to AList.Count - 1 do
    begin
      CurFileName := ExtractFileName(AList[I]);
      if LowerCase(FileName) = LowerCase(CurFileName) then
      begin
        Result := CurFileName;
        Break;
      end;
    end;
  finally
    FreeAndNil(AList);
  end;
end;

function FindAllFiles(OnFileFound: TFileFoundEvent; const SearchPath: string;
  SearchMask: string; SearchSubDirs: boolean): TStringList;
var
  ASearcher: TListFileSearcher;
begin
  Result := TStringList.Create;
  ASearcher := TListFileSearcher.Create(Result);
  try
    ASearcher.OnFileFound := OnFileFound;
    ASearcher.Search(SearchPath, SearchMask, SearchSubDirs);
  finally
    FreeAndNil(ASearcher);
  end;
end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  if Assigned(OnFileFound) then
    OnFileFound(Self);
  inherited DoFileFound;
end;

end.

