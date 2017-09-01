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
unit testerfileutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils;

procedure BeginCache;
procedure EndCache;

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

  { TSearchResultsCache }

  TSearchResultsCache = class
  private
    FCacheLock: integer;
    FCachedState: TStringList;
    FSearchMask: string;
    FSearchPath: string;
    FSearchSubDirs: boolean;
    procedure SetSearchMask(AValue: string);
    procedure SetSearchPath(AValue: string);
    procedure SetSearchSubDirs(AValue: boolean);
  public
    property SearchPath: string read FSearchPath write SetSearchPath;
    property SearchMask: string read FSearchMask write SetSearchMask;
    property SearchSubDirs: boolean read FSearchSubDirs write SetSearchSubDirs;
    procedure BeginCache;
    procedure EndCache;
    procedure ClearCache;
    function FindAllFiles(OnFileFound: TFileFoundEvent; const ASearchPath: string;
      ASearchMask: string = ''; ASearchSubDirs: boolean = True): TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

var
  ACache: TSearchResultsCache;

procedure BeginCache;
begin
  ACache.BeginCache;
end;

procedure EndCache;
begin
  ACache.EndCache;
end;

function CorrectFileName(FileName: string): string;
var
  FileDir, FileNameOnly: string;
begin
  // TODO: Correct file name in all path, not only in the last file!!!
  FileName := CorrectSeparators(FileName);
  FileDir := ExtractFilePath(FileName);
  FileNameOnly := ExtractFileName(FileName);
  Result := AppendPathDelim(FileDir) + CorrectFileNameCase(FileDir, FileNameOnly);
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
begin
  Result := ACache.FindAllFiles(OnFileFound, SearchPath, SearchMask, SearchSubDirs);
end;

{ TSearchResultsCache }

procedure TSearchResultsCache.SetSearchMask(AValue: string);
begin
  if FSearchMask = AValue then
    Exit;
  FSearchMask := AValue;
end;

procedure TSearchResultsCache.SetSearchPath(AValue: string);
begin
  if FSearchPath = AValue then
    Exit;
  FSearchPath := AValue;
end;

procedure TSearchResultsCache.SetSearchSubDirs(AValue: boolean);
begin
  if FSearchSubDirs = AValue then
    Exit;
  FSearchSubDirs := AValue;
end;

procedure TSearchResultsCache.BeginCache;
begin
  Inc(FCacheLock);
end;

procedure TSearchResultsCache.EndCache;
begin
  Dec(FCacheLock);
  if FCacheLock < 0 then
    FCacheLock := 0;
  if FCacheLock = 0 then
    ClearCache;
end;

procedure TSearchResultsCache.ClearCache;
begin
  SearchPath := '*%unknown%*';
  SearchMask := '*%unknown%*';
  SearchSubDirs := False;
  FCachedState.Clear;
end;

function TSearchResultsCache.FindAllFiles(OnFileFound: TFileFoundEvent;
  const ASearchPath: string; ASearchMask: string;
  ASearchSubDirs: boolean): TStringList;
var
  ASearcher: TListFileSearcher;
begin
  // check the cached state
  if (FCacheLock > 0) and (ASearchPath = SearchPath) and
    (ASearchMask = SearchMask) and (ASearchSubDirs = SearchSubDirs) then
  begin
    Result := TStringList.Create;
    Result.Assign(FCachedState);
    Exit;
  end;
  // find all files
  Result := TStringList.Create;
  ASearcher := TListFileSearcher.Create(Result);
  try
    ASearcher.OnFileFound := OnFileFound;
    ASearcher.Search(ASearchPath, ASearchMask, ASearchSubDirs);
  finally
    FreeAndNil(ASearcher);
  end;
  // cache current state
  if FCacheLock > 0 then
  begin
    SearchPath := ASearchPath;
    SearchMask := ASearchMask;
    SearchSubDirs := ASearchSubDirs;
    FCachedState.Assign(Result);
  end;
end;

constructor TSearchResultsCache.Create;
begin
  FCachedState := TStringList.Create;
  FCacheLock := 0;
  ClearCache;
end;

destructor TSearchResultsCache.Destroy;
begin
  FreeAndNil(FCachedState);
  inherited Destroy;
end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  if Assigned(OnFileFound) then
    OnFileFound(Self);
  inherited DoFileFound;
end;

initialization
  ACache := TSearchResultsCache.Create;

finalization
  FreeAndNil(ACache);

end.
