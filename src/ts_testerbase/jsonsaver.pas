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
unit jsonsaver;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, problemprops, fpjson, fpjsonrtti, jsonparser,
  multitesters, jsonscanner, versioninfo, testresults;

function LoadChecker(Obj: TJSONData): TProblemChecker;
procedure LoadFromJSONObj(Obj: TJSONObject; Props: TProblemProperties;
  Version: TFileVersion);
procedure LoadFromJSONStr(const Str: TJSONStringType; Props: TProblemProperties;
  Version: TFileVersion);

function GetPropsFileVersion(const AFileName: string): TFileVersion;

function SaveChecker(Checker: TProblemChecker): TJSONData;
function SavePropsToJSONObj(Props: TProblemProperties): TJSONObject;
function SavePropsToJSONStr(Props: TProblemProperties): TJSONStringType;

procedure LoadTestedProblemFromJSONObj(Obj: TJSONObject;
  TestedProblem: TTestedProblem);
procedure LoadTestedProblemFromJSONStr(const Str: TJSONStringType;
  TestedProblem: TTestedProblem);
function SaveTestedProblemToJSONStr(TestedProblem: TTestedProblem): TJSONStringType;

function SaveMultiTesterToJSONStr(MultiTester: TMultiTester): TJSONStringType;

implementation

function LoadChecker(Obj: TJSONData): TProblemChecker;
var
  DeStreamer: TJSONDeStreamer;
begin
  if Obj.IsNull then
    Result := nil
  else
  begin
    with Obj as TJSONObject do
    begin
      Result := CreateChecker(Elements['Type'].AsString);
      DeStreamer := TJSONDeStreamer.Create(nil);
      try
        DeStreamer.JSONToObject(Elements['Value'] as TJSONObject, Result);
      finally
        FreeAndNil(DeStreamer);
      end;
    end;
  end;
end;

procedure LoadFromJSONObj(Obj: TJSONObject; Props: TProblemProperties;
  Version: TFileVersion);
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    // load properties
    if Props <> nil then
    begin
      DeStreamer.JSONToObject(Obj, Props);
      Props.Checker := LoadChecker(Obj['Checker']);
    end;
    // load version info
    if Version <> nil then
    begin
      try
        DeStreamer.JSONToObject(Obj['Version'] as TJSONObject, Version);
      except
        Version.FillZero;
        // mute the exception
      end;
    end;
  finally
    FreeAndNil(DeStreamer);
  end;
end;

procedure LoadFromJSONStr(const Str: TJSONStringType;
  Props: TProblemProperties; Version: TFileVersion);
var
  Parser: TJSONParser;
  Obj: TJSONData;
begin
  Parser := TJSONParser.Create(Str, DefaultOptions);
  try
    Obj := Parser.Parse;
    try
      LoadFromJSONObj(Obj as TJSONObject, Props, Version);
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

function GetPropsFileVersion(const AFileName: string): TFileVersion;
var
  MemStream: TMemoryStream;
  S: string;
begin
  Result := TFileVersion.Create;
  try
    MemStream := TMemoryStream.Create;
    try
      MemStream.LoadFromFile(AFileName);
      SetLength(S, MemStream.Size);
      MemStream.Read(S[1], Length(S));
      LoadFromJSONStr(S, nil, Result);
    finally
      FreeAndNil(MemStream);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function SaveChecker(Checker: TProblemChecker): TJSONData;
var
  Streamer: TJSONStreamer;
begin
  if Checker = nil then
    Result := TJSONNull.Create
  else
  begin
    Result := TJSONObject.Create;
    with Result as TJSONObject do
    begin
      Elements['Type'] := TJSONString.Create(Checker.ClassName);
      Streamer := TJSONStreamer.Create(nil);
      try
        Elements['Value'] := Streamer.ObjectToJSON(Checker);
      finally
        FreeAndNil(Streamer);
      end;
    end;
  end;
end;

function SavePropsToJSONObj(Props: TProblemProperties): TJSONObject;
var
  Streamer: TJSONStreamer;
  CurVersion: TFileVersion;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSON(Props);
    Result['Checker'] := SaveChecker(Props.Checker);
    // write version info
    CurVersion := TFileVersion.Current;
    try
      Result['Version'] := Streamer.ObjectToJSON(CurVersion);
    finally
      FreeAndNil(CurVersion);
    end;
  finally
    FreeAndNil(Streamer);
  end;
end;

function SavePropsToJSONStr(Props: TProblemProperties): TJSONStringType;
var
  Obj: TJSONObject;
begin
  Obj := SavePropsToJSONObj(Props);
  try
    Result := Obj.AsJSON;
  finally
    FreeAndNil(Obj);
  end;
end;

procedure LoadTestedProblemFromJSONObj(Obj: TJSONObject;
  TestedProblem: TTestedProblem);
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(Obj, TestedProblem);
  finally
    FreeAndNil(DeStreamer);
  end;
end;

procedure LoadTestedProblemFromJSONStr(const Str: TJSONStringType;
  TestedProblem: TTestedProblem);
var
  Parser: TJSONParser;
  Obj: TJSONData;
begin
  Parser := TJSONParser.Create(Str, DefaultOptions);
  try
    Obj := Parser.Parse;
    try
      LoadTestedProblemFromJSONObj(Obj as TJSONObject, TestedProblem);
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(Parser);
  end;
end;

function SaveTestedProblemToJSONStr(TestedProblem: TTestedProblem): TJSONStringType;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSONString(TestedProblem);
  finally
    FreeAndNil(Streamer);
  end;
end;

function SaveMultiTesterToJSONStr(MultiTester: TMultiTester): TJSONStringType;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSONString(MultiTester);
  finally
    FreeAndNil(Streamer);
  end;
end;

end.
