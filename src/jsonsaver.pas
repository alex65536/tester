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
unit jsonsaver;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, problemprops, fpjson, fpjsonrtti, jsonparser, multitesters;

function LoadChecker(Obj: TJSONData): TProblemChecker;
procedure LoadFromJSONObj(Obj: TJSONObject; Props: TProblemProperties);
procedure LoadFromJSONStr(const Str: TJSONStringType; Props: TProblemProperties);

function SaveChecker(Checker: TProblemChecker): TJSONData;
function SavePropsToJSONObj(Props: TProblemProperties): TJSONObject;
function SavePropsToJSONStr(Props: TProblemProperties): TJSONStringType;

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

procedure LoadFromJSONObj(Obj: TJSONObject; Props: TProblemProperties);
var
  DeStreamer: TJSONDeStreamer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(Obj, Props);
    Props.Checker := LoadChecker(Obj['Checker']);
  finally
    FreeAndNil(DeStreamer);
  end;
end;

procedure LoadFromJSONStr(const Str: TJSONStringType; Props: TProblemProperties);
var
  Parser: TJSONParser;
  Obj: TJSONData;
begin
  Parser := TJSONParser.Create(Str);
  try
    Obj := Parser.Parse;
    try
      LoadFromJSONObj(Obj as TJSONObject, Props);
    finally
      FreeAndNil(Obj);
    end;
  finally
    FreeAndNil(Parser);
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
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSON(Props);
    Result['Checker'] := SaveChecker(Props.Checker);
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
