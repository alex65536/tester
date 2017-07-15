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
unit parser_polygon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, propsparserbase, problemprops, LazFileUtils, testerfileutil,
  LazFileCache, logfile, checkers;

type

  { TPolygonPropertiesParser }

  TPolygonPropertiesParser = class(TPropertiesParserBase)
  protected
    function DoParse: boolean; override;
  end;

implementation

uses
  Laz_XMLRead, Laz2_DOM, Laz_DOM, formatutils;

{ TPolygonPropertiesParser }

function TPolygonPropertiesParser.DoParse: boolean;
var
  XMLDocument: TXMLDocument;

  function Parser: boolean;
  var
    RootNode, JudgingNode, TestsetNode, AssetsNode, CheckerNode, BinaryNode: TDOMElement;
    CheckerPath: string;
    Success: boolean;

    function GetTestsetNode(const ANodeName: string): string; inline;
    begin
      Result := TestsetNode.FindNode(ANodeName).TextContent;
    end;

    function FirstTestsetNode: TDOMNode;
    begin
      Result := JudgingNode.FirstChild;
      while Result <> nil do
      begin
        if Result.CompareName('testset') = 0 then
          Exit;
        Result := Result.NextSibling;
      end;
    end;

    function NextTestsetNode: TDOMNode;
    begin
      Result := TestsetNode.NextSibling;
      while Result <> nil do
      begin
        if Result.CompareName('testset') = 0 then
          Exit;
        Result := Result.NextSibling;
      end;
    end;

    procedure AddTestsetNode;
    var
      TestCount: integer;
      InputTestFmt, OutputTestFmt: string;
    begin
      // parse TL, ML
      with Properties, TProblemPropsCollector do
      begin
        TimeLimit := MergeInt(TimeLimit, StrToInt(GetTestsetNode('time-limit')),
          Success);
        MemoryLimit := MergeInt(MemoryLimit, StrToInt(GetTestsetNode('memory-limit')) div
          1024, Success);
      end;
      // parse test info
      TestCount := StrToInt(GetTestsetNode('test-count'));
      InputTestFmt := FromCppFormat(GetTestsetNode('input-path-pattern'));
      InputTestFmt := AppendPathDelim(WorkingDir) + InputTestFmt;
      OutputTestFmt := FromCppFormat(GetTestsetNode('answer-path-pattern'));
      OutputTestFmt := AppendPathDelim(WorkingDir) + OutputTestFmt;
      if IsTerminated then
        Exit;
      // add tests
      AddTestsFmt(InputTestFmt, OutputTestFmt, TestCount);
      if IsTerminated then
        Exit;
    end;

  begin
    Result := False;
    Success := True;
    RootNode := XMLDocument.DocumentElement;
    JudgingNode := RootNode.FindNode('judging') as TDOMElement;
    // if "judging" node has no attributes - XML is not Polygon's
    if JudgingNode.Attributes.Length = 0 then
    begin
      Result := True;
      Exit;
    end;
    // initialize
    with TProblemPropsCollector do
      CheckerPath := UnknownStr;
    // parse input and output file - "judging" node
    Properties.InputFile := JudgingNode.GetAttribute('input-file');
    Properties.OutputFile := JudgingNode.GetAttribute('output-file');
    if IsTerminated then
      Exit;
    // parsing all the "judging/testset" nodes
    BeginCache;
    try
      TestsetNode := FirstTestsetNode as TDOMElement;
      while TestsetNode <> nil do
      begin
        AddTestsetNode;
        if IsTerminated then
          Break;
        TestsetNode := NextTestsetNode as TDOMElement;
      end;
    finally
      EndCache;
    end;
    // parse checker - "assets/checker" node
    AssetsNode := RootNode.FindNode('assets') as TDOMElement;
    CheckerNode := AssetsNode.FindNode('checker') as TDOMElement;
    BinaryNode := CheckerNode.FindNode('binary') as TDOMElement;
    CheckerPath := AppendPathDelim(WorkingDir) + BinaryNode.GetAttribute('path');
    if not FileExistsUTF8(CheckerPath) then
      Success := False;
    CheckerPath := CreateRelativePath(CheckerPath, WorkingDir);
    WriteLog('checker path = ' + CheckerPath);
    Properties.Checker := TTestlibChecker.Create(CheckerPath);
    // that's all! ;)
    Result := Success;
  end;

var
  XMLFileName: string;
begin
  // load xml
  XMLFileName := CorrectFileName(AppendPathDelim(WorkingDir) + 'problem.xml');
  if not FileExistsUTF8(XMLFileName) then
    Exit;
  ReadXMLFile(XMLDocument, XMLFileName);
  try
    // parse xml
    Result := Parser;
  finally
    FreeAndNil(XMLDocument);
  end;
end;

end.
