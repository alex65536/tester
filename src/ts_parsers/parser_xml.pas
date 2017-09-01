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
unit parser_xml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, propsparserbase, problemprops, LazFileUtils, testerfileutil,
  LazFileCache, logfile, checkers, Laz_XMLRead, Laz2_DOM, Laz_DOM, parserutils,
  testerprimitives, testtemplates;

type

  { TXMLPropertiesParser }

  TXMLPropertiesParser = class(TPropertiesParserBase)
  protected
    procedure AddTests(BaseNode: TDOMElement; var Success: boolean);
    procedure AddTestsetNode(TestsetNode: TDOMElement; var Success: boolean);
      virtual; abstract;
    function Parser(XMLDocument: TXMLDocument): boolean; virtual; abstract;
    function DoParse: boolean; override;
  end;

  { TPolygonPropertiesParser }

  TPolygonPropertiesParser = class(TXMLPropertiesParser)
  protected
    procedure AddTestsetNode(TestsetNode: TDOMElement; var Success: boolean); override;
    function Parser(XMLDocument: TXMLDocument): boolean; override;
  end;

  { TRoiPropertiesParser }

  TRoiPropertiesParser = class(TXMLPropertiesParser)
  private
    procedure AddTest(ATest: TProblemTest; out Stop: boolean);
  protected
    procedure AddTestsetNode(TestsetNode: TDOMElement; var Success: boolean); override;
    function Parser(XMLDocument: TXMLDocument): boolean; override;
  end;

implementation

{ TRoiPropertiesParser }

procedure TRoiPropertiesParser.AddTest(ATest: TProblemTest; out Stop: boolean);
begin
  Properties.AddTest(ATest);
  Stop := IsTerminated;
end;

procedure TRoiPropertiesParser.AddTestsetNode(TestsetNode: TDOMElement;
  var Success: boolean);
var
  InputTestFmt, OutputTestFmt: string;
  OurTL: TProblemTime;
  OurML: TProblemMemory;
  OurInput, OurOutput: string;

  function FindTests: boolean;
  var
    ATemplate: TProblemTestTemplate;
  begin
    Result := True;
    ATemplate := TProblemTestTemplate.Create('', '', 1, 1);
    try
      ATemplate.InputFile := InputTestFmt;
      ATemplate.OutputFile := OutputTestFmt;
      try
        ATemplate.GenerateTests(WorkingDir, @AddTest);
      except
        on E: ETestTemplate do
          // mute the exception
        else
          Result := False;
      end;
    finally
      FreeAndNil(ATemplate);
    end;
  end;

begin
  WriteLog('RoiParsers adds the node');
  // parse TL, ML, input, output files
  OurTL := StrToTimeLimit(TestsetNode.GetAttribute('time-limit'));
  OurML := StrToMemoryLimit(TestsetNode.GetAttribute('memory-limit'));
  OurInput := TestsetNode.GetAttribute('input-name');
  OurOutput := TestsetNode.GetAttribute('output-name');
  WriteLogFmt('RoiParser found tl=%d, ml=%d, input=%s, output=%s',
    [OurTL, OurML, OurInput, OurOutput]);
  // merge everything carefully
  with Properties, TProblemPropsCollector do
  begin
    TimeLimit := MergeInt(TimeLimit, OurTL, Success);
    MemoryLimit := MergeInt(MemoryLimit, OurML, Success);
    InputFile := MergeStr(InputFile, OurInput, Success);
    OutputFile := MergeStr(OutputFile, OurOutput, Success);
  end;
  WriteLog('RoiParser merged them');
  if not Success then
    WriteLog('..but unsuccessfully');
  // parse test info
  InputTestFmt := FromRoiFormat(TestsetNode.GetAttribute('input-href'));
  OutputTestFmt := FromRoiFormat(TestsetNode.GetAttribute('answer-href'));
  WriteLogFmt('RoiParser found infmt=%s outfmt=%s', [InputTestFmt, OutputTestFmt]);
  if IsTerminated then
    Exit;
  // add tests
  if not FindTests then
    Success := False;
end;

function TRoiPropertiesParser.Parser(XMLDocument: TXMLDocument): boolean;
var
  ProblemNode, JudgingNode, ScriptNode: TDOMElement;
  Success: boolean;
begin
  Result := False;
  Success := True;
  // go to "problem/judging"
  ProblemNode := XMLDocument.DocumentElement;
  JudgingNode := ProblemNode.FindNode('judging') as TDOMElement;
  // we must find "script node"
  if JudgingNode.FindNode('script') = nil then
  begin
    Result := True;
    Exit;
  end;
  ScriptNode := JudgingNode.FindNode('script') as TDOMElement;
  WriteLogFmt('RoiParser found script node : %p', [Pointer(ScriptNode)]);
  // parsing all the stuff
  AddTests(ScriptNode, Success);
  if IsTerminated then
    Exit;
  // parsing the checker

  // TODO: add checker parsing here (maybe no?)

  // that's all! :)
  Result := Success;
end;

{ TPolygonPropertiesParser }

procedure TPolygonPropertiesParser.AddTestsetNode(TestsetNode: TDOMElement;
  var Success: boolean);

  function GetSonContent(const ANodeName: string): string; inline;
  begin
    Result := TestsetNode.FindNode(ANodeName).TextContent;
  end;

var
  OurTL: TProblemTime;
  OurML: TProblemMemory;
  TestCount: integer;
  InputTestFmt, OutputTestFmt: string;
begin
  // parse TL, ML
  OurTL := StrToTimeLimit(GetSonContent('time-limit'));
  OurML := StrToMemoryLimit(GetSonContent('memory-limit'));
  with Properties, TProblemPropsCollector do
  begin
    TimeLimit := MergeInt(TimeLimit, OurTL, Success);
    MemoryLimit := MergeInt(MemoryLimit, OurML, Success);
  end;
  // parse test info
  TestCount := StrToInt(GetSonContent('test-count'));
  InputTestFmt := FromCppFormat(GetSonContent('input-path-pattern'));
  InputTestFmt := AppendPathDelim(WorkingDir) + InputTestFmt;
  OutputTestFmt := FromCppFormat(GetSonContent('answer-path-pattern'));
  OutputTestFmt := AppendPathDelim(WorkingDir) + OutputTestFmt;
  if IsTerminated then
    Exit;
  // add tests
  AddTestsFmt(InputTestFmt, OutputTestFmt, TestCount);
end;

function TPolygonPropertiesParser.Parser(XMLDocument: TXMLDocument): boolean;
var
  RootNode, JudgingNode, AssetsNode, CheckerNode, BinaryNode: TDOMElement;
  CheckerPath: string;
  Success: boolean;
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
  AddTests(JudgingNode, Success);
  if IsTerminated then
    Exit;
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

{ TXMLPropertiesParser }

procedure TXMLPropertiesParser.AddTests(BaseNode: TDOMElement; var Success: boolean);
var
  TestsetNode: TDOMElement;

  function FirstTestsetNode: TDOMNode;
  begin
    Result := BaseNode.FirstChild;
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

begin
  BeginCache;
  try
    TestsetNode := FirstTestsetNode as TDOMElement;
    while TestsetNode <> nil do
    begin
      AddTestsetNode(TestsetNode, Success);
      if IsTerminated then
        Break;
      TestsetNode := NextTestsetNode as TDOMElement;
    end;
  finally
    EndCache;
  end;
end;

function TXMLPropertiesParser.DoParse: boolean;

  procedure TryXML(const XMLFileName: string);
  var
    XMLDocument: TXMLDocument;
  begin
    // load xml
    if not FileExistsUTF8(XMLFileName) then
      Exit;
    ReadXMLFile(XMLDocument, XMLFileName);
    try
      // parse xml
      Result := Parser(XMLDocument);
    finally
      FreeAndNil(XMLDocument);
    end;
  end;

begin
  TryXML(CorrectFileName(AppendPathDelim(WorkingDir) + 'problem.xml'));
  TryXML(CorrectFileName(AppendPathDelim(WorkingDir) + 'pcms' + PathDelim +
    'problem.xml'));
end;

end.
