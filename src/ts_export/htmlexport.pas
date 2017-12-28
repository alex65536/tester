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
unit htmlexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, multitesters, typinfo, testerprimitives, Graphics,
  verdictcolors, strconsts, strverdicts, versioninfo;

type

  { TIndentStringList }

  TIndentStringList = class(TStringList)
  private
    FIndent: integer;
    procedure SetIndent(AValue: integer);
  protected
    procedure InsertItem(Index: integer; const S: string; O: TObject); override;
  public
    property Indent: integer read FIndent write SetIndent;
  end;

  { THTMLExportStringList }

  THTMLExportStringList = class(TIndentStringList)
  private
    function MakeOpenTag(const ATag, AAttrib: string): string;
    function MakeCloseTag(const ATag: string): string;
    function MakeComment(const AComm: string): string;
  public
    procedure Clear; override;
    procedure AddComment(const AComm: string);
    procedure BeginTag(const ATag: string; const AAttrib: string = '';
      NoEnd: boolean = False);
    procedure EndTag(const ATag: string);
    procedure SingleLineTag(const ATag, AAttrib, AContent: string);
  end;

  { TMultiTesterHTMLExporter }

  TMultiTesterHTMLExporter = class
  private
    FDocument: THTMLExportStringList;
    FMultiTester: TMultiTester;
    FStyleTable: TStringList;
    function EnumNameToStyleName(const Pref, S: string): string;
    function TestVerdictClassName(AVerdict: TTestVerdict): string;
    function CompilerVerdictClassName(AVerdict: TCompilerVerdict): string;
    procedure PrepareStyleTable;
    function GetDocument: TStringList;
    procedure SetMultiTester(AValue: TMultiTester);
  public
    property MultiTester: TMultiTester read FMultiTester write SetMultiTester;
    property Document: TStringList read GetDocument;
    procedure ExportHTML;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  IndentDelta = 1;
  IndentFill = #9;

{ THTMLExportStringList }

function THTMLExportStringList.MakeOpenTag(const ATag, AAttrib: string): string;
begin
  Result := '<' + ATag;
  if AAttrib <> '' then
    Result := Result + ' ' + AAttrib;
  Result := Result + '>';
end;

function THTMLExportStringList.MakeCloseTag(const ATag: string): string;
begin
  Result := '</' + ATag + '>';
end;

function THTMLExportStringList.MakeComment(const AComm: string): string;
begin
  Result := '<!-- ' + AComm + ' -->';
end;

procedure THTMLExportStringList.Clear;
begin
  inherited Clear;
  Indent := 0;
  Add('<!DOCTYPE html>');
end;

procedure THTMLExportStringList.AddComment(const AComm: string);
begin
  if AComm <> '' then
    Add(MakeComment(AComm));
end;

procedure THTMLExportStringList.BeginTag(const ATag: string;
  const AAttrib: string; NoEnd: boolean);
begin
  Add(MakeOpenTag(ATag, AAttrib));
  if not NoEnd then
    Indent := Indent + IndentDelta;
end;

procedure THTMLExportStringList.EndTag(const ATag: string);
begin
  Indent := Indent - IndentDelta;
  Add(MakeCloseTag(ATag));
end;

procedure THTMLExportStringList.SingleLineTag(const ATag, AAttrib, AContent: string);
var
  Cont: string;
begin
  Cont := AContent;
  Add(MakeOpenTag(ATag, AAttrib) + Cont + MakeCloseTag(ATag));
end;

{ TIndentStringList }

procedure TIndentStringList.SetIndent(AValue: integer);
begin
  if FIndent = AValue then
    Exit;
  FIndent := AValue;
end;

procedure TIndentStringList.InsertItem(Index: integer; const S: string; O: TObject);
var
  IndentStr: string;
  I: integer;
begin
  IndentStr := '';
  for I := 0 to Indent - 1 do
    IndentStr := IndentStr + IndentFill;
  inherited InsertItem(Index, IndentStr + S, O);
end;

{ TMultiTesterHTMLExporter }

function TMultiTesterHTMLExporter.EnumNameToStyleName(const Pref, S: string): string;
var
  I: integer;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] in ['a' .. 'z']) do
    Inc(I);
  Result := Pref;
  while I <= Length(S) do
  begin
    if not (S[I] in ['a' .. 'z']) then
      Result += '-';
    Result += LowerCase(S[I]);
    Inc(I);
  end;
end;

function TMultiTesterHTMLExporter.TestVerdictClassName(AVerdict: TTestVerdict): string;
begin
  Result := EnumNameToStyleName('test',
    GetEnumName(TypeInfo(TTestVerdict), Ord(AVerdict)));
end;

function TMultiTesterHTMLExporter.CompilerVerdictClassName(AVerdict:
  TCompilerVerdict): string;
begin
  Result := EnumNameToStyleName('compile',
    GetEnumName(TypeInfo(TCompilerVerdict), Ord(AVerdict)));
end;

procedure TMultiTesterHTMLExporter.PrepareStyleTable;

  procedure AddColorBlock(const AName: string; AColor: TColor);
  const
    ColorBlockFormat =
      '.%s {' + LineEnding +
      IndentFill + 'color: rgb(%d, %d, %d);' + LineEnding +
      '}';
  begin
    FStyleTable.AddText(Format(ColorBlockFormat,
      [AName, Red(AColor), Green(AColor), Blue(AColor)]));
  end;

var
  T: TTestVerdict;
  C: TCompilerVerdict;
begin
  if FStyleTable <> nil then
    Exit;
  FStyleTable := TStringList.Create;
  FStyleTable.AddText({$I defaultcss.inc});
  for C := Low(TCompilerVerdict) to High(TCompilerVerdict) do
    AddColorBlock(CompilerVerdictClassName(C), CompilerVerdictColors[C]);
  for T := Low(TTestVerdict) to High(TTestVerdict) do
    AddColorBlock(TestVerdictClassName(T), TestVerdictColors[T]);
end;

function TMultiTesterHTMLExporter.GetDocument: TStringList;
begin
  Result := FDocument;
end;

procedure TMultiTesterHTMLExporter.SetMultiTester(AValue: TMultiTester);
begin
  if FMultiTester = AValue then
    Exit;
  FMultiTester := AValue;
end;

procedure TMultiTesterHTMLExporter.ExportHTML;

  procedure AddHeaderCell(const AContent: string);
  begin
    with FDocument do
      SingleLineTag('th', '', AContent);
  end;

  function TableCellAttr: string;
  begin
    Result := '';
  end;

  function ScoreCellAttr(CurScore, MaxScore: double): string;
  var
    AColor: TColor;
  begin
    AColor := GetTotalScoreColor(CurScore, MaxScore);
    Result := Format('class="big-label" style="color: rgb(%d, %d, %d)"',
      [Red(AColor), Green(AColor), Blue(AColor)]);
  end;

  function CompileCellAttr(AVerdict: TCompilerVerdict): string;
  begin
    Result := Format('class="big-label %s"',
      [CompilerVerdictClassName(AVerdict)]);
  end;

  function TestDivAttr(AVerdict: TTestVerdict): string;
  begin
    Result := Format('class="big-label %s"', [TestVerdictClassName(AVerdict)]);
  end;

  function GetTimeMemContent(const ATime: TProblemTime;
    AMem: TProblemMemory): string;
  begin
    Result := ProblemTimeToStr(ATime) + '<br>' + ProblemMemoryToStr(AMem);
  end;

var
  TestCount, TesterCount: integer;
  I, J: integer;
  C: TCompilerVerdict;
  T: TTestVerdict;
  Cur, Max: double;
begin
  TestCount := FMultiTester.Properties.TestCount;
  TesterCount := FMultiTester.TesterCount;
  PrepareStyleTable;
  with FDocument do
  begin
    Clear;
    AddComment(Format(SHTMLComment, [GetAppVersion]));
    BeginTag('html');
      BeginTag('head');
        BeginTag('meta', 'http-equiv="Content-Type" content="text/html; charset=utf-8"', True);
        SingleLineTag('title', '', SHTMLTitle);
        BeginTag('style');
          AddStrings(FStyleTable);
        EndTag('style');
      EndTag('head');
      BeginTag('body');
        BeginTag('table', 'class="score-tab"');
          BeginTag('tr');
            AddHeaderCell(SSourceName);
            AddHeaderCell(SCompileStatus);
            AddHeaderCell(STotalScore);
            for I := 1 to TestCount do
              AddHeaderCell(Format(STestIndex, [I]));
          EndTag('tr');
          for I := 0 to TesterCount - 1 do
            with FMultiTester do
            begin
              BeginTag('tr');
                // source
                SingleLineTag('td', TableCellAttr, ExtractFileName(Sources[I]));
                // compile
                C := Testers[I].Results.CompileVerdict;
                SingleLineTag('td', CompileCellAttr(C), SCompilerVerdictsS[C]);
                // score
                Cur := Testers[I].Results.TotalScore;
                Max := Properties.MaxScore;
                SingleLineTag('td', ScoreCellAttr(Cur, Max), Format(SScoreFmt, [Cur]));
                for J := 0 to Testers[I].Results.TestResultsCount - 1 do
                  with Testers[I].Results[J] do
                  begin
                    T := Verdict;
                    // test
                    BeginTag('td', '');
                      SingleLineTag('div', TestDivAttr(T), STestVerdictsS[T]);
                      SingleLineTag('div', 'class="small-label"', GetTimeMemContent(Time, Memory));
                    EndTag('td');
                  end;
              EndTag('tr');
            end;
        EndTag('table');
      EndTag('body');
    EndTag('html');
  end;
end;

constructor TMultiTesterHTMLExporter.Create;
begin
  FDocument := THTMLExportStringList.Create;
  FDocument.Clear;
  FStyleTable := nil;
end;

destructor TMultiTesterHTMLExporter.Destroy;
begin
  FreeAndNil(FStyleTable);
  FreeAndNil(FDocument);
  inherited Destroy;
end;

end.
