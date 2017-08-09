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
unit testerframes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Grids, StdCtrls, problemprops, Math, strconsts,
  Graphics, testresults, Types, srcviewer, Dialogs, verdictcolors, compilerinfo,
  testinfo, multitesters, testerprimitives, ComCtrls, solutioninfo;

type
  ETesterFrame = class(Exception);

  { TTesterFrame }

  TTesterFrame = class(TFrame)
    ScoreLabel: TLabel;
    DrawGrid: TDrawGrid;
    TimeLabel: TLabel;
    MemoryLabel: TLabel;
    VerdictLabel: TLabel;
    CompilerLabel: TLabel;
    procedure DrawGridClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; Col, Row: integer;
      ARect: TRect; AState: TGridDrawState);
  private
    FThread: TMultiTesterThread;
    FIsTesting: boolean;
    FOnTestingEnd: TNotifyEvent;
    FOnTestingStart: TNotifyEvent;
    FProgressBar: TProgressBar;
    FReady: boolean;
    FMultiTester: TMultiTester;
    function GetProperties: TProblemProperties;
    function GetSources: TStrings;
    function GetTestResultCount: integer;
    function GetTestResults(I: integer): TTestedProblem;
    procedure SetOnTestingEnd(AValue: TNotifyEvent);
    procedure SetOnTestingStart(AValue: TNotifyEvent);
    procedure SetProgressBar(AValue: TProgressBar);
    procedure SetProperties(AValue: TProblemProperties);
    procedure PrepareTable;
    procedure DrawCell(Col, Row: integer; ARect: TRect; ACanvas: TCanvas;
      Draw: boolean; var MaxWidth: integer);
    function CalcMaxWidth(Col, Row: integer): integer;
    procedure UpdateWidths;
    procedure UpdateWidthsCol(ACol: integer);
    procedure UpdateWidthsRow(ARow: integer);
    procedure UpdateWidthCell(ACol, ARow: integer);
  protected
    procedure MultiTesterStart(Sender: TObject);
    procedure MultiTesterUpdate(Sender: TObject; TesterID: integer;
      AKind: TTesterUpdateKind);
    procedure MultiTesterFinish(Sender: TObject);
    procedure MultiTesterException(Sender: TObject; E: Exception);
  public
    property ProgressBar: TProgressBar read FProgressBar write SetProgressBar;
    property IsTesting: boolean read FIsTesting;
    property OnTestingStart: TNotifyEvent read FOnTestingStart write SetOnTestingStart;
    property OnTestingEnd: TNotifyEvent read FOnTestingEnd write SetOnTestingEnd;
    property MultiTester: TMultiTester read FMultiTester;
    property Properties: TProblemProperties read GetProperties write SetProperties;
    property Sources: TStrings read GetSources;
    property TestResults[I: integer]: TTestedProblem read GetTestResults;
    property TestResultCount: integer read GetTestResultCount;
    procedure Prepare;
    procedure Unprepare;
    procedure LaunchTesting;
    procedure TerminateTesting;
    procedure AfterConstruction; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TTesterFrame }

procedure TTesterFrame.SetProperties(AValue: TProblemProperties);
begin
  FMultiTester.Properties := AValue;
end;

function TTesterFrame.GetSources: TStrings;
begin
  Result := FMultiTester.Sources;
end;

function TTesterFrame.GetProperties: TProblemProperties;
begin
  Result := FMultiTester.Properties;
end;

function TTesterFrame.GetTestResultCount: integer;
begin
  Result := FMultiTester.TesterCount;
end;

function TTesterFrame.GetTestResults(I: integer): TTestedProblem;
begin
  Result := FMultiTester.Testers[I].Results;
end;

procedure TTesterFrame.SetOnTestingEnd(AValue: TNotifyEvent);
begin
  if FOnTestingEnd = AValue then
    Exit;
  FOnTestingEnd := AValue;
end;

procedure TTesterFrame.SetOnTestingStart(AValue: TNotifyEvent);
begin
  if FOnTestingStart = AValue then
    Exit;
  FOnTestingStart := AValue;
end;

procedure TTesterFrame.SetProgressBar(AValue: TProgressBar);
begin
  if FProgressBar = AValue then
    Exit;
  FProgressBar := AValue;
end;

procedure TTesterFrame.PrepareTable;

  procedure AddColumn(const ATitle: string);
  begin
    with DrawGrid.Columns.Add do
    begin
      Title.Caption := ATitle;
      Title.Alignment := taCenter;
      Width := DrawGrid.Canvas.TextWidth(ATitle) + 16;
    end;
  end;

var
  CellH, I: integer;

begin
  // work with cols
  DrawGrid.FixedCols := 0;
  DrawGrid.Columns.Clear;
  AddColumn(SSourceName);
  AddColumn(SCompileStatus);
  AddColumn(STotalScore);
  for I := 1 to Properties.TestCount do
    AddColumn(Format(STestIndex, [I]));
  // work with rows
  DrawGrid.RowCount := Sources.Count + 1;
  DrawGrid.FixedRows := 1;
  // update heights
  CellH := VerdictLabel.Canvas.TextHeight('42') + TimeLabel.Canvas.TextHeight('42') +
    MemoryLabel.Canvas.TextHeight('42') + 4;
  DrawGrid.RowHeights[0] := DrawGrid.Canvas.TextHeight('42') + 4;
  for I := 1 to DrawGrid.RowCount - 1 do
    DrawGrid.RowHeights[I] := CellH;
  // update widths
  UpdateWidthsRow(1);
  UpdateWidthsCol(0);
end;

procedure TTesterFrame.UpdateWidths;
var
  I: integer;
begin
  for I := 0 to DrawGrid.ColCount - 1 do
    UpdateWidthsCol(I);
end;

procedure TTesterFrame.UpdateWidthsCol(ACol: integer);
var
  W, I: integer;
begin
  W := DrawGrid.ColWidths[ACol];
  for I := 0 to DrawGrid.RowCount - 1 do
    W := Max(W, CalcMaxWidth(ACol, I));
  DrawGrid.ColWidths[ACol] := W;
end;

procedure TTesterFrame.UpdateWidthsRow(ARow: integer);
var
  I: integer;
begin
  for I := 0 to DrawGrid.ColCount - 1 do
    DrawGrid.ColWidths[I] := Max(DrawGrid.ColWidths[I], CalcMaxWidth(I, ARow));
end;

procedure TTesterFrame.UpdateWidthCell(ACol, ARow: integer);
begin
  DrawGrid.ColWidths[ACol] := Max(DrawGrid.ColWidths[ACol], CalcMaxWidth(ACol, ARow));
end;

procedure TTesterFrame.MultiTesterStart(Sender: TObject);
var
  CurTester: TMultiTester;
begin
  FIsTesting := True;
  if Assigned(FOnTestingStart) then
    FOnTestingStart(Self);
  if Assigned(FProgressBar) then
  begin
    FProgressBar.Min := 0;
    FProgressBar.Max := Sources.Count * (Properties.TestCount + 1);
    FProgressBar.Position := 0;
    FProgressBar.Step := 1;
  end;
  CurTester := (Sender as TMultiTesterThread).MultiTester;
  FMultiTester.Assign(CurTester);
  Prepare;
end;

procedure TTesterFrame.MultiTesterUpdate(Sender: TObject; TesterID: integer;
  AKind: TTesterUpdateKind);
var
  CurTester: TMultiTester;
  NeedsUpdate: array of boolean;
  I: integer;
begin
  if Assigned(FProgressBar) and (AKind in [ukCompile, ukTest, ukTestSkip]) then
    FProgressBar.StepIt;
  if AKind = ukTestSkip then
    Exit;
  CurTester := (Sender as TMultiTesterThread).MultiTester;
  // decide whom to update
  SetLength(NeedsUpdate, DrawGrid.ColCount);
  for I := 0 to DrawGrid.ColCount - 1 do
    NeedsUpdate[I] := False;
  NeedsUpdate[1] := True;
  NeedsUpdate[2] := True;
  for I := 0 to CurTester.Testers[TesterID].Results.Items.Count - 1 do
    NeedsUpdate[3 + I] :=
      (CurTester.Testers[TesterID].Results[I].Verdict <> veWaiting) and
      (FMultiTester.Testers[TesterID].Results[I].Verdict = veWaiting);
  // reassign
  FMultiTester.Testers[TesterID].Assign(CurTester.Testers[TesterID]);
  // update
  for I := 0 to DrawGrid.ColCount - 1 do
    if NeedsUpdate[I] then
      UpdateWidthCell(I, TesterID + 1);
  // repaint
  Repaint;
end;

procedure TTesterFrame.MultiTesterFinish(Sender: TObject);
begin
  Repaint;
  if Assigned(FOnTestingEnd) then
    FOnTestingEnd(Self);
  FIsTesting := False;
end;

procedure TTesterFrame.MultiTesterException(Sender: TObject; E: Exception);
begin
  MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TTesterFrame.DrawCell(Col, Row: integer; ARect: TRect;
  ACanvas: TCanvas; Draw: boolean; var MaxWidth: integer);

  procedure DrawTextCenter(ARect: TRect; AText: string);
  var
    Sz: TSize;
  begin
    Sz := ACanvas.TextExtent(AText);
    MaxWidth := Max(MaxWidth, Sz.cx + 16);
    if Draw then
      ACanvas.TextOut((ARect.Left + ARect.Right - Sz.cx) div 2,
        (ARect.Top + ARect.Bottom - Sz.cy) div 2, AText);
  end;

  procedure RestoreFont;
  begin
    DrawGrid.Canvas.Font.Assign(Self.Font);
  end;

var
  CompilerVerdict: TCompilerVerdict;
  TestVerdict: TTestVerdict;
  TestVerdictStr: string;
  TimeStr: string;
  MemStr: string;
  H1, H2, H3: integer;
  NewRect: TRect;
  Score, MaxScore: double;

begin
  if not FReady then
    Exit;
  if Row = 0 then
    Exit;
  Dec(Row);
  MaxWidth := 0;
  if Col = 0 then // source name
  begin
    ACanvas.Font.Assign(Font);
    DrawTextCenter(ARect, ExtractFileName(Sources[Row]));
    RestoreFont;
    Exit;
  end;
  if Col = 1 then // compile status
  begin
    ACanvas.Font.Assign(CompilerLabel.Font);
    CompilerVerdict := TestResults[Row].CompileVerdict;
    ACanvas.Font.Color := CompilerVerdictColors[CompilerVerdict];
    DrawTextCenter(ARect, SCompilerVerdictsS[CompilerVerdict]);
    RestoreFont;
    Exit;
  end;
  if Col = 2 then // score
  begin
    ACanvas.Font.Assign(ScoreLabel.Font);
    MaxScore := Properties.MaxScore;
    Score := TestResults[Row].TotalScore;
    ACanvas.Font.Color := GetTotalScoreColor(Score, MaxScore);
    DrawTextCenter(ARect, Format(SScoreFmt, [Score]));
    RestoreFont;
    Exit;
  end;
  if (3 <= Col) and (Col <= Properties.TestCount + 2) then // tests
  begin
    NewRect := ARect;
    // test verdict
    ACanvas.Font.Assign(VerdictLabel.Font);
    TestVerdict := TestResults[Row][Col - 3].Verdict;
    TestVerdictStr := STestVerdictsS[TestVerdict];
    ACanvas.Font.Color := TestVerdictColors[TestVerdict];
    H1 := ACanvas.TextHeight(TestVerdictStr);
    NewRect.Bottom := NewRect.Top + H1;
    DrawTextCenter(NewRect, TestVerdictStr);
    // time
    ACanvas.Font.Assign(TimeLabel.Font);
    TimeStr := ProblemTimeToStr(TestResults[Row][Col - 3].Time);
    H2 := ACanvas.TextHeight(TimeStr);
    NewRect.Top := NewRect.Bottom;
    NewRect.Bottom := NewRect.Top + H2;
    DrawTextCenter(NewRect, TimeStr);
    // memory
    ACanvas.Font.Assign(MemoryLabel.Font);
    MemStr := ProblemMemoryToStr(TestResults[Row][Col - 3].Memory);
    H3 := ACanvas.TextHeight(MemStr);
    NewRect.Top := NewRect.Bottom;
    NewRect.Bottom := NewRect.Top + H3;
    DrawTextCenter(NewRect, MemStr);
    RestoreFont;
    Exit;
  end;
  // we shoudn't have reached the end...
  raise Exception.Create('We shoudn''t have reached the end...');
end;

procedure TTesterFrame.DrawGridClick(Sender: TObject);
var
  Col, Row: integer;
begin
  Col := DrawGrid.Col;
  Row := DrawGrid.Row;
  if Row = 0 then
    Exit;
  Dec(Row);
  if Col = 0 then // source name
  begin
    try
      ViewSource(Sources[Row]);
    except
      on E: Exception do
        MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
    Exit;
  end;
  if Col = 1 then // compile status
  begin
    if TestResults[Row].CompileVerdict <> cvWaiting then
      ShowCompilerInfo(TestResults[Row]);
    Exit;
  end;
  if Col = 2 then // score
  begin
    ShowSolutionStats(Properties, TestResults[Row]);
    Exit;
  end;
  if (3 <= Col) and (Col <= Properties.TestCount + 2) then // tests
  begin
    if not (TestResults[Row][Col - 3].Verdict in [veWaiting, veSkipped]) then
      ShowTestInfo(Properties.Tests[Col - 3], TestResults[Row][Col - 3]);
    Exit;
  end;
  // we shoudn't have reached the end...
  raise Exception.Create('We shoudn''t have reached the end...');
end;

procedure TTesterFrame.DrawGridDrawCell(Sender: TObject; Col, Row: integer;
  ARect: TRect; AState: TGridDrawState);
var
  Temp: integer;
begin
  AState := AState; // to prevent a hint
  Temp := 0;
  DrawCell(Col, Row, ARect, DrawGrid.Canvas, True, Temp);
end;

function TTesterFrame.CalcMaxWidth(Col, Row: integer): integer;
begin
  Result := 0;
  DrawCell(Col, Row, Rect(0, 0, 0, 0), DrawGrid.Canvas, False, Result);
end;

procedure TTesterFrame.Prepare;
begin
  FReady := True;
  PrepareTable;
  Repaint;
end;

procedure TTesterFrame.Unprepare;
begin
  FReady := False;
  DrawGrid.FixedCols := 0;
  DrawGrid.Columns.Clear;
  DrawGrid.RowCount := 0;
end;

procedure TTesterFrame.LaunchTesting;
begin
  if FIsTesting then
    raise ETesterFrame.Create(SAlreadyTesting);
  Unprepare;
  FThread := TMultiTesterThread.Create(True);
  with FThread do
  begin
    FreeOnTerminate := True;
    MultiTester.Assign(FMultiTester);
    OnStart := @MultiTesterStart;
    OnUpdate := @MultiTesterUpdate;
    OnFinish := @MultiTesterFinish;
    OnException := @MultiTesterException;
    Start;
  end;
end;

procedure TTesterFrame.TerminateTesting;
begin
  FThread.Terminate;
  FThread.WaitFor;
end;

procedure TTesterFrame.AfterConstruction;
var
  DefaultHeight: integer;
begin
  inherited AfterConstruction;
  DefaultHeight := GetFontData(Font.Reference.Handle).Height;
  CompilerLabel.Font.Height := Round(1.5 * DefaultHeight);
  VerdictLabel.Font.Height := Round(1.5 * DefaultHeight);
  TimeLabel.Font.Height := Round(0.75 * DefaultHeight);
  MemoryLabel.Font.Height := Round(0.75 * DefaultHeight);
  ScoreLabel.Font.Height := Round(1.5 * DefaultHeight);
  Unprepare;
end;

constructor TTesterFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMultiTester := TMultiTester.Create;
  FReady := False;
  FIsTesting := False;
  DrawGrid.DoubleBuffered := True;
end;

destructor TTesterFrame.Destroy;
begin
  FreeAndNil(FMultiTester);
  inherited Destroy;
end;

end.
