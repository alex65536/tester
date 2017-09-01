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
unit solutioninfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, baseforms, StdCtrls, ButtonPanel, ExtCtrls, testresults,
  problemstats, problemprops, strconsts, verdictcolors, strverdicts;

type

  { TSolutionStatsDlg }

  TSolutionStatsDlg = class(TBaseForm)
    TotalTimeLabel: TLabel;
    Label12: TLabel;
    Panel12: TPanel;
    ScoreLabel: TLabel;
    TestsPassedLabel: TLabel;
    TestsFailedLabel: TLabel;
    TestsSkippedLabel: TLabel;
    TestsWaitingLabel: TLabel;
    MinTimeLabel: TLabel;
    MaxTimeLabel: TLabel;
    AverageTimeLabel: TLabel;
    MinMemoryLabel: TLabel;
    MaxMemoryLabel: TLabel;
    AverageMemoryLabel: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ButtonPanel: TButtonPanel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel: TPanel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
  private
    FProps: TProblemProperties;
    FResults: TTestedProblem;
    procedure AssignData;
  public
    procedure Show(AProps: TProblemProperties; AResults: TTestedProblem);
  end;

var
  SolutionStatsDlg: TSolutionStatsDlg;

procedure ShowSolutionStats(AProps: TProblemProperties; AResults: TTestedProblem);

implementation

procedure ShowSolutionStats(AProps: TProblemProperties; AResults: TTestedProblem);
var
  AForm: TSolutionStatsDlg;
begin
  AForm := TSolutionStatsDlg.Create(nil);
  try
    AForm.Show(AProps, AResults);
  finally
    FreeAndNil(AForm);
  end;
end;

{$R *.lfm}

{ TSolutionStatsDlg }

procedure TSolutionStatsDlg.AssignData;
var
  Stats: TProblemStats;
begin
  Stats := TProblemStats.Create(FResults);
  try
    ScoreLabel.Font.Color := GetTotalScoreColor(FResults.TotalScore, FProps.MaxScore);
    ScoreLabel.Caption := Format(SScoreDivide, [FResults.TotalScore, FProps.MaxScore]);
    with Stats do
    begin
      // tests
      if CanCountTests then
      begin
        TestsPassedLabel.Caption := GetTestsOfCaption(TestsPassed, TestsTotal);
        TestsFailedLabel.Caption := GetTestsOfCaption(TestsFailed, TestsTotal);
        TestsSkippedLabel.Caption := GetTestsOfCaption(TestsSkipped, TestsTotal);
        TestsWaitingLabel.Caption := GetTestsOfCaption(TestsWaiting, TestsTotal);
      end
      else
      begin
        TestsPassedLabel.Caption := SNoAnswer;
        TestsFailedLabel.Caption := SNoAnswer;
        TestsSkippedLabel.Caption := SNoAnswer;
        TestsWaitingLabel.Caption := SNoAnswer;
      end;
      // time
      if CanCountTime then
      begin
        MinTimeLabel.Caption := ProblemTimeToStrEx(MinTime);
        MaxTimeLabel.Caption := ProblemTimeToStrEx(MaxTime);
        AverageTimeLabel.Caption := ProblemTimeToStrEx(AverageTime);
        TotalTimeLabel.Caption := ProblemTimeToStrEx(TotalTime);
      end
      else
      begin
        MinTimeLabel.Caption := SNoAnswer;
        MaxTimeLabel.Caption := SNoAnswer;
        AverageTimeLabel.Caption := SNoAnswer;
        TotalTimeLabel.Caption := SNoAnswer;
      end;
      // memory
      if CanCountMemory then
      begin
        MinMemoryLabel.Caption := ProblemMemoryToStrEx(MinMemory);
        MaxMemoryLabel.Caption := ProblemMemoryToStrEx(MaxMemory);
        AverageMemoryLabel.Caption := ProblemMemoryToStrEx(AverageMemory);
      end
      else
      begin
        MinMemoryLabel.Caption := SNoAnswer;
        MaxMemoryLabel.Caption := SNoAnswer;
        AverageMemoryLabel.Caption := SNoAnswer;
      end;
    end;
  finally
    FreeAndNil(Stats);
  end;
end;

procedure TSolutionStatsDlg.Show(AProps: TProblemProperties; AResults: TTestedProblem);
begin
  FProps := AProps;
  FResults := AResults;
  AssignData;
  ShowModal;
end;

end.
