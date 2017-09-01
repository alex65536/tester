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
unit testinfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, ButtonPanel, strconsts, verdictcolors,
  testresults, problemprops, Classes, baseforms, strverdicts;

type

  { TTestInfoDlg }

  TTestInfoDlg = class(TBaseForm)
    ButtonPanel: TButtonPanel;
    CheckerOutputMemo: TMemo;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    MemoryLabel: TLabel;
    Panel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    TimeLabel: TLabel;
    VerdictLabel: TLabel;
    ScoreLabel: TLabel;
  private
    FTest: TProblemTest;
    FResults: TTestResult;
    procedure AssignData;
  public
    procedure Show(ATest: TProblemTest; AResults: TTestResult);
  end;

var
  TestInfoDlg: TTestInfoDlg;

procedure ShowTestInfo(ATest: TProblemTest; AResults: TTestResult);

implementation

procedure ShowTestInfo(ATest: TProblemTest; AResults: TTestResult);
var
  AForm: TTestInfoDlg;
begin
  AForm := TTestInfoDlg.Create(nil);
  try
    AForm.Show(ATest, AResults);
  finally
    FreeAndNil(AForm);
  end;
end;

{$R *.lfm}

{ TTestInfoDlg }

procedure TTestInfoDlg.AssignData;
begin
  VerdictLabel.Font.Color := TestVerdictColors[FResults.Verdict];
  VerdictLabel.Caption := STestVerdicts[FResults.Verdict];
  ScoreLabel.Font.Color := GetTotalScoreColor(FResults.Score, FTest.Cost);
  ScoreLabel.Caption := Format(SScoreDivide, [FResults.Score, FTest.Cost]);
  TimeLabel.Caption := ProblemTimeToStrEx(FResults.Time);
  MemoryLabel.Caption := ProblemMemoryToStrEx(FResults.Memory);
  CheckerOutputMemo.Text := FResults.CheckerOutput;
end;

procedure TTestInfoDlg.Show(ATest: TProblemTest; AResults: TTestResult);
begin
  FResults := AResults;
  FTest := ATest;
  AssignData;
  ShowModal;
end;

end.
