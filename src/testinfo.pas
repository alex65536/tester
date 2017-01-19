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
unit testinfo;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ExtCtrls, StdCtrls, ButtonPanel, strconsts,
  verdictcolors, testresults, problemprops;

type

  { TTestInfoDlg }

  TTestInfoDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
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
  public
    procedure Show(ATest: TProblemTest; AResults: TTestResult);
  end;

var
  TestInfoDlg: TTestInfoDlg;

implementation

{$R *.lfm}

{ TTestInfoDlg }

procedure TTestInfoDlg.Show(ATest: TProblemTest; AResults: TTestResult);
begin
  VerdictLabel.Font.Color := TestVerdictColors[AResults.Verdict];
  VerdictLabel.Caption := STestVerdicts[AResults.Verdict];
  ScoreLabel.Font.Color := GetTotalScoreColor(AResults.Score, ATest.Cost);
  ScoreLabel.Caption := Format(SScoreDivide, [AResults.Score, ATest.Cost]);
  TimeLabel.Caption := Format(STimeConsumedEx, [AResults.Time / 1000]);
  MemoryLabel.Caption := Format(SMemConsumedEx, [AResults.Memory / 1024]);
  CheckerOutputMemo.Text := AResults.CheckerOutput;
  ShowModal;
end;

end.
