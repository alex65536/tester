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
  Classes, SysUtils, baseforms, StdCtrls, ButtonPanel, ExtCtrls;

type

  { TSolutionStatsDlg }

  TSolutionStatsDlg = class(TBaseForm)
    ScoreLabel: TLabel;
    TestsPassedLabel: TLabel;
    TestsFailedLabel: TLabel;
    TestsSkippedLabel: TLabel;
    TestsWaitingLabel: TLabel;
    BestTimeLabel: TLabel;
    WorstTimeLabel: TLabel;
    AverageTimeLabel: TLabel;
    BestMemoryLabel: TLabel;
    WorstMemoryLabel: TLabel;
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
  end;

var
  SolutionStatsDlg: TSolutionStatsDlg;

implementation

{$R *.lfm}

end.

