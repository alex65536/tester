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
unit compilerinfo;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, ExtCtrls, StdCtrls, verdictcolors, testresults, strconsts,
  Classes, baseforms, SysUtils;

type

  { TCompilerInfoDlg }

  TCompilerInfoDlg = class(TBaseForm)
    ButtonPanel: TButtonPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    VerdictLabel: TLabel;
    CompilerOutputMemo: TMemo;
    Panel1: TPanel;
  private
    FResults: TTestedProblem;
    procedure AssignData;
  public
    procedure Show(AResults: TTestedProblem);
  end;

var
  CompilerInfoDlg: TCompilerInfoDlg;

procedure ShowCompilerInfo(AResults: TTestedProblem);

implementation

procedure ShowCompilerInfo(AResults: TTestedProblem);
var
  AForm: TCompilerInfoDlg;
begin
  AForm := TCompilerInfoDlg.Create(nil);
  try
    AForm.Show(AResults);
  finally
    FreeAndNil(AForm);
  end;
end;

{$R *.lfm}

{ TCompilerInfoDlg }

procedure TCompilerInfoDlg.AssignData;
begin
  VerdictLabel.Font.Color := CompilerVerdictColors[FResults.CompileVerdict];
  VerdictLabel.Caption := SCompilerVerdicts[FResults.CompileVerdict];
  CompilerOutputMemo.Text := FResults.CompilerOutput;
end;

procedure TCompilerInfoDlg.Show(AResults: TTestedProblem);
begin
  FResults := AResults;
  AssignData;
  ShowModal;
end;

end.

