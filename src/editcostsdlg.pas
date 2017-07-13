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
unit editcostsdlg;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, Spin, StdCtrls, Controls, problemprops;

type

  { TEditCostsDialog }

  TEditCostsDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    TestsCostEdit: TFloatSpinEdit;
    ResizePolicyCombo: TComboBox;
  public
    procedure Execute(AProps: TProblemProperties);
  end;

var
  EditCostsDialog: TEditCostsDialog;

implementation

{$R *.lfm}

{ TEditCostsDialog }

procedure TEditCostsDialog.Execute(AProps: TProblemProperties);
var
  APolicy: TTestCostEditPolicy;
begin
  TestsCostEdit.Value := AProps.MaxScore;
  ResizePolicyCombo.ItemIndex := 0;
  if ShowModal <> mrOK then
    Exit;
  case ResizePolicyCombo.ItemIndex of
    0: APolicy := tcepProportionally;
    1: APolicy := tcepMakeEqual;
    2: APolicy := tcepAllCostToLast;
  end;
  AProps.RescaleCosts(TestsCostEdit.Value, APolicy);
end;

end.

