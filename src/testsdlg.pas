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
unit testsdlg;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ButtonPanel, Spin, problemprops;

type

  { TTestDialog }

  TTestDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    OutputFileEdit: TEdit;
    Label3: TLabel;
    TestCostEdit: TFloatSpinEdit;
    Label2: TLabel;
    InputFileEdit: TEdit;
    Label1: TLabel;
  public
    function GetTest: TProblemTest;
    procedure PutTest(ATest: TProblemTest);
    function ShowModal: integer; override;
    function ShowModal(ACaption: string): Integer;
  end;

var
  TestDialog: TTestDialog;

implementation

{$R *.lfm}

{ TTestDialog }

function TTestDialog.ShowModal: integer;
begin
  Result := inherited ShowModal;
end;

function TTestDialog.GetTest: TProblemTest;
begin
  Result := TProblemTest.Create(InputFileEdit.Text, OutputFileEdit.Text,
    TestCostEdit.Value);
end;

procedure TTestDialog.PutTest(ATest: TProblemTest);
begin
  InputFileEdit.Text := ATest.InputFile;
  OutputFileEdit.Text := ATest.OutputFile;
  TestCostEdit.Value := ATest.Cost;
end;

function TTestDialog.ShowModal(ACaption: string): Integer;
begin
  Caption := ACaption;
  Result := inherited ShowModal;
end;

end.
