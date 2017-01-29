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
unit checkerselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, problemprops,
  checkereditorbase, checkers;

const
  CheckerTypesCount = 3;
  CheckerTypes: array [0 .. 2] of TProblemCheckerClass =
    (TFileCompareChecker, TTextChecker, TTestlibChecker);

type

  { TCheckerSelect }

  TCheckerSelect = class(TFrame)
    CheckerCombo: TComboBox;
    Label5: TLabel;
    Panel: TPanel;
    procedure CheckerComboChange(Sender: TObject);
  private
    FComboLock: integer;
    FEditor: TCheckerEditor;
    function GetChecker: TProblemChecker;
    procedure RecreateEditor;
    procedure SetChecker(AValue: TProblemChecker);
  public
    property Checker: TProblemChecker read GetChecker write SetChecker;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

{ TCheckerSelect }

procedure TCheckerSelect.CheckerComboChange(Sender: TObject);
begin
  if FComboLock <> 0 then
    Exit;
  RecreateEditor;
end;

function TCheckerSelect.GetChecker: TProblemChecker;
begin
  if FEditor = nil then
    Result := nil
  else
    Result := FEditor.Checker;
end;

procedure TCheckerSelect.RecreateEditor;
begin
  FreeAndNil(FEditor);
  if CheckerCombo.ItemIndex >= 0 then
    FEditor := GetEditor(CheckerTypes[CheckerCombo.ItemIndex]).Create;
  if FEditor = nil then
    Exit;
  with FEditor.Control do
  begin
    FEditor.Control.Parent := Self;
    FEditor.Control.Align := alClient;
    FEditor.Control.AutoSize := True;
  end;
end;

procedure TCheckerSelect.SetChecker(AValue: TProblemChecker);
var
  I: integer;
begin
  Inc(FComboLock);
  CheckerCombo.ItemIndex := -1;
  for I := 0 to CheckerTypesCount - 1 do
    if (AValue <> nil) and (AValue.ClassType = CheckerTypes[I]) then
      CheckerCombo.ItemIndex := I;
  Dec(FComboLock);
  RecreateEditor;
  if FEditor <> nil then
    FEditor.Checker := AValue;
end;

constructor TCheckerSelect.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditor := nil;
  FComboLock := 0;
end;

destructor TCheckerSelect.Destroy;
begin
  FreeAndNil(FEditor);
  inherited Destroy;
end;

procedure TCheckerSelect.AfterConstruction;
begin
  inherited AfterConstruction;
  RecreateEditor;
end;

end.

