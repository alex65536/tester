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
unit checkerselector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, problemprops,
  checkereditorbase, checkers;

const
  CheckerTypesCount = 3;
  CheckerTypes: array [0 .. CheckerTypesCount - 1] of TProblemCheckerClass =
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
    FEditors: array [0 .. CheckerTypesCount - 1] of TCheckerEditor;
    procedure CreateEditor(I: integer);
    procedure UpdateEditor;
    function GetChecker: TProblemChecker;
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
  UpdateEditor;
end;

function TCheckerSelect.GetChecker: TProblemChecker;
var
  AEditor: TCheckerEditor;
begin
  if CheckerCombo.ItemIndex < 0 then
    Result := nil
  else
  begin
    AEditor := FEditors[CheckerCombo.ItemIndex];
    if AEditor = nil then
      Result := nil
    else
      Result := AEditor.Checker;
  end;
end;

procedure TCheckerSelect.CreateEditor(I: integer);
begin
  if FEditors[I] <> nil then
    Exit;
  FEditors[I] := GetEditor(CheckerTypes[I]).Create;
  with FEditors[I].Control do
  begin
    Parent := Self;
    Align := alClient;
    AutoSize := True;
    Visible := False;
  end;
end;

procedure TCheckerSelect.UpdateEditor;
var
  I: integer;
begin
  if CheckerCombo.ItemIndex >= 0 then
    CreateEditor(CheckerCombo.ItemIndex);
  for I := 0 to CheckerTypesCount - 1 do
    if FEditors[I] <> nil then
      FEditors[I].Control.Visible := CheckerCombo.ItemIndex = I;
end;

procedure TCheckerSelect.SetChecker(AValue: TProblemChecker);
var
  I: integer;
  Ind: integer;
begin
  Ind := -1;
  for I := 0 to CheckerTypesCount - 1 do
    if (AValue <> nil) and (AValue.ClassType = CheckerTypes[I]) then
      Ind := I;
  Inc(FComboLock);
  CheckerCombo.ItemIndex := Ind;
  Dec(FComboLock);
  UpdateEditor;
  if Ind >= 0 then
    FEditors[Ind].Checker := AValue;
end;

constructor TCheckerSelect.Create(TheOwner: TComponent);
var
  I: integer;
begin
  inherited Create(TheOwner);
  for I := 0 to CheckerTypesCount - 1 do
    FEditors[I] := nil;
  FComboLock := 0;
end;

destructor TCheckerSelect.Destroy;
var
  I: integer;
begin
  for I := 0 to CheckerTypesCount - 1 do
    FreeAndNil(FEditors[I]);
  inherited Destroy;
end;

procedure TCheckerSelect.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateEditor;
end;

end.

