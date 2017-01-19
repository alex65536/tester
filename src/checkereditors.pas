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
unit checkereditors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, checkereditorbase, stdexecheckeredit, checkers, ExtCtrls, Controls,
  problemprops;

type

  { TFileCompareCheckerEditor }

  TFileCompareCheckerEditor = class(TCheckerEditor)
  private
    FControl: TPanel;
  protected
    function GetControl: TControl; override;
    function GetChecker: TProblemChecker; override;
    procedure SetChecker(AValue: TProblemChecker); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TTextCheckerEditor }

  TTextCheckerEditor = class(TCheckerEditor)
  private
    FControl: TStdExecuteCheckerEdit;
  protected
    function GetControl: TControl; override;
    function GetChecker: TProblemChecker; override;
    procedure SetChecker(AValue: TProblemChecker); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TTestlibCheckerEditor }

  TTestlibCheckerEditor = class(TCheckerEditor)
  private
    FControl: TStdExecuteCheckerEdit;
  protected
    function GetControl: TControl; override;
    function GetChecker: TProblemChecker; override;
    procedure SetChecker(AValue: TProblemChecker); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


implementation

{ TTestlibCheckerEditor }

function TTestlibCheckerEditor.GetControl: TControl;
begin
  Result := FControl;
end;

function TTestlibCheckerEditor.GetChecker: TProblemChecker;
begin
  Result := FControl.GetChecker(TTestlibChecker);
end;

procedure TTestlibCheckerEditor.SetChecker(AValue: TProblemChecker);
begin
  FControl.SetChecker(AValue);
end;

constructor TTestlibCheckerEditor.Create;
begin
  inherited Create;
  FControl := TStdExecuteCheckerEdit.Create(nil);
end;

destructor TTestlibCheckerEditor.Destroy;
begin
  FreeAndNil(FControl);
  inherited Destroy;
end;

{ TTextCheckerEditor }

function TTextCheckerEditor.GetControl: TControl;
begin
  Result := FControl;
end;

function TTextCheckerEditor.GetChecker: TProblemChecker;
begin
  Result := FControl.GetChecker(TTextChecker);
end;

procedure TTextCheckerEditor.SetChecker(AValue: TProblemChecker);
begin
  FControl.SetChecker(AValue);
end;

constructor TTextCheckerEditor.Create;
begin
  inherited Create;
  FControl := TStdExecuteCheckerEdit.Create(nil);
end;

destructor TTextCheckerEditor.Destroy;
begin
  FreeAndNil(FControl);
  inherited Destroy;
end;

{ TFileCompareCheckerEditor }

function TFileCompareCheckerEditor.GetControl: TControl;
begin
  Result := FControl;
end;

function TFileCompareCheckerEditor.GetChecker: TProblemChecker;
begin
  Result := TFileCompareChecker.Create;
end;

procedure TFileCompareCheckerEditor.SetChecker(AValue: TProblemChecker);
begin
  AValue := AValue; // to prevent hints
end;

constructor TFileCompareCheckerEditor.Create;
begin
  inherited Create;
  FControl := TPanel.Create(nil);
  FControl.BevelInner := bvNone;
  FControl.BevelOuter := bvNone;
end;

destructor TFileCompareCheckerEditor.Destroy;
begin
  FreeAndNil(FControl);
  inherited Destroy;
end;

initialization
  SetEditor(TFileCompareChecker, TFileCompareCheckerEditor);
  SetEditor(TTextChecker, TTextCheckerEditor);
  SetEditor(TTestlibChecker, TTestlibCheckerEditor);

end.
