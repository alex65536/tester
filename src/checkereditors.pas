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
unit checkereditors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, checkereditorbase, stdexecheckeredit, checkers, ExtCtrls, Controls,
  problemprops, textcheckedit, filecompcheckedit;

type

  { TFileCompareCheckerEditor }

  TFileCompareCheckerEditor = class(TCheckerEditor)
  private
    FControl: TFileCompareCheckerEdit;
  protected
    function GetControl: TControl; override;
    function GetChecker: TProblemChecker; override;
    procedure SetChecker(AValue: TProblemChecker); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TStdExecutableCheckerEditor }

  TStdExecutableCheckerEditor = class(TCheckerEditor)
  protected
    FControl: TStdExecuteCheckerEdit;
    function GetControl: TControl; override;
    function GetChecker: TProblemChecker; override;
    procedure SetChecker(AValue: TProblemChecker); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TTextCheckerEditor }

  TTextCheckerEditor = class(TStdExecutableCheckerEditor)
  protected
    function GetChecker: TProblemChecker; override;
  public
    constructor Create; override;
  end;

  { TTestlibCheckerEditor }

  TTestlibCheckerEditor = class(TStdExecutableCheckerEditor)
  protected
    function GetChecker: TProblemChecker; override;
  end;

implementation

{ TTestlibCheckerEditor }

function TTestlibCheckerEditor.GetChecker: TProblemChecker;
begin
  Result := FControl.GetChecker(TTestlibChecker);
end;

{ TTextCheckerEditor }

function TTextCheckerEditor.GetChecker: TProblemChecker;
begin
  Result := FControl.GetChecker(TTextChecker);
end;

constructor TTextCheckerEditor.Create;
begin
  FControl := TTextCheckerEdit.Create(nil);
end;

{ TStdExecutableCheckerEditor }

function TStdExecutableCheckerEditor.GetControl: TControl;
begin
  Result := FControl;
end;

function TStdExecutableCheckerEditor.GetChecker: TProblemChecker;
begin
  Result := FControl.GetChecker(TStdExecutableChecker);
end;

procedure TStdExecutableCheckerEditor.SetChecker(AValue: TProblemChecker);
begin
  FControl.SetChecker(AValue);
end;

constructor TStdExecutableCheckerEditor.Create;
begin
  inherited Create;
  FControl := TStdExecuteCheckerEdit.Create(nil);
end;

destructor TStdExecutableCheckerEditor.Destroy;
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
  Result := FControl.GetChecker;
end;

procedure TFileCompareCheckerEditor.SetChecker(AValue: TProblemChecker);
begin
  FControl.SetChecker(AValue as TFileCompareChecker);
end;

constructor TFileCompareCheckerEditor.Create;
begin
  inherited Create;
  FControl := TFileCompareCheckerEdit.Create(nil);
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
