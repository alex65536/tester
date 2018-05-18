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
unit baseforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TBaseForm }

  TBaseForm = class(TForm)
  private
    FInitialHeight: integer;
    FInitialWidth: integer;
  protected
    procedure DoShow; override;
  public
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy; const AFromPPI,
      AToPPI, AOldFormWidth, ANewFormWidth: Integer); override;
    procedure AfterConstruction; override;
  end;

  { TBaseFrame }

  TBaseFrame = class(TFrame)
  public
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy; const AFromPPI,
      AToPPI, AOldFormWidth, ANewFormWidth: Integer); override;
  end;

  TFixFontsAction = (faSaveZeroHeight, faLoadZeroHeight);

var
  BaseForm: TBaseForm;

procedure FixFonts(AControl: TControl; AAction: TFixFontsAction);

implementation

// Storing if the font doesn't need scaling in Font.Quality is not a good option,
// but we have no other place to store it (no tags for fonts)
procedure FixFonts(AControl: TControl; AAction: TFixFontsAction);
var
  WinCtrl: TWinControl;
  I: integer;
begin
  if AControl is TWinControl then
  begin
    WinCtrl := AControl as TWinControl;
    for I := 0 to WinCtrl.ComponentCount-1 do
      if WinCtrl.Components[I] is TControl then
        FixFonts(WinCtrl.Components[I] as TControl, AAction);
  end;
  case AAction of
    faSaveZeroHeight:
      begin
        if AControl.Font.Size = 0 then
          AControl.Font.Quality := fqCleartypeNatural
        else
          AControl.Font.Quality := fqCleartype;
      end;
    faLoadZeroHeight:
      if AControl.Font.Quality = fqCleartypeNatural then
      begin
        AControl.Font.Quality := fqCleartype;
        AControl.Font.Size := 0;
      end;
  end;
end;

{$R *.lfm}

{ TBaseFrame }

procedure TBaseFrame.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin
  DisableAutoSizing;
  try
    FixFonts(Self, faSaveZeroHeight);
    inherited AutoAdjustLayout(AMode, AFromPPI, AToPPI, AOldFormWidth,
      ANewFormWidth);
    FixFonts(Self, faLoadZeroHeight);
  finally
    EnableAutoSizing;
  end;
end;

{ TBaseForm }

procedure TBaseForm.DoShow;
var
  WasPos: TPosition;
begin
  if (BorderStyle = bsSizeable) and AutoSize then
  begin
    Constraints.MinHeight := Height;
    Constraints.MinWidth := Width;
    AutoSize := False;
    Height := FInitialHeight;
    Width := FInitialWidth;
    WasPos := Position;
    Position := poDesigned;
    Position := WasPos;
  end;
  inherited DoShow;
end;

procedure TBaseForm.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin
  DisableAutoSizing;
  try
    FixFonts(Self, faSaveZeroHeight);
    inherited AutoAdjustLayout(AMode, AFromPPI, AToPPI, AOldFormWidth,
      ANewFormWidth);
    FixFonts(Self, faLoadZeroHeight);
  finally
    EnableAutoSizing;
  end;
end;

procedure TBaseForm.AfterConstruction;
begin
  inherited AfterConstruction;
  if BorderStyle = bsSizeable then
  begin
    FInitialHeight := Height;
    FInitialWidth := Width;
    AutoSize := True;
  end;
end;

end.

