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
unit licenseforms;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, ButtonPanel, StdCtrls, Graphics, math, LCLIntf, baseforms;

type

  { TMemo }

  TMemo = class(StdCtrls.TMemo)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
  end;

  { TLicenseForm }

  TLicenseForm = class(TBaseForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Memo1: TMemo;
  end;

var
  LicenseForm: TLicenseForm;

implementation

{$R *.lfm}

{ TMemo }

procedure TMemo.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: boolean);
var
  ACanvas: TCanvas;
  I: integer;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := GetDC(Handle);
    ACanvas.Font.Assign(Font);
    try
      for I := 0 to Lines.Count - 1 do
        PreferredWidth := Max(PreferredWidth, ACanvas.TextWidth(Lines[I]) + 32);
      PreferredHeight := ACanvas.TextHeight('42') * 25 + 32;
    finally
      ReleaseDC(Handle, ACanvas.Handle);
    end;
  finally
    FreeAndNil(ACanvas);
  end;
end;

end.

