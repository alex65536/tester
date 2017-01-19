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
unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, DividerBevel, Forms, ExtCtrls, StdCtrls, licenseforms;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Button1: TButton;
    Button3: TButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel: TPanel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

{ TAboutBox }

procedure TAboutBox.Button1Click(Sender: TObject);
begin
  LicenseForm.ShowModal;
end;

procedure TAboutBox.Button3Click(Sender: TObject);
begin
  Close;
end;

end.

