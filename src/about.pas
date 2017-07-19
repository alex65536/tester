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
  Classes, DividerBevel, Forms, ExtCtrls, StdCtrls, licenseforms, versioninfo,
  strconsts, SysUtils, LCLIntf, baseforms;

type

  { TAboutBox }

  TAboutBox = class(TBaseForm)
    LisenceBtn: TButton;
    FileVersionLbl: TLabel;
    WebsiteBtn: TButton;
    CloseBtn: TButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    Image: TImage;
    Label1: TLabel;
    VersionLbl: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    DateLbl: TLabel;
    PlatformLbl: TLabel;
    Panel: TPanel;
    Panel1: TPanel;
    procedure LisenceBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WebsiteBtnClick(Sender: TObject);
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

{ TAboutBox }

procedure TAboutBox.LisenceBtnClick(Sender: TObject);
begin
  LicenseForm.ShowModal;
end;

procedure TAboutBox.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  VersionLbl.Caption := Format(SVersionFmt, [GetAppVersion]);
  FileVersionLbl.Caption := GetFileVersion;
  PlatformLbl.Caption := GetAppTarget;
  DateLbl.Caption := Format(SBuildDateFmt, [GetAppBuildDate]);
end;

procedure TAboutBox.WebsiteBtnClick(Sender: TObject);
begin
  OpenURL('https://alex65536.github.io/tester');
end;

end.

