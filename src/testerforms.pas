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
unit testerforms;

{$mode objfpc}{$H+}{$coperators on}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  EditBtn, testerframes, problemprops, LazFileUtils, LCLType, ActnList, Buttons,
  Menus, jsonsaver, imgkeeper;

type

  { TFileNameEdit }

  TFileNameEdit = class(EditBtn.TFileNameEdit)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
  public
    procedure AfterConstruction; override;
  end;

  { TTesterForm }

  TTesterForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    FileNameEdit: TFileNameEdit;
    FileNameEditViewer: TEdit;
    FilePanel: TPanel;
    Label2: TLabel;
    MenuItem5: TMenuItem;
    OpenSourcesAction: TAction;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    StopTestAction: TAction;
    SaveDialog: TSaveDialog;
    SaveResultsAction: TAction;
    LaunchTestAction: TAction;
    ActionList: TActionList;
    Label1: TLabel;
    Panel: TPanel;
    ProgressPanel: TPanel;
    ProgressBar: TProgressBar;
    TestFrame: TTesterFrame;
    procedure FileNameEditChange(Sender: TObject);
    procedure FileNameEditViewerKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LaunchTestActionExecute(Sender: TObject);
    procedure LaunchTestActionUpdate(Sender: TObject);
    procedure OpenSourcesActionExecute(Sender: TObject);
    procedure OpenSourcesActionUpdate(Sender: TObject);
    procedure SaveResultsActionExecute(Sender: TObject);
    procedure SaveResultsActionUpdate(Sender: TObject);
    procedure StopTestActionExecute(Sender: TObject);
    procedure StopTestActionUpdate(Sender: TObject);
    procedure TestFrameStart(Sender: TObject);
    procedure TestFrameEnd(Sender: TObject);
  private
    FProperties: TProblemProperties;
    FEverTested: boolean;
  public
    property Properties: TProblemProperties read FProperties;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  TesterForm: TTesterForm;

implementation

{$R *.lfm}

{ TFileNameEdit }

procedure TFileNameEdit.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  PreferredWidth := Button.Width;
end;

procedure TFileNameEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  ImageKeeper.SmallImageList.GetBitmap(0, Button.Glyph);
end;

{ TTesterForm }

procedure TTesterForm.FileNameEditChange(Sender: TObject);
var
  I: integer;
  AText: string;
begin
  AText := '';
  with FileNameEdit.DialogFiles do
  begin
    for I := 0 to Count - 1 do
    begin
      if I <> 0 then
        AText += ';';
      AText += CreateRelativePath(ExpandFileNameUTF8(Strings[I]),
        ExpandFileNameUTF8(GetCurrentDirUTF8));
    end;
  end;
  FileNameEditViewer.Text := AText;
end;

procedure TTesterForm.FileNameEditViewerKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  Shift := Shift; // to prevent hints
  if Key = VK_RETURN then
    FileNameEdit.ButtonClick;
end;

procedure TTesterForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if TestFrame.IsTesting then
    TestFrame.TerminateTesting;
  CanClose := true;
end;

procedure TTesterForm.FormShow(Sender: TObject);
begin
  TestFrame.Unprepare;
  FEverTested := False;
end;

procedure TTesterForm.LaunchTestActionExecute(Sender: TObject);
begin
  FEverTested := True;
  TestFrame.Properties := FProperties;
  TestFrame.Sources.Assign(FileNameEdit.DialogFiles);
  TestFrame.ProgressBar := ProgressBar;
  TestFrame.OnTestingStart := @TestFrameStart;
  TestFrame.OnTestingEnd := @TestFrameEnd;
  TestFrame.LaunchTesting;
end;

procedure TTesterForm.LaunchTestActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (FileNameEdit.DialogFiles.Count <> 0) and (not TestFrame.IsTesting);
end;

procedure TTesterForm.OpenSourcesActionExecute(Sender: TObject);
begin
  FileNameEdit.ButtonClick;
end;

procedure TTesterForm.OpenSourcesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not TestFrame.IsTesting;
end;

procedure TTesterForm.SaveResultsActionExecute(Sender: TObject);
var
  MemStream: TMemoryStream;
  S: string;
begin
  if not SaveDialog.Execute then
    Exit;
  MemStream := TMemoryStream.Create;
  try
    S := SaveMultiTesterToJSONStr(TestFrame.MultiTester);
    MemStream.Write(S[1], Length(S));
    MemStream.SaveToFile(SaveDialog.FileName);
  finally
    FreeAndNil(MemStream);
  end;
end;

procedure TTesterForm.SaveResultsActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FEverTested and (not TestFrame.IsTesting);
end;

procedure TTesterForm.StopTestActionExecute(Sender: TObject);
begin
  TestFrame.TerminateTesting;
end;

procedure TTesterForm.StopTestActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TestFrame.IsTesting;
end;

procedure TTesterForm.TestFrameStart(Sender: TObject);
begin
  ProgressPanel.Visible := True;
  FilePanel.Enabled := False;
end;

procedure TTesterForm.TestFrameEnd(Sender: TObject);
begin
  ProgressPanel.Visible := False;
  FilePanel.Enabled := True;
end;

constructor TTesterForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FProperties := TProblemProperties.Create;
end;

destructor TTesterForm.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

end.
