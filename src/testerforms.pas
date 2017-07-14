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
  Menus, jsonsaver, imgkeeper, htmlexport, LCLIntf, baseforms;

type

  { TTesterForm }

  TTesterForm = class(TBaseForm)
    BitBtn5: TBitBtn;
    ExportHTMLAction: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    FileNameEditViewer: TEdit;
    FilePanel: TPanel;
    Label2: TLabel;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenSourcesAction: TAction;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    SaveHTMLDialog: TSaveDialog;
    FileChooseButton: TSpeedButton;
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
    procedure ExportHTMLActionExecute(Sender: TObject);
    procedure ExportHTMLActionUpdate(Sender: TObject);
    procedure FileChooseButtonClick(Sender: TObject);
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
    FAppliedFiles: TStringList;
    FProperties: TProblemProperties;
    FEverTested: boolean;
    procedure FilesChangedTrigger;
  public
    property Properties: TProblemProperties read FProperties;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  TesterForm: TTesterForm;

implementation

{$R *.lfm}

{ TTesterForm }

procedure TTesterForm.ExportHTMLActionExecute(Sender: TObject);
var
  AExporter: TMultiTesterHTMLExporter;
begin
  if not SaveHTMLDialog.Execute then Exit;
  AExporter := TMultiTesterHTMLExporter.Create;
  try
    AExporter.MultiTester := TestFrame.MultiTester;
    AExporter.ExportHTML;
    AExporter.Document.SaveToFile(SaveHTMLDialog.FileName);
    OpenDocument(SaveHTMLDialog.FileName);
  finally
    FreeAndNil(AExporter);
  end;
end;

procedure TTesterForm.ExportHTMLActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FEverTested and (not TestFrame.IsTesting);
end;

procedure TTesterForm.FileChooseButtonClick(Sender: TObject);
begin
  OpenDialog.Files.Assign(FAppliedFiles);
  if OpenDialog.Execute then
  begin
    FAppliedFiles.Assign(OpenDialog.Files);
    FilesChangedTrigger;
  end;
end;

procedure TTesterForm.FileNameEditViewerKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  Shift := Shift; // to prevent hints
  if Key = VK_RETURN then
    FileChooseButton.Click;
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
  TestFrame.Sources.Assign(FAppliedFiles);
  TestFrame.ProgressBar := ProgressBar;
  TestFrame.OnTestingStart := @TestFrameStart;
  TestFrame.OnTestingEnd := @TestFrameEnd;
  TestFrame.LaunchTesting;
end;

procedure TTesterForm.LaunchTestActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (FAppliedFiles.Count <> 0) and (not TestFrame.IsTesting);
end;

procedure TTesterForm.OpenSourcesActionExecute(Sender: TObject);
begin
  FileChooseButton.Click;
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

procedure TTesterForm.FilesChangedTrigger;
var
  I: integer;
  AText: string;
begin
  AText := '';
  with FAppliedFiles do
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

constructor TTesterForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FProperties := TProblemProperties.Create;
  FAppliedFiles := TStringList.Create;
  ImageKeeper.SmallImageList.GetBitmap(0, FileChooseButton.Glyph);
end;

destructor TTesterForm.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FAppliedFiles);
  inherited Destroy;
end;

end.
