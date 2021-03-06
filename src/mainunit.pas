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
unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Forms, ComCtrls, ExtCtrls, ExtendedNotebook, Classes, propseditor, Controls,
  ActnList, Dialogs, Menus, Buttons, SysUtils, LazFileUtils, testerforms, about,
  parserforms, appinfo, versioninfo, baseforms, jsonsaver, strconsts,
  logfile;

type
  TCreateEditorPolicy = (ceLoad, ceSave);

  { TMainForm }

  TMainForm = class(TBaseForm)
    AboutAction: TAction;
    CloseOtherTabsAction: TAction;
    CloseAllTabsAction: TAction;
    EditCostsAction: TAction;
    ClearTestsAction: TAction;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MultiAddTestsBtn: TAction;
    EditTestAction: TAction;
    CloseTabAction: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    MainMenu: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    ExitMenuItem: TMenuItem;
    MenuItem9: TMenuItem;
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    DeleteTestAction: TAction;
    InsertTestAction: TAction;
    AddTestAction: TAction;
    MenuItem1: TMenuItem;
    TabPopupMenu: TPopupMenu;
    EmptyPopupMenu: TPopupMenu;
    TestAction: TAction;
    OpenDialog: TOpenDialog;
    OpenFileAction: TAction;
    SaveFileAction: TAction;
    NewFileAction: TAction;
    ActionList: TActionList;
    Panel: TPanel;
    PropsList: TExtendedNotebook;
    SaveDialog: TSaveDialog;
    AutoSaveTimer: TTimer;
    procedure AboutActionExecute(Sender: TObject);
    procedure AddTestActionExecute(Sender: TObject);
    procedure AddTestActionUpdate(Sender: TObject);
    procedure AutoSaveTimerTimer(Sender: TObject);
    procedure ClearTestsActionExecute(Sender: TObject);
    procedure ClearTestsActionUpdate(Sender: TObject);
    procedure CloseAllTabsActionExecute(Sender: TObject);
    procedure CloseAllTabsActionUpdate(Sender: TObject);
    procedure CloseOtherTabsActionExecute(Sender: TObject);
    procedure CloseOtherTabsActionUpdate(Sender: TObject);
    procedure CloseTabActionExecute(Sender: TObject);
    procedure CloseTabActionUpdate(Sender: TObject);
    procedure DeleteTestActionExecute(Sender: TObject);
    procedure DeleteTestActionUpdate(Sender: TObject);
    procedure EditCostsActionExecute(Sender: TObject);
    procedure EditCostsActionUpdate(Sender: TObject);
    procedure EditTestActionExecute(Sender: TObject);
    procedure EditTestActionUpdate(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InsertTestActionExecute(Sender: TObject);
    procedure InsertTestActionUpdate(Sender: TObject);
    procedure MoveDownActionExecute(Sender: TObject);
    procedure MoveDownActionUpdate(Sender: TObject);
    procedure MoveUpActionExecute(Sender: TObject);
    procedure MoveUpActionUpdate(Sender: TObject);
    procedure MultiAddTestsBtnExecute(Sender: TObject);
    procedure MultiAddTestsBtnUpdate(Sender: TObject);
    procedure NewFileActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure PropEditorTestsChange(Sender: TObject);
    procedure PropsListCloseTabClicked(Sender: TObject);
    procedure SaveFileActionExecute(Sender: TObject);
    procedure SaveFileActionUpdate(Sender: TObject);
    procedure TabPopupMenuPopup(Sender: TObject);
    procedure TabShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure TestActionExecute(Sender: TObject);
    procedure TestActionUpdate(Sender: TObject);
  private
    procedure DoUpdateActions;
  public
    function GetCurrentPage(const MousePos: TPoint): TTabSheet;
    function CurEditor: TProblemPropsEditor;
    function EditorFromTab(TabSheet: TTabSheet): TProblemPropsEditor;
    procedure CreateFromFile(const FileName: string; APolicy: TCreateEditorPolicy;
      Parse: boolean);
    procedure AfterConstruction; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.PropsListCloseTabClicked(Sender: TObject);
begin
  FreeAndNil(Sender);
  DoUpdateActions;
end;

procedure TMainForm.PropEditorTestsChange(Sender: TObject);
begin
  DoUpdateActions;
end;

function TMainForm.CurEditor: TProblemPropsEditor;
var
  TabSheet: TTabSheet;
begin
  TabSheet := PropsList.ActivePage;
  if TabSheet = nil then
    Result := nil
  else
    Result := EditorFromTab(TabSheet);
end;

function TMainForm.EditorFromTab(TabSheet: TTabSheet): TProblemPropsEditor;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to TabSheet.ComponentCount - 1 do
    if TabSheet.Components[I] is TProblemPropsEditor then
    begin
      Result := TabSheet.Components[I] as TProblemPropsEditor;
      Exit;
    end;
end;

procedure TMainForm.CreateFromFile(const FileName: string;
  APolicy: TCreateEditorPolicy; Parse: boolean);
var
  TabSheet: TTabSheet;
  PropEditor: TProblemPropsEditor;
  KeepLoading: boolean;
  FileVersion: TFileVersion;
  CurVersion: TFileVersion;
  MsgText: string;
  MsgFmt: string;

  function CheckFileVersion: boolean;
  var
    MsgText: string;
  begin
    Result := True;
    FileVersion := GetPropsFileVersion(FileName);
    try
      CurVersion := TFileVersion.Current;
      try
        if CompareFileVersions(FileVersion, CurVersion) > 0 then
        begin
          MsgText := Format(STooNewVersion, [FileName, FileVersion.ToString, CurVersion.ToString]);
          Result := MessageDlg(MsgText, mtWarning, mbYesNo, 0) = mrYes;
        end;
      finally
        FreeAndNil(CurVersion);
      end;
    finally
      FreeAndNil(FileVersion);
    end;
  end;

begin
  try
    KeepLoading := True;
    if APolicy = ceLoad then
      KeepLoading := CheckFileVersion;
    if not KeepLoading then
      Exit;
    // if everything is ok - continue loading
    TabSheet := PropsList.AddTabSheet;
    PropsList.ActivePage := TabSheet;
    PropEditor := TProblemPropsEditor.Create(TabSheet);
    PropEditor.OnTestsChange := @PropEditorTestsChange;
    try
      TabSheet.Caption := ExtractFileName(FileName);
      PropEditor.PopupMenu := EmptyPopupMenu;
      PropEditor.FileName := ExpandFileNameUTF8(FileName);
      if Parse then
      begin
        RunAllParsers(ExtractFilePath(FileName), PropEditor.Properties);
        PropEditor.UpdateControls;
      end;
      case APolicy of
        ceLoad: PropEditor.LoadFromJSON;
        ceSave: PropEditor.SaveToJSON;
      end;
      PropEditor.ValidateAndCorrect;
      PropEditor.Parent := TabSheet;
      PropEditor.Align := alClient;
    except
      on E: Exception do
      begin
        WriteLog('Exception while loading/saving: ' + E.ClassName + ' ' + E.Message);
        PropEditor.FileName := '';
        FreeAndNil(TabSheet);
        raise;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLog('Exception while loading/saving: ' + E.ClassName + ' ' + E.Message);
      if APolicy = ceLoad then
        MsgFmt := SUnableToLoad
      else
        MsgFmt := SUnableToSave;
      MsgText := Format(MsgFmt, [FileName]);
      MessageDlg(MsgText, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  DoUpdateActions;
end;

procedure TMainForm.SaveFileActionExecute(Sender: TObject);
var
  AEditor: TProblemPropsEditor;
begin
  if not SaveDialog.Execute then
    Exit;
  AEditor := CurEditor;
  AEditor.FileName := ExpandFileNameUTF8(SaveDialog.FileName);
  AEditor.Parent.Caption := ExtractFileName(SaveDialog.FileName);
  AEditor.SaveToJSON;
  AEditor.ValidateAndCorrect;
end;

procedure TMainForm.SaveFileActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := PropsList.ActivePage <> nil;
end;

procedure TMainForm.TabPopupMenuPopup(Sender: TObject);
var
  ACurPage: TTabSheet;
begin
  if not (TabPopupMenu.PopupComponent = PropsList) then
    Exit;
  ACurPage := GetCurrentPage(TabPopupMenu.PopupPoint);
  if ACurPage <> nil then
    PropsList.ActivePage := ACurPage;
end;

procedure TMainForm.TabShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  ACurTab: TTabSheet;
begin
  ACurTab := GetCurrentPage(Mouse.CursorPos);
  if (ACurTab = nil) or (HintInfo = nil) then
    Exit;
  HintInfo^.HintStr := EditorFromTab(ACurTab).FileName;
end;

procedure TMainForm.TestActionExecute(Sender: TObject);
var
  AEditor: TProblemPropsEditor;
  WasDir: string;
begin
  AEditor := CurEditor;
  AEditor.SaveToJSON;
  WasDir := GetCurrentDirUTF8;
  try
    SetCurrentDirUTF8(ExtractFilePath(AEditor.FileName));
    TesterForm.Properties.Assign(AEditor.Properties);
    TesterForm.ShowModal;
  finally
    SetCurrentDirUTF8(WasDir);
  end;
end;

procedure TMainForm.TestActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := PropsList.ActivePage <> nil;
end;

procedure TMainForm.DoUpdateActions;
var
  I: integer;
begin
  with ActionList do
  begin
    for I := 0 to ActionList.ActionCount - 1 do
      ActionList.Actions[I].Update;
  end;
end;

function TMainForm.GetCurrentPage(const MousePos: TPoint): TTabSheet;
var
  TabIndex: integer;
begin
  Result := nil;
  if PropsList.PageCount = 0 then
    Exit;
  TabIndex := PropsList.IndexOfPageAt(PropsList.ScreenToClient(MousePos));
  if TabIndex < 0 then
    Result := nil
  else
    Result := PropsList.Pages[TabIndex];
end;

procedure TMainForm.NewFileActionExecute(Sender: TObject);
begin
  if not SaveDialog.Execute then
    Exit;
  CreateFromFile(SaveDialog.FileName, ceSave, True);
  DoUpdateActions;
end;

procedure TMainForm.AutoSaveTimerTimer(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to PropsList.PageCount - 1 do
    EditorFromTab(PropsList.Pages[I]).SaveToJSON;
end;

procedure TMainForm.ClearTestsActionExecute(Sender: TObject);
begin
  CurEditor.ClearTestsBtn.Click;
end;

procedure TMainForm.ClearTestsActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.ClearTestsBtn.Enabled;
end;

procedure TMainForm.CloseAllTabsActionExecute(Sender: TObject);
begin
  while PropsList.PageCount <> 0 do
  begin
    PropsList.Pages[0].Free;
    Refresh;
  end;
  DoUpdateActions;
end;

procedure TMainForm.CloseAllTabsActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := PropsList.PageCount <> 0;
end;

procedure TMainForm.CloseOtherTabsActionExecute(Sender: TObject);
var
  CurPage: TTabSheet;
begin
  CurPage := PropsList.ActivePage;
  while PropsList.PageCount > 1 do
  begin
    if PropsList.Pages[0] = CurPage then
      PropsList.Pages[1].Free
    else
      PropsList.Pages[0].Free;
    Refresh;
  end;
  PropsList.PageIndex := 0;
  DoUpdateActions;
end;

procedure TMainForm.CloseOtherTabsActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (PropsList.ActivePage <> nil) and
    (PropsList.PageCount > 1);
end;

procedure TMainForm.CloseTabActionExecute(Sender: TObject);
begin
  PropsList.ActivePage.Free;
  DoUpdateActions;
end;

procedure TMainForm.CloseTabActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := PropsList.ActivePage <> nil;
end;

procedure TMainForm.DeleteTestActionExecute(Sender: TObject);
begin
  CurEditor.DeleteTestBtn.Click;
end;

procedure TMainForm.DeleteTestActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.DeleteTestBtn.Enabled;
end;

procedure TMainForm.EditCostsActionExecute(Sender: TObject);
begin
  CurEditor.EditCostsBtn.Click;
end;

procedure TMainForm.EditCostsActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.EditCostsBtn.Enabled;
end;

procedure TMainForm.EditTestActionExecute(Sender: TObject);
begin
  CurEditor.EditTestBtn.Click;
end;

procedure TMainForm.EditTestActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.EditTestBtn.Enabled;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := GetAppFullName;
  PropsList.OnShowHint := @TabShowHint;
  PropsList.ShowHint := True;
end;

procedure TMainForm.InsertTestActionExecute(Sender: TObject);
begin
  CurEditor.InsertTestBtn.Click;
end;

procedure TMainForm.InsertTestActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.InsertTestBtn.Enabled;
end;

procedure TMainForm.MoveDownActionExecute(Sender: TObject);
begin
  CurEditor.MoveDownBtn.Click;
end;

procedure TMainForm.MoveDownActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.MoveDownBtn.Enabled;
end;

procedure TMainForm.MoveUpActionExecute(Sender: TObject);
begin
  CurEditor.MoveUpBtn.Click;
end;

procedure TMainForm.MoveUpActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.MoveUpBtn.Enabled;
end;

procedure TMainForm.MultiAddTestsBtnExecute(Sender: TObject);
begin
  CurEditor.MultiAddTestsBtn.Click;
end;

procedure TMainForm.MultiAddTestsBtnUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.MultiAddTestsBtn.Enabled;
end;

procedure TMainForm.AboutActionExecute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.AddTestActionExecute(Sender: TObject);
begin
  CurEditor.AddTestBtn.Click;
end;

procedure TMainForm.AddTestActionUpdate(Sender: TObject);
var
  Editor: TProblemPropsEditor;
begin
  Editor := CurEditor;
  (Sender as TAction).Enabled := (Editor <> nil) and Editor.AddTestBtn.Enabled;
end;

procedure TMainForm.OpenFileActionExecute(Sender: TObject);
var
  I: integer;
begin
  if not OpenDialog.Execute then
    Exit;
  for I := 0 to OpenDialog.Files.Count - 1 do
    CreateFromFile(OpenDialog.Files[I], ceLoad, False);
  DoUpdateActions;
end;

end.
