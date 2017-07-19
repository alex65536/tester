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
unit propseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ExtCtrls, Buttons,
  problemprops, testsdlg, checkerselector, jsonsaver, imgkeeper,
  testtemplates, testtemplatedlg, strconsts, editcostsdlg, LazFileUtils,
  Dialogs, Math;

type
  EUnknownChecker = class(Exception);

  { TProblemPropsEditor }

  TProblemPropsEditor = class(TFrame)
    AddTestBtn: TBitBtn;
    ButtonPanel: TPanel;
    StopAfterFirstFailCheck: TCheckBox;
    CheckerCombo: TComboBox;
    CheckSelect: TCheckerSelect;
    ClearTestsBtn: TBitBtn;
    DeleteTestBtn: TBitBtn;
    EditTestBtn: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    InputFileEdit: TEdit;
    InsertTestBtn: TBitBtn;
    Label1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MemoryLimitEdit: TSpinEdit;
    EditCostsBtn: TBitBtn;
    MoveDownBtn: TBitBtn;
    MoveUpBtn: TBitBtn;
    MultiAddTestsBtn: TBitBtn;
    OutputFileEdit: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Tests: TGroupBox;
    TestsList: TListBox;
    TimeLimitEdit: TSpinEdit;
    procedure AddTestBtnClick(Sender: TObject);
    procedure ClearTestsBtnClick(Sender: TObject);
    procedure DeleteTestBtnClick(Sender: TObject);
    procedure EditCostsBtnClick(Sender: TObject);
    procedure EditTestBtnClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure InsertTestBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MultiAddTestsBtnClick(Sender: TObject);
    procedure TestsListDblClick(Sender: TObject);
    procedure TestsListSelectionChange(Sender: TObject; User: boolean);
  private
    FFileName: string;
    FOnTestsChange: TNotifyEvent;
    FProperties: TProblemProperties;
    function GetProperties: TProblemProperties;
    function GetChecker: TProblemChecker;
    procedure PutChecker(AChecker: TProblemChecker);
    procedure SetFileName(AValue: string);
    procedure SetOnTestsChange(AValue: TNotifyEvent);
    function TestToStr(ATest: TProblemTest): string;
    procedure AddTestProc(ATest: TProblemTest; out Stop: boolean);
    procedure AddInsertTestHelper(Place: integer);
  protected
    procedure DoTestChange; virtual;
  public
    property OnTestsChange: TNotifyEvent read FOnTestsChange write SetOnTestsChange;
    property Properties: TProblemProperties read GetProperties;
    property FileName: string read FFileName write SetFileName;
    procedure LoadFromJSON;
    procedure SaveToJSON;
    procedure ValidateAndCorrect;
    procedure UpdateProperties;
    procedure UpdateControls;
    procedure UpdateEnabled;
    procedure RefreshTests;
    procedure RecalcWidths;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TProblemPropsEditor }

procedure TProblemPropsEditor.TestsListDblClick(Sender: TObject);
var
  Place: integer;
  ATest: TProblemTest;
begin
  Place := TestsList.ItemIndex;
  if Place < 0 then
    Exit;
  TestDialog.PutTest(FProperties.Tests[Place]);
  if TestDialog.ShowModal(SEditTest) = mrOk then
  begin
    ATest := TestDialog.GetTest;
    try
      FProperties.Tests[Place] := ATest;
    finally
      FreeAndNil(ATest);
      RefreshTests;
      UpdateEnabled;
      DoTestChange;
    end;
  end;
end;

procedure TProblemPropsEditor.TestsListSelectionChange(Sender: TObject; User: boolean);
begin
  User := User; // to prevent hints
  UpdateEnabled;
  DoTestChange;
end;

procedure TProblemPropsEditor.AddTestBtnClick(Sender: TObject);
begin
  AddInsertTestHelper(-1);
end;

procedure TProblemPropsEditor.InsertTestBtnClick(Sender: TObject);
var
  Place: integer;
begin
  Place := TestsList.ItemIndex;
  if Place < 0 then
    Exit;
  AddInsertTestHelper(Place);
end;

procedure TProblemPropsEditor.AddInsertTestHelper(Place: integer);
var
  PutPlace: integer;
  Test: TProblemTest;
  WasDir: string;
begin
  PutPlace := Place;
  if Place < 0 then
    PutPlace := FProperties.TestCount;
  if TestDialog.ShowModal(SInsertTest) = mrOk then
  begin
    Test := TestDialog.GetTest;
    // correct test file names
    WasDir := GetCurrentDirUTF8;
    try
      SetCurrentDirUTF8(ExtractFilePath(FileName));
      Test.CorrectFileNames;
      if not Test.IsFileNamesValid then
        if MessageDlg(STestFilesDontExist, mtWarning, [mbYes, mbNo], 0) = mrNo then
          FreeAndNil(Test);
    finally
      SetCurrentDirUTF8(WasDir);
    end;
    // if test is valid and exists - insert it
    if Test <> nil then
    begin
      FProperties.InsertTest(PutPlace, Test);
      RefreshTests;
      if Place < 0 then
        TestsList.ItemIndex := TestsList.Count - 1;
      UpdateEnabled;
      DoTestChange;
    end;
  end;
end;

procedure TProblemPropsEditor.MoveDownBtnClick(Sender: TObject);
var
  Place: integer;
begin
  Place := TestsList.ItemIndex;
  if (Place < 0) or (Place = TestsList.Count - 1) then
    Exit;
  FProperties.TestList.Exchange(Place, Place + 1);
  TestsList.ItemIndex := Place + 1;
  RefreshTests;
  UpdateEnabled;
  DoTestChange;
end;

procedure TProblemPropsEditor.MoveUpBtnClick(Sender: TObject);
var
  Place: integer;
begin
  Place := TestsList.ItemIndex;
  if Place < 1 then
    Exit;
  FProperties.TestList.Exchange(Place, Place - 1);
  TestsList.ItemIndex := Place - 1;
  RefreshTests;
  UpdateEnabled;
  DoTestChange;
end;

procedure TProblemPropsEditor.MultiAddTestsBtnClick(Sender: TObject);
var
  Template: TProblemTestTemplate;
begin
  if TestTemplateDialog.ShowModal(SMultiAddTest) = mrOk then
  begin
    Template := TestTemplateDialog.GetTemplate;
    try
      Template.GenerateTests(ExtractFileDir(FileName), @AddTestProc);
    finally
      FreeAndNil(Template);
      RefreshTests;
      TestsList.ItemIndex := TestsList.Count - 1;
      UpdateEnabled;
      DoTestChange;
    end;
  end;
end;

procedure TProblemPropsEditor.DeleteTestBtnClick(Sender: TObject);
var
  Place: integer;
begin
  Place := TestsList.ItemIndex;
  if Place < 0 then
    Exit;
  FProperties.DeleteTest(Place);
  RefreshTests;
  TestsList.ItemIndex := Place - 1;
  UpdateEnabled;
  DoTestChange;
end;

procedure TProblemPropsEditor.EditCostsBtnClick(Sender: TObject);
begin
  EditCostsDialog.Execute(FProperties);
  UpdateControls;
end;

procedure TProblemPropsEditor.EditTestBtnClick(Sender: TObject);
begin
  TestsListDblClick(TestsList);
end;

procedure TProblemPropsEditor.FrameResize(Sender: TObject);
begin
  RecalcWidths;
end;

procedure TProblemPropsEditor.ClearTestsBtnClick(Sender: TObject);
begin
  FProperties.TestList.Clear;
  RefreshTests;
  UpdateEnabled;
  DoTestChange;
end;

function TProblemPropsEditor.GetProperties: TProblemProperties;
begin
  UpdateProperties;
  Result := FProperties;
end;

function TProblemPropsEditor.GetChecker: TProblemChecker;
begin
  Result := CheckSelect.Checker;
end;

procedure TProblemPropsEditor.PutChecker(AChecker: TProblemChecker);
begin
  CheckSelect.Checker := AChecker;
end;

procedure TProblemPropsEditor.SetFileName(AValue: string);
begin
  if FFileName = AValue then
    Exit;
  FFileName := AValue;
end;

procedure TProblemPropsEditor.SetOnTestsChange(AValue: TNotifyEvent);
begin
  if FOnTestsChange = AValue then
    Exit;
  FOnTestsChange := AValue;
end;

function TProblemPropsEditor.TestToStr(ATest: TProblemTest): string;
begin
  Result := Format('%d. "%s" "%s" (%.3f)', [ATest.Index + 1, ATest.InputFile,
    ATest.OutputFile, ATest.Cost]);
end;

procedure TProblemPropsEditor.AddTestProc(ATest: TProblemTest; out Stop: boolean);
begin
  FProperties.AddTest(ATest);
  Stop := False;
end;

procedure TProblemPropsEditor.DoTestChange;
begin
  if Assigned(FOnTestsChange) then
    FOnTestsChange(Self);
end;

procedure TProblemPropsEditor.LoadFromJSON;
var
  MemStream: TMemoryStream;
  S: string;
begin
  if FileName = '' then
    Exit;
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromFile(FileName);
    SetLength(S, MemStream.Size);
    MemStream.Read(S[1], Length(S));
    LoadFromJSONStr(S, FProperties, nil);
  finally
    FreeAndNil(MemStream);
    UpdateControls;
  end;
end;

procedure TProblemPropsEditor.SaveToJSON;
var
  MemStream: TMemoryStream;
  S: string;
begin
  if FileName = '' then
    Exit;
  try
    UpdateProperties;
    MemStream := TMemoryStream.Create;
    try
      S := SavePropsToJSONStr(FProperties);
      MemStream.Write(S[1], Length(S));
      MemStream.SaveToFile(FileName);
    finally
      FreeAndNil(MemStream);
    end;
  except
    on E: Exception do
      MessageDlg(Format(SSavePropsError, [FileName]), mtError, [mbOK], 0);
  end;
end;

procedure TProblemPropsEditor.ValidateAndCorrect;
const
  MaxLostViewed = 16;
var
  WasDir: string;

  procedure ValidateFilesFound;
  var
    LostFiles: TStringList;
    DlgText: string;
    I: integer;
  begin
    LostFiles := TStringList.Create;
    try
      FProperties.InvalidFilesList(LostFiles);
      if LostFiles.Count > 0 then
      begin
        if LostFiles.Count > MaxLostViewed then
        begin
          while LostFiles.Count >= MaxLostViewed do
            LostFiles.Delete(LostFiles.Count - 1);
          LostFiles.Add('...');
        end;
        DlgText := Format(SNotFoundWarningFmt, [SNotFoundWarning, SCheckRecommendation]);
        DlgText := DlgText + LineEnding + SNotFoundFilesListBegin;
        for I := 0 to LostFiles.Count - 1 do
          DlgText := DlgText + LineEnding + Format(SNotFoundFileFormat, [LostFiles[I]]);
        MessageDlg(DlgText, mtWarning, [mbOK], 0);
      end;
    finally
      FreeAndNil(LostFiles);
    end;
  end;

begin
  UpdateProperties;
  WasDir := GetCurrentDirUTF8;
  try
    SetCurrentDirUTF8(ExtractFilePath(FileName));
    FProperties.CorrectFileNames;
    ValidateFilesFound;
  finally
    SetCurrentDirUTF8(WasDir);
    UpdateControls;
  end;
end;

procedure TProblemPropsEditor.UpdateProperties;
begin
  FProperties.InputFile := InputFileEdit.Text;
  FProperties.OutputFile := OutputFileEdit.Text;
  FProperties.TimeLimit := TimeLimitEdit.Value;
  FProperties.MemoryLimit := MemoryLimitEdit.Value;
  FProperties.StopAfterFirstFail := StopAfterFirstFailCheck.Checked;
  FProperties.Checker := GetChecker;
end;

procedure TProblemPropsEditor.UpdateControls;
begin
  InputFileEdit.Text := FProperties.InputFile;
  OutputFileEdit.Text := FProperties.OutputFile;
  TimeLimitEdit.Value := FProperties.TimeLimit;
  MemoryLimitEdit.Value := FProperties.MemoryLimit;
  StopAfterFirstFailCheck.Checked := FProperties.StopAfterFirstFail;
  PutChecker(FProperties.Checker);
  RefreshTests;
  UpdateEnabled;
end;

procedure TProblemPropsEditor.UpdateEnabled;
begin
  AddTestBtn.Enabled := True;
  InsertTestBtn.Enabled := TestsList.ItemIndex >= 0;
  MultiAddTestsBtn.Enabled := True;
  EditTestBtn.Enabled := TestsList.ItemIndex >= 0;
  EditCostsBtn.Enabled := TestsList.Count <> 0;
  DeleteTestBtn.Enabled := TestsList.ItemIndex >= 0;
  ClearTestsBtn.Enabled := TestsList.Count <> 0;
  MoveUpBtn.Enabled := TestsList.ItemIndex >= 1;
  MoveDownBtn.Enabled := (TestsList.ItemIndex >= 0) and
    (TestsList.ItemIndex <> TestsList.Count - 1);
end;

procedure TProblemPropsEditor.RefreshTests;
var
  I: integer;
  WasIndex: integer;
begin
  WasIndex := TestsList.ItemIndex;
  with TestsList.Items do
  begin
    BeginUpdate;
    Clear;
    for I := 0 to FProperties.TestCount - 1 do
      Add(TestToStr(FProperties.Tests[I]));
    EndUpdate;
  end;
  if WasIndex < TestsList.Count then
    TestsList.ItemIndex := WasIndex;
end;

procedure TProblemPropsEditor.RecalcWidths;
var
  I: integer;
  MaxWidth: integer;
begin
  with TestsList.Items do
  begin
    MaxWidth := 0;
    TestsList.Canvas.Font.Assign(TestsList.Font);
    for I := 0 to Count - 1 do
      MaxWidth := Max(MaxWidth, TestsList.Canvas.TextWidth(Strings[I]));
    TestsList.ScrollWidth := MaxWidth + 2;
  end;
end;

constructor TProblemPropsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FProperties := TProblemProperties.Create;
  TestsList.DoubleBuffered := True;
  ImageKeeper.ImageList.GetBitmap(1, AddTestBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(2, DeleteTestBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(3, InsertTestBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(4, MoveDownBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(5, MoveUpBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(12, EditTestBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(13, MultiAddTestsBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(14, ClearTestsBtn.Glyph);
  ImageKeeper.ImageList.GetBitmap(15, EditCostsBtn.Glyph);
end;

destructor TProblemPropsEditor.Destroy;
begin
  SaveToJSON;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

end.
