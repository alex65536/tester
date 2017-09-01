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
unit checkers;

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, problemprops, processfork, LazFileUtils, LazUTF8, strconsts,
  testerprimitives, testerfileutil, fcutils;

type

  { TProcessProblemChecker }

  TProcessProblemChecker = class(TProblemChecker)
  protected
    procedure GetCommandLine(out ExeName: string; Args: TStringList);
      virtual; abstract;
    function ProcessCheckerOutput(Output: string; ExitCode: integer): TTestVerdict;
      virtual; abstract;
    function DoCheck: TTestVerdict; override;
  end;

  { TFileCompareChecker }

  TFileCompareChecker = class(TProblemChecker)
  private
    FStripSpaces: boolean;
    procedure SetStripSpaces(AValue: boolean);
  protected
    function DoCheck: TTestVerdict; override;
  public
    constructor Create; override;
    procedure AssignTo(Dest: TPersistent); override;
    function Equals(Obj: TObject): boolean; override;
  published
    property StripSpaces: boolean read FStripSpaces write SetStripSpaces;
  end;

  TStdExecutableCheckerParamsPolicy = (secpOutAns, secpInOutAns);

  { TStdExecutableChecker }

  TStdExecutableChecker = class(TProcessProblemChecker)
  private
    FCheckerFileName: string;
    FParamsPolicy: TStdExecutableCheckerParamsPolicy;
    procedure SetCheckerFileName(AValue: string);
    procedure SetParamsPolicy(AValue: TStdExecutableCheckerParamsPolicy);
  protected
    property ParamsPolicy: TStdExecutableCheckerParamsPolicy
      read FParamsPolicy write SetParamsPolicy;
    procedure GetCommandLine(out ExeName: string; Args: TStringList); override;
  public
    constructor Create; override;
    constructor Create(const ACheckerFileName: string);
    procedure CorrectFileNames; override;
    procedure InvalidFilesList(AList: TStrings); override;
    procedure AssignTo(Dest: TPersistent); override;
    function Equals(Obj: TObject): boolean; override;
  published
    property CheckerFileName: string read FCheckerFileName write SetCheckerFileName;
  end;

  { TTextChecker }

  TTextChecker = class(TStdExecutableChecker)
  protected
    function ProcessCheckerOutput(Output: string; ExitCode: integer): TTestVerdict;
      override;
  published
    property ParamsPolicy;
  end;

  { TTestlibChecker }

  TTestlibChecker = class(TStdExecutableChecker)
  protected
    function ProcessCheckerOutput(Output: string; ExitCode: integer): TTestVerdict;
      override;
  end;

implementation

const
  TestlibOK = 0;
  TestlibWA = 1;
  TestlibPE = 2;

  TextOK = 'ok';
  TextWA1 = 'wa';
  TextWA2 = 'wrong answer';
  TextPE1 = 'pe';
  TextPE2 = 'presentation error';
  TextPE3 = 'wrong output format';

{ TTestlibChecker }

function TTestlibChecker.ProcessCheckerOutput(Output: string;
  ExitCode: integer): TTestVerdict;
begin
  Output := Output; // to mute the hint
  case ExitCode of
    TestlibOK: Result := veAccepted;
    TestlibPE: Result := vePresentationError;
    TestlibWA: Result := veWrongAnswer;
    else
      Result := veCheckError;
  end;
end;

{ TStdExecutableChecker }

procedure TStdExecutableChecker.SetCheckerFileName(AValue: string);
begin
  if FCheckerFileName = AValue then
    Exit;
  FCheckerFileName := AValue;
end;

procedure TStdExecutableChecker.SetParamsPolicy(AValue:
  TStdExecutableCheckerParamsPolicy);
begin
  if FParamsPolicy = AValue then
    Exit;
  FParamsPolicy := AValue;
end;

procedure TStdExecutableChecker.GetCommandLine(out ExeName: string; Args: TStringList);
begin
  ExeName := ExpandFileNameUTF8(CheckerFileName, WorkingDir);
  case ParamsPolicy of
    secpOutAns:
      Args.Text := OutputFile + LineEnding + AnswerFile;
    secpInOutAns:
      Args.Text := InputFile + LineEnding + OutputFile + LineEnding + AnswerFile;
  end;
end;

constructor TStdExecutableChecker.Create;
begin
  inherited;
  CheckerFileName := 'checker.exe';
  ParamsPolicy := secpInOutAns;
end;

constructor TStdExecutableChecker.Create(const ACheckerFileName: string);
begin
  Create;
  CheckerFileName := ACheckerFileName;
end;

procedure TStdExecutableChecker.CorrectFileNames;
begin
  inherited CorrectFileNames;
  CheckerFileName := CorrectFileName(CheckerFileName);
end;

procedure TStdExecutableChecker.InvalidFilesList(AList: TStrings);
begin
  inherited InvalidFilesList(AList);
  if not FileExistsUTF8(CheckerFileName) then
    AList.Add(CheckerFileName);
end;

procedure TStdExecutableChecker.AssignTo(Dest: TPersistent);
begin
  inherited;
  with Dest as TStdExecutableChecker do
  begin
    CheckerFileName := Self.CheckerFileName;
    ParamsPolicy := Self.ParamsPolicy;
  end;
end;

function TStdExecutableChecker.Equals(Obj: TObject): boolean;
begin
  if Obj.ClassType <> ClassType then
    Result := False
  else
    with Obj as TStdExecutableChecker do
      Result := (CheckerFileName = Self.CheckerFileName) and
        (ParamsPolicy = Self.ParamsPolicy);
end;

{ TTextChecker }

function TTextChecker.ProcessCheckerOutput(Output: string;
  ExitCode: integer): TTestVerdict;
begin
  ExitCode := ExitCode; // to mute the hint
  Output := Trim(LowerCase(Output));
  if Pos(TextOK, Output) = 1 then
    Result := veAccepted
  else if Pos(TextWA1, Output) = 1 then
    Result := veWrongAnswer
  else if Pos(TextWA2, Output) = 1 then
    Result := veWrongAnswer
  else if Pos(TextPE1, Output) = 1 then
    Result := vePresentationError
  else if Pos(TextPE2, Output) = 1 then
    Result := vePresentationError
  else if Pos(TextPE3, Output) = 1 then
    Result := vePresentationError
  else
    Result := veCheckError;
end;

{ TFileCompareChecker }

procedure TFileCompareChecker.SetStripSpaces(AValue: boolean);
begin
  if FStripSpaces = AValue then
    Exit;
  FStripSpaces := AValue;
end;

function TFileCompareChecker.DoCheck: TTestVerdict;
var
  OutputList: TStringList;
  AnswerList: TStringList;
  Line, Col: integer;
  Reason: string;
begin
  // check for existance
  if not FileExistsUTF8(AppendPathDelim(WorkingDir) + OutputFile) then
  begin
    CheckerOutput := Format(SFileNotFound, [OutputFile]);
    Result := vePresentationError;
    Exit;
  end;
  if not FileExistsUTF8(AppendPathDelim(WorkingDir) + AnswerFile) then
  begin
    CheckerOutput := Format(SFileNotFound, [AnswerFile]);
    Result := veCheckError;
    Exit;
  end;
  // get output and answer lists
  OutputList := TStringList.Create;
  AnswerList := TStringList.Create;
  try
    OutputList.LoadFromFile(OutputFile);
    AnswerList.LoadFromFile(AnswerFile);
    StripList(OutputList, StripSpaces);
    StripList(AnswerList, StripSpaces);
    // check for equality
    if CompareText(OutputList, AnswerList, Line, Col, Reason) then
    begin
      CheckerOutput := SFilesEqual;
      Result := veAccepted;
    end
    else
    begin
      CheckerOutput := Format(SFilesNotEqualFmt, [Line, Col, Reason]);
      Result := veWrongAnswer;
    end;
  finally
    FreeAndNil(OutputList);
    FreeAndNil(AnswerList);
  end;
end;

constructor TFileCompareChecker.Create;
begin
  inherited Create;
  StripSpaces := True;
end;

procedure TFileCompareChecker.AssignTo(Dest: TPersistent);
begin
  inherited;
  with Dest as TFileCompareChecker do
  begin
    StripSpaces := Self.StripSpaces;
  end;
end;

function TFileCompareChecker.Equals(Obj: TObject): boolean;
begin
  if Obj.ClassType <> ClassType then
    Result := False
  else
    with Obj as TFileCompareChecker do
      Result := (StripSpaces = Self.StripSpaces);
end;

{ TProcessProblemChecker }

function TProcessProblemChecker.DoCheck: TTestVerdict;
var
  ExitCode: integer;
  Output: string;
  ExeName: string;
  Args: TStringList;
  ArgsArr: array of string;
  I: integer;
begin
  Args := TStringList.Create;
  try
    GetCommandLine(ExeName, Args);
    SetLength(ArgsArr, Args.Count);
    for I := 0 to Args.Count - 1 do
      ArgsArr[I] := Args[I];
    ExitCode := 0;
    Output := '';
    if RunCommandIndirUTF8(WorkingDir, ExeName, ArgsArr, Output, ExitCode) <> 0 then
    begin
      CheckerOutput := Format(SCheckerRunFail, [ExeName]);
      Result := veCheckError;
    end
    else
    begin
      CheckerOutput := Output + LineEnding + Format(SCheckerExitCode, [ExitCode]);
      Result := ProcessCheckerOutput(Output, ExitCode);
    end;
  finally
    FreeAndNil(Args);
  end;
end;

initialization
  RegisterChecker(TFileCompareChecker);
  RegisterChecker(TTextChecker);
  RegisterChecker(TTestlibChecker);

end.
