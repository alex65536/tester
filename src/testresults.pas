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
unit testresults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testerprimitives;

type
  TProblemTime = testerprimitives.TProblemTime;
  TProblemMemory = testerprimitives.TProblemMemory;
  TCompilerVerdict = testerprimitives.TCompilerVerdict;
  TTestVerdict = testerprimitives.TTestVerdict;

  { TTestResult }

  TTestResult = class(TCollectionItem)
  private
    FCheckerOutput: string;
    FMemory: TProblemTime;
    FScore: double;
    FTime: TProblemMemory;
    FVerdict: TTestVerdict;
    procedure SetCheckerOutput(AValue: string);
    procedure SetMemory(AValue: TProblemMemory);
    procedure SetScore(AValue: double);
    procedure SetTime(AValue: TProblemTime);
    procedure SetVerdict(AValue: TTestVerdict);
  public
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ATime: TProblemTime; AMemory: TProblemMemory;
      AOutput: string; AVerdict: TTestVerdict);
    constructor Create(ACollection: TCollection); override;
  published
    property Time: TProblemTime read FTime write SetTime;
    property Memory: TProblemMemory read FMemory write SetMemory;
    property CheckerOutput: string read FCheckerOutput write SetCheckerOutput;
    property Verdict: TTestVerdict read FVerdict write SetVerdict;
    property Score: double read FScore write SetScore;
  end;

  { TTestResultList }

  TTestResultList = class(TCollection)
  private
    procedure SetItem(Index: integer; AValue: TTestResult);
    function GetItem(Index: integer): TTestResult;
  public
    function Add: TTestResult;
    function Insert(Index: integer): TTestResult;
    property Items[Index: integer]: TTestResult read GetItem write SetItem; default;
    constructor Create;
  end;

  { TTestedProblem }

  TTestedProblem = class(TPersistent)
  private
    FCompilerOutput: string;
    FCompileVerdict: TCompilerVerdict;
    FItems: TTestResultList;
    function GetTestResultsCount: integer;
    function GetTestResults(I: integer): TTestResult;
    procedure SetCompilerOutput(AValue: string);
    procedure SetCompileVerdict(AValue: TCompilerVerdict);
    procedure SetTestResults(I: integer; AValue: TTestResult);
  public
    constructor Create;
    destructor Destroy; override;
    property TestResults[I: integer]: TTestResult
      read GetTestResults write SetTestResults; default;
    property TestResultsCount: integer read GetTestResultsCount;
    function TotalScore: double;
  published
    property CompileVerdict: TCompilerVerdict
      read FCompileVerdict write SetCompileVerdict;
    property CompilerOutput: string read FCompilerOutput write SetCompilerOutput;
    property Items: TTestResultList read FItems;
    procedure AssignTo(Dest: TPersistent); override;
  end;

implementation

{ TTestedProblem }

function TTestedProblem.GetTestResultsCount: integer;
begin
  Result := FItems.Count;
end;

function TTestedProblem.GetTestResults(I: integer): TTestResult;
begin
  Result := FItems[I];
end;

procedure TTestedProblem.SetCompilerOutput(AValue: string);
begin
  if FCompilerOutput = AValue then
    Exit;
  FCompilerOutput := AValue;
end;

procedure TTestedProblem.SetCompileVerdict(AValue: TCompilerVerdict);
begin
  if FCompileVerdict = AValue then
    Exit;
  FCompileVerdict := AValue;
end;

procedure TTestedProblem.SetTestResults(I: integer; AValue: TTestResult);
begin
  FItems[I] := AValue;
  FreeAndNil(AValue);
end;

constructor TTestedProblem.Create;
begin
  FItems := TTestResultList.Create;
  FCompileVerdict := cvWaiting;
  FCompilerOutput := '';
end;

destructor TTestedProblem.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TTestedProblem.TotalScore: double;
var
  I: integer;
begin
  Result := 0.0;
  for I := 0 to TestResultsCount - 1 do
    Result := Result + TestResults[I].Score;
end;

procedure TTestedProblem.AssignTo(Dest: TPersistent);
begin
  with Dest as TTestedProblem do
  begin
    CompileVerdict := Self.CompileVerdict;
    CompilerOutput := Self.CompilerOutput;
    Items.Assign(Self.Items);
  end;
end;

{ TTestResultList }

procedure TTestResultList.SetItem(Index: integer; AValue: TTestResult);
begin
  inherited SetItem(Index, AValue);
end;

function TTestResultList.GetItem(Index: integer): TTestResult;
begin
  Result := (inherited GetItem(Index)) as TTestResult;
end;

function TTestResultList.Add: TTestResult;
begin
  Result := (inherited Add) as TTestResult;
end;

function TTestResultList.Insert(Index: integer): TTestResult;
begin
  Result := (inherited Insert(Index)) as TTestResult;
end;

constructor TTestResultList.Create;
begin
  inherited Create(TTestResult);
end;

{ TTestResult }

procedure TTestResult.SetMemory(AValue: TProblemMemory);
begin
  if FMemory = AValue then
    Exit;
  FMemory := AValue;
end;

procedure TTestResult.SetScore(AValue: double);
begin
  if FScore = AValue then
    Exit;
  FScore := AValue;
end;

procedure TTestResult.SetCheckerOutput(AValue: string);
begin
  if FCheckerOutput = AValue then
    Exit;
  FCheckerOutput := AValue;
end;

procedure TTestResult.SetTime(AValue: TProblemTime);
begin
  if FTime = AValue then
    Exit;
  FTime := AValue;
end;

procedure TTestResult.SetVerdict(AValue: TTestVerdict);
begin
  if FVerdict = AValue then
    Exit;
  FVerdict := AValue;
end;

procedure TTestResult.AssignTo(Dest: TPersistent);
begin
  with Dest as TTestResult do
  begin
    Time := Self.Time;
    Memory := Self.Memory;
    CheckerOutput := Self.CheckerOutput;
    Verdict := Self.Verdict;
    Score := Self.Score;
  end;
end;

constructor TTestResult.Create(ATime: TProblemTime; AMemory: TProblemMemory;
  AOutput: string; AVerdict: TTestVerdict);
begin
  inherited Create(nil);
  Time := ATime;
  Memory := AMemory;
  CheckerOutput := AOutput;
  Verdict := AVerdict;
end;

constructor TTestResult.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Time := 0;
  Memory := 0;
  CheckerOutput := '';
  Verdict := veWaiting;
  Score := 0.0;
end;

end.
