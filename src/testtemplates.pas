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
unit testtemplates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemprops, LazFileUtils, strconsts, testerfileutil;

type
  ETestTemplate = class(Exception);

  TAddTestProc = procedure(ATest: TProblemTest; out Stop: boolean) of object;

  { TProblemTestTemplate }

  TProblemTestTemplate = class(TProblemTest)
  private
    FStartFrom: integer;
    procedure SetStartFrom(AValue: integer);
  public
    procedure GenerateTests(const WorkingDir: string; Proc: TAddTestProc);
    constructor Create(AInputFile, AOutputFile: string; ACost: double);
    constructor Create(AInputFile, AOutputFile: string; ACost: double;
      AStartFrom: integer);
  published
    property StartFrom: integer read FStartFrom write SetStartFrom;
  end;

implementation

{ TProblemTestTemplate }

procedure TProblemTestTemplate.SetStartFrom(AValue: integer);
begin
  if FStartFrom = AValue then
    Exit;
  FStartFrom := AValue;
end;

procedure TProblemTestTemplate.GenerateTests(const WorkingDir: string;
  Proc: TAddTestProc);
var
  I: integer;
  LastInput, LastOutput: string;
  InputFileName, OutputFileName: string;
  Stop: boolean;
begin
  BeginCache;
  try
    Stop := False;
    I := StartFrom;
    LastInput := '';
    LastOutput := '';
    while True do
    begin
      InputFileName := CorrectFileName(AppendPathDelim(WorkingDir) +
        Format(InputFile, [I]));
      OutputFileName := CorrectFileName(AppendPathDelim(WorkingDir) +
        Format(OutputFile, [I]));
      if (InputFileName = LastInput) and (OutputFileName = LastOutput) then
        Break;
      if FileExistsUTF8(InputFileName) and FileExistsUTF8(OutputFileName) then
      begin
        LastInput := InputFileName;
        LastOutput := OutputFileName;
        InputFileName := CreateRelativePath(InputFileName, WorkingDir);
        OutputFileName := CreateRelativePath(OutputFileName, WorkingDir);
        Proc(TProblemTest.Create(InputFileName, OutputFileName, Cost), Stop);
        Inc(I);
        if Stop then
          Break;
      end
      else
        Break;
    end;
  finally
    EndCache;
  end;
  if I = StartFrom then
    raise ETestTemplate.Create(SNoTestsAdded);
end;

constructor TProblemTestTemplate.Create(AInputFile, AOutputFile: string; ACost: double);
begin
  inherited;
  StartFrom := 1;
end;

constructor TProblemTestTemplate.Create(AInputFile, AOutputFile: string;
  ACost: double; AStartFrom: integer);
begin
  inherited Create(AInputFile, AOutputFile, ACost);
  StartFrom := AStartFrom;
end;

end.
