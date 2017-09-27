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
unit compilers;

{$mode objfpc}{$H+}{$B-}

interface

uses
  Classes, SysUtils, processfork, strconsts, AvgLvlTree, testerprimitives,
  FileUtil, LazFileUtils;

type
  ECompiler = class(Exception);

  { TCompiler }

  TCompiler = class
  private
    FCompilerOutput: string;
    FExeName: string;
    FSrcName: string;
    FStackSize: integer;
    FWorkingDir: string;
    procedure SetCompilerOutput(AValue: string);
    procedure SetExeName(AValue: string);
    procedure SetSrcName(AValue: string);
    procedure SetStackSize(AValue: integer);
    procedure SetWorkingDir(AValue: string);
  public
    property SrcName: string read FSrcName write SetSrcName;
    property ExeName: string read FExeName write SetExeName;
    property WorkingDir: string read FWorkingDir write SetWorkingDir;
    property StackSize: integer read FStackSize write SetStackSize;
    property CompilerOutput: string read FCompilerOutput write SetCompilerOutput;
    function Compile: TCompilerVerdict; virtual; abstract;
    function LanguageName: string; virtual; abstract;
    function CompilerName: string; virtual; abstract;
    function CompilerVersion: string; virtual; abstract;
    function CompilerFullName: string;
    constructor Create; virtual;
  end;

  TCompilerClass = class of TCompiler;

  { TProcessCompiler }

  TProcessCompiler = class(TCompiler)
  protected
    function GetCmdName: string; virtual; abstract;
    procedure GetCommandLine(Args: TStringList); virtual; abstract;
    function GetVersionKey: string; virtual; abstract;
  public
    function Compile: TCompilerVerdict; override;
    function CompilerVersion: string; override;
  end;

  { TFreePascalCompiler }

  TFreePascalCompiler = class(TProcessCompiler)
  protected
    function GetCmdName: string; override;
    procedure GetCommandLine(Args: TStringList); override;
    function GetVersionKey: string; override;
  public
    function LanguageName: string; override;
    function CompilerName: string; override;
  end;

  { TDelphiCompiler }

  TDelphiCompiler = class(TFreePascalCompiler)
  protected
    procedure GetCommandLine(Args: TStringList); override;
  public
    function LanguageName: string; override;
  end;

  { TGnuCCompiler }

  TGnuCCompiler = class(TProcessCompiler)
  protected
    function GetCmdName: string; override;
    procedure GetCommandLine(Args: TStringList); override;
    function GetVersionKey: string; override;
  public
    function LanguageName: string; override;
    function CompilerName: string; override;
  end;

  { TGnuCppCompiler }

  TGnuCppCompiler = class(TProcessCompiler)
  protected
    function GetCmdName: string; override;
    procedure GetCommandLine(Args: TStringList); override;
    function GetVersionKey: string; override;
  public
    function LanguageName: string; override;
    function CompilerName: string; override;
  end;

  { TGnuCpp11Compiler }

  TGnuCpp11Compiler = class(TGnuCppCompiler)
  protected
    procedure GetCommandLine(Args: TStringList); override;
  public
    function LanguageName: string; override;
  end;

var
  FreePascalDir: string = '';
  GccDir: string = '';
  GppDir: string = '';
{$IfDef Windows}
  FreePascalExe: string = 'fpc.exe';
  GccExe: string = 'gcc.exe';
  GppExe: string = 'g++.exe';
{$Else}
  FreePascalExe: string = 'fpc';
  GccExe: string = 'gcc';
  GppExe: string = 'g++';
{$EndIf}

const
  DefaultStackSize = 16384; // in KBytes

procedure RegisterCompiler(const Extension: string; AClass: TCompilerClass);
function CompileFile(const SrcName, ExeName: string; out Output: string;
  StackSize: TProblemMemory = DefaultStackSize; WorkingDir: string = ''): TCompilerVerdict;
function CompileChecker(const AFileName: string): string;

implementation

var
  CompilerMap: TStringToPointerTree;

procedure RegisterCompiler(const Extension: string; AClass: TCompilerClass);
begin
  CompilerMap[Extension] := AClass;
end;

function CompileFile(const SrcName, ExeName: string; out Output: string;
  StackSize: TProblemMemory; WorkingDir: string): TCompilerVerdict;
var
  Extension: string;
  Compiler: TCompiler;
begin
  Extension := ExtractFileExt(SrcName);
  if not CompilerMap.Contains(Extension) then
  begin
    Output := Format(SCompilerNotRegistered, [Extension]);
    Result := cvCompileFail;
    Exit;
  end;
  try
    Compiler := TCompilerClass(CompilerMap[Extension]).Create;
    try
      if WorkingDir = '' then
        Compiler.WorkingDir := GetCurrentDirUTF8
      else
        Compiler.WorkingDir := ExpandFileNameUTF8(WorkingDir);
      Compiler.SrcName := ExpandFileNameUTF8(SrcName);
      Compiler.ExeName := ExpandFileNameUTF8(ExeName);
      Compiler.StackSize := StackSize;
      Result := Compiler.Compile;
      Output := Compiler.CompilerOutput;
    finally
      FreeAndNil(Compiler);
    end;
  except
    on E: Exception do
    begin
      Result := cvCompileFail;
      Output := E.Message;
    end;
  end;
end;

function CompileChecker(const AFileName: string): string;
var
  ShortFileName, CheckerExe, CompilerOutput: string;
  CompilerVerdict: TCompilerVerdict;
begin
  ShortFileName := ExtractFileNameWithoutExt(ExtractFileName(AFileName));
  CheckerExe := AppendPathDelim(ExtractFileDir(AFileName)) + ShortFileName;
  {$IfDef Windows}
  CheckerExe := CheckerExe + '.exe';
  {$EndIf}
  if FileExistsUTF8(CheckerExe) then
    // already compiled
    CompilerVerdict := cvSuccess
  else
    // compile it
    CompilerVerdict := CompileFile(AFileName, CheckerExe, CompilerOutput);
  if CompilerVerdict = cvSuccess then
    Result := CheckerExe
  else
    Result := '';
end;

{ TDelphiCompiler }

procedure TDelphiCompiler.GetCommandLine(Args: TStringList);
begin
  inherited GetCommandLine(Args);
  Args.Add('-Sh');
  Args.Add('-Mdelphi');
end;

function TDelphiCompiler.LanguageName: string;
begin
  Result := 'Delphi';
end;

{ TGnuCpp11Compiler }

procedure TGnuCpp11Compiler.GetCommandLine(Args: TStringList);
begin
  inherited GetCommandLine(Args);
  Args.Add('-std=c++11');
end;

function TGnuCpp11Compiler.LanguageName: string;
begin
  Result := 'C++11';
end;

{ TGnuCppCompiler }

function TGnuCppCompiler.GetCmdName: string;
begin
  Result := GppDir + GppExe;
end;

procedure TGnuCppCompiler.GetCommandLine(Args: TStringList);
begin
  Args.Clear;
  Args.Add(SrcName);
  Args.Add('-o');
  Args.Add(ExeName);
  Args.Add('-O2');
  {$IfDef Windows}
  Args.Add('-Wl,--stack=' + IntToStr(StackSize * 1024));
  {$EndIf}
end;

function TGnuCppCompiler.GetVersionKey: string;
begin
  Result := '-dumpversion';
end;

function TGnuCppCompiler.LanguageName: string;
begin
  Result := 'C++';
end;

function TGnuCppCompiler.CompilerName: string;
begin
  Result := 'g++';
end;

{ TGnuCCompiler }

function TGnuCCompiler.GetCmdName: string;
begin
  Result := GccDir + GccExe;
end;

procedure TGnuCCompiler.GetCommandLine(Args: TStringList);
begin
  Args.Clear;
  Args.Add(SrcName);
  Args.Add('-o');
  Args.Add(ExeName);
  Args.Add('-O2');
  {$IfDef Windows}
  Args.Add('-Wl,--stack=' + IntToStr(StackSize * 1024));
  {$EndIf}
end;

function TGnuCCompiler.GetVersionKey: string;
begin
  Result := '-dumpversion';
end;

function TGnuCCompiler.LanguageName: string;
begin
  Result := 'C';
end;

function TGnuCCompiler.CompilerName: string;
begin
  Result := 'gcc';
end;

{ TFreePascalCompiler }

function TFreePascalCompiler.GetCmdName: string;
begin
  Result := FreePascalDir + FreePascalExe;
end;

procedure TFreePascalCompiler.GetCommandLine(Args: TStringList);
begin
  Args.Clear;
  Args.Add(SrcName);
  Args.Add('-o' + ExeName);
  Args.Add('-O2');
end;

function TFreePascalCompiler.GetVersionKey: string;
begin
  Result := '-iV';
end;

function TFreePascalCompiler.LanguageName: string;
begin
  Result := 'Pascal';
end;

function TFreePascalCompiler.CompilerName: string;
begin
  Result := 'fpc';
end;

{ TProcessCompiler }

function TProcessCompiler.Compile: TCompilerVerdict;
var
  ExitCode: integer;
  Output: string;
  CmdName: string;
  Args: TStringList;
  ArgsArr: array of string;
  I: integer;
  CurDir: string;
begin
  try
    Args := TStringList.Create;
    try
      CmdName := GetCmdName;
      GetCommandLine(Args);
      SetLength(ArgsArr, Args.Count);
      for I := 0 to Args.Count - 1 do
        ArgsArr[I] := Args[I];
      ExitCode := 0;
      Output := '';
      if WorkingDir = '' then
        CurDir := GetCurrentDirUTF8
      else
        CurDir := WorkingDir;
      if RunCommandIndirUTF8(CurDir, CmdName, ArgsArr,
        Output, ExitCode) <> 0 then
      begin
        CompilerOutput := Format(SCompilerError, [CmdName]);
        Result := cvCompileFail;
      end
      else
      begin
        CompilerOutput := Output + LineEnding + Format(SCompilerExitCode, [ExitCode]);
        if ExitCode = 0 then
          Result := cvSuccess
        else
          Result := cvCompilationError;
      end;
    finally
      FreeAndNil(Args);
    end;
  except
    on E: Exception do
    begin
      Result := cvCompileFail;
      CompilerOutput := E.Message;
    end;
  end;
end;

function TProcessCompiler.CompilerVersion: string;
var
  Status: integer;
  ExitCode: integer;
begin
  Status := RunCommandIndirUTF8('', GetCmdName, [GetVersionKey], Result, ExitCode);
  if (ExitCode <> 0) or (Status <> 0) then
    raise ECompiler.Create(SCouldNotCompilerVersion);
  // trim spaces/newlines at the end of the output
  while (Result <> '') and (Result[Length(Result)] in [#0 .. ' ']) do
    Delete(Result, Length(Result), 1);
end;

{ TCompiler }

procedure TCompiler.SetCompilerOutput(AValue: string);
begin
  if FCompilerOutput = AValue then
    Exit;
  FCompilerOutput := AValue;
end;

procedure TCompiler.SetExeName(AValue: string);
begin
  if FExeName = AValue then
    Exit;
  FExeName := AValue;
end;

procedure TCompiler.SetSrcName(AValue: string);
begin
  if FSrcName = AValue then
    Exit;
  FSrcName := AValue;
end;

procedure TCompiler.SetStackSize(AValue: integer);
begin
  if FStackSize = AValue then
    Exit;
  FStackSize := AValue;
end;

procedure TCompiler.SetWorkingDir(AValue: string);
begin
  if FWorkingDir = AValue then
    Exit;
  FWorkingDir := AValue;
end;

function TCompiler.CompilerFullName: string;
begin
  Result := Format(SCompilerFullVersion, [LanguageName, CompilerName, CompilerVersion]);
end;

constructor TCompiler.Create;
begin
  WorkingDir := '';
  StackSize := DefaultStackSize;
end;

initialization
  CompilerMap := TStringToPointerTree.Create(False);
  RegisterCompiler('.pas', TFreePascalCompiler);
  RegisterCompiler('.pp', TFreePascalCompiler);
  RegisterCompiler('.dpr', TDelphiCompiler);
  RegisterCompiler('.c', TGnuCCompiler);
  RegisterCompiler('.cxx', TGnuCppCompiler);
  RegisterCompiler('.cpp', TGnuCpp11Compiler);
  //RegisterCompiler('.cpp', TGnuCppCompiler);
  //RegisterCompiler('.c11', TGnuCpp11Compiler);

finalization
  FreeAndNil(CompilerMap);

end.
