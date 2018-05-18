{
  This file is part of Tester

  Copyright (C) 2017-2018 Alexander Kernozhitsky <sh200105@mail.ru>

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
    FIncludePaths: TStringList;
    FSrcName: string;
    FStackSize: integer;
    FUnitPaths: TStringList;
    FWorkingDir: string;
    procedure SetCompilerOutput(AValue: string);
    procedure SetStackSize(AValue: integer);
    procedure SetWorkingDir(AValue: string);
  protected
    procedure SetExeName(AValue: string); virtual;
    procedure SetSrcName(AValue: string); virtual;
  public
    property IncludePaths: TStringList read FIncludePaths;
    property UnitPaths: TStringList read FUnitPaths;
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
    destructor Destroy; override;
  end;

  TCompilerClass = class of TCompiler;

  { TProcessCompiler }

  TProcessCompiler = class(TCompiler)
  private
    procedure SanitizeFileName(var S: string);
  protected
    function GetCmdName: string; virtual; abstract;
    procedure GetCommandLine(Args: TStringList); virtual; abstract;
    function GetVersionKey: string; virtual; abstract;
    procedure SetExeName(AValue: string); override;
    procedure SetSrcName(AValue: string); override;
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

  { TGnuCpp11GnuExtCompiler }

  // compiles with --std=gnu++11
  TGnuCpp11GnuExtCompiler = class(TGnuCppCompiler)
  protected
    procedure GetCommandLine(Args: TStringList); override;
  public
    function LanguageName: string; override;
  end;

  { TPythonCompiler }

  TPythonCompiler = class(TProcessCompiler)
  protected
    procedure GetCommandLine(Args: TStringList); override;
    function GetCmdName: string; override;
  public
    function Compile: TCompilerVerdict; override;
    function CompilerVersion: string; override;
    function LanguageName: string; override;
    function CompilerName: string; override;
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
  StackSize: TProblemMemory = DefaultStackSize; WorkingDir: string = '';
  IncludePaths: TStringList = nil; UnitPaths: TStringList = nil;
  CompilerClass: TCompilerClass = nil): TCompilerVerdict;

implementation

var
  CompilerMap: TStringToPointerTree;

procedure RegisterCompiler(const Extension: string; AClass: TCompilerClass);
begin
  CompilerMap[Extension] := AClass;
end;

function CompileFile(const SrcName, ExeName: string; out Output: string;
  StackSize: TProblemMemory; WorkingDir: string; IncludePaths: TStringList;
  UnitPaths: TStringList; CompilerClass: TCompilerClass): TCompilerVerdict;
var
  S: string;
  Extension: string;
  Compiler: TCompiler;
begin
  Extension := ExtractFileExt(SrcName);
  if CompilerClass = nil then
  begin
    if not CompilerMap.Contains(Extension) then
    begin
      Output := Format(SCompilerNotRegistered, [Extension]);
      Result := cvCompileFail;
      Exit;
    end;
    CompilerClass := TCompilerClass(CompilerMap[Extension]);
  end;
  try
    Compiler := CompilerClass.Create;
    try
      if IncludePaths <> nil then
        for S in IncludePaths do
          Compiler.IncludePaths.Add(ExpandFileNameUTF8(S));
      if UnitPaths <> nil then
        for S in UnitPaths do
          Compiler.UnitPaths.Add(ExpandFileNameUTF8(S));
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

{ TPythonCompiler }

procedure TPythonCompiler.GetCommandLine(Args: TStringList);
begin
  Args.Clear;
  Args.Add('-T');
  Args.Add(SrcName);
  Args.Add(ExeName);
end;

function TPythonCompiler.GetCmdName: string;
begin
  Result := 'install';
end;

function TPythonCompiler.Compile: TCompilerVerdict;
var
  StrList: TStringList;
begin
  Result := inherited Compile;
  if Result = cvSuccess then
  begin
    StrList := TStringList.Create;
    try
      StrList.LoadFromFile(ExeName);
      StrList.Insert(0, '#!/usr/bin/env python3');
      StrList.LineBreak := LineEnding;
      StrList.SaveToFile(ExeName);
    finally
      FreeAndNil(StrList);
    end;
  end;
end;

function TPythonCompiler.CompilerVersion: string;
begin
  Result := '3';
end;

function TPythonCompiler.LanguageName: string;
begin
  Result := 'Python3 (ALPHA)';
end;

function TPythonCompiler.CompilerName: string;
begin
  Result := 'install';
end;

{ TGnuCpp11GnuExtCompiler }

procedure TGnuCpp11GnuExtCompiler.GetCommandLine(Args: TStringList);
begin
  inherited GetCommandLine(Args);
  Args.Add('--std=gnu++11');
end;

function TGnuCpp11GnuExtCompiler.LanguageName: string;
begin
  Result := 'Gnu++11';
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
var
  S: string;
begin
  Args.Clear;
  Args.Add(SrcName);
  Args.Add('-o');
  Args.Add(ExeName);
  Args.Add('-O2');
  {$IfDef Windows}
  Args.Add('-Wl,--stack=' + IntToStr(StackSize * 1024));
  {$EndIf}
  for S in IncludePaths do
    Args.Add('-I' + S);
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
var
  S: string;
begin
  Args.Clear;
  Args.Add(SrcName);
  Args.Add('-o');
  Args.Add(ExeName);
  Args.Add('-O2');
  {$IfDef Windows}
  Args.Add('-Wl,--stack=' + IntToStr(StackSize * 1024));
  {$EndIf}
  for S in IncludePaths do
    Args.Add('-I' + S);
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
var
  S: string;
begin
  Args.Clear;
  Args.Add(SrcName);
  Args.Add('-o' + ExeName);
  Args.Add('-O2');
  for S in IncludePaths do
    Args.Add('-Fi' + S);
  for S in UnitPaths do
    Args.Add('-Fu' + S);
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

procedure TProcessCompiler.SanitizeFileName(var S: string);
begin
  if S = '' then
    Exit;
  if S[1] = '-' then
    S := '.' + PathDelim + S;
end;

procedure TProcessCompiler.SetExeName(AValue: string);
begin
  SanitizeFileName(AValue);
  inherited SetExeName(AValue);
end;

procedure TProcessCompiler.SetSrcName(AValue: string);
begin
  SanitizeFileName(AValue);
  inherited SetSrcName(AValue);
end;

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
  FIncludePaths := TStringList.Create;
  FUnitPaths := TStringList.Create;
  WorkingDir := '';
  StackSize := DefaultStackSize;
end;

destructor TCompiler.Destroy;
begin
  FreeAndNil(FUnitPaths);
  FreeAndNil(FIncludePaths);
  inherited Destroy;
end;

initialization
  CompilerMap := TStringToPointerTree.Create(False);
  RegisterCompiler('.pas', TFreePascalCompiler);
  RegisterCompiler('.pp', TFreePascalCompiler);
  RegisterCompiler('.dpr', TDelphiCompiler);
  RegisterCompiler('.c', TGnuCCompiler);
  RegisterCompiler('.cxx', TGnuCppCompiler);
  RegisterCompiler('.cpp', TGnuCpp11Compiler);
  //RegisterCompiler('.gpp', TGnuCpp11GnuExtCompiler);
  //RegisterCompiler('.cpp', TGnuCppCompiler);
  //RegisterCompiler('.c11', TGnuCpp11Compiler);
  RegisterCompiler('.py', TPythonCompiler);

finalization
  FreeAndNil(CompilerMap);

end.
