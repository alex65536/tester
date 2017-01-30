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
unit compilers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, processfork, strconsts, AvgLvlTree, testerprimitives,
  FileUtil, LazFileUtils;

type

  { TCompiler }

  TCompiler = class
  private
    FCompilerOutput: string;
    FExeName: string;
    FSrcName: string;
    procedure SetCompilerOutput(AValue: string);
    procedure SetExeName(AValue: string);
    procedure SetSrcName(AValue: string);
  public
    property SrcName: string read FSrcName write SetSrcName;
    property ExeName: string read FExeName write SetExeName;
    property CompilerOutput: string read FCompilerOutput write SetCompilerOutput;
    function Compile: TCompilerVerdict; virtual; abstract;
    constructor Create; virtual;
  end;

  TCompilerClass = class of TCompiler;

  { TProcessCompiler }

  TProcessCompiler = class(TCompiler)
  protected
    procedure GetCommandLine(var CmdName: string; Args: TStringList);
      virtual; abstract;
  public
    function Compile: TCompilerVerdict; override;
  end;

  { TFreePascalCompiler }

  TFreePascalCompiler = class(TProcessCompiler)
  protected
    procedure GetCommandLine(var CmdName: string; Args: TStringList); override;
  end;

  { TGnuCCompiler }

  TGnuCCompiler = class(TProcessCompiler)
  protected
    procedure GetCommandLine(var CmdName: string; Args: TStringList); override;
  end;

  { TGnuCppCompiler }

  TGnuCppCompiler = class(TProcessCompiler)
  protected
    procedure GetCommandLine(var CmdName: string; Args: TStringList); override;
  end;

  { TGnuCpp11Compiler }

  TGnuCpp11Compiler = class(TGnuCppCompiler)
  protected
    procedure GetCommandLine(var CmdName: string; Args: TStringList); override;
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


procedure RegisterCompiler(const Extension: string; AClass: TCompilerClass);
function CompileFile(const SrcName, ExeName: string; var Output: string): TCompilerVerdict;
function CompileChecker(const AFileName: string): string;

implementation

var
  CompilerMap: TStringToPointerTree;

procedure RegisterCompiler(const Extension: string; AClass: TCompilerClass);
begin
  CompilerMap[Extension] := AClass;
end;

function CompileFile(const SrcName, ExeName: string;
  var Output: string): TCompilerVerdict;
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
      Compiler.SrcName := SrcName;
      Compiler.ExeName := ExeName;
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

{ TGnuCpp11Compiler }

procedure TGnuCpp11Compiler.GetCommandLine(var CmdName: string; Args: TStringList);
begin
  inherited GetCommandLine(CmdName, Args);
  Args.Add('-std=c++11');
end;

{ TGnuCppCompiler }

procedure TGnuCppCompiler.GetCommandLine(var CmdName: string; Args: TStringList);
begin
  CmdName := GppDir + GppExe;
  Args.Text := SrcName + LineEnding + '-o' + LineEnding + ExeName + LineEnding + '-O2';
end;

{ TGnuCCompiler }

procedure TGnuCCompiler.GetCommandLine(var CmdName: string; Args: TStringList);
begin
  CmdName := GccDir + GccExe;
  Args.Text := SrcName + LineEnding + '-o' + LineEnding + ExeName +
    LineEnding + '-O2' + LineEnding;
end;

{ TFreePascalCompiler }

procedure TFreePascalCompiler.GetCommandLine(var CmdName: string; Args: TStringList);
begin
  CmdName := FreePascalDir + FreePascalExe;
  Args.Text := SrcName + LineEnding + '-o' + ExeName + LineEnding + '-O2';
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
begin
  try
    Args := TStringList.Create;
    try
      GetCommandLine(CmdName, Args);
      SetLength(ArgsArr, Args.Count);
      for I := 0 to Args.Count - 1 do
        ArgsArr[I] := Args[I];
      ExitCode := 0;
      Output := '';
      if RunCommandIndirUTF8(GetCurrentDirUTF8, CmdName, ArgsArr, Output, ExitCode) <> 0 then
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

constructor TCompiler.Create;
begin
end;

initialization
  CompilerMap := TStringToPointerTree.Create(False);
  RegisterCompiler('.pas', TFreePascalCompiler);
  RegisterCompiler('.pp', TFreePascalCompiler);
  RegisterCompiler('.c', TGnuCCompiler);
  RegisterCompiler('.cpp', TGnuCpp11Compiler);
  //RegisterCompiler('.cpp', TGnuCppCompiler);
  //RegisterCompiler('.c11', TGnuCpp11Compiler);

finalization
  FreeAndNil(CompilerMap);

end.
