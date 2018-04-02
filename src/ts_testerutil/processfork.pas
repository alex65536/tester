{
  This file is part of Tester

  Copyright (c) 1999-2000 by the Free Pascal development team
  Copyright (c) 2017 Alexander Kernozhitsky <sh200105@mail.ru>

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// This unit is a modification of RunCommandInDir from FreePascal's process.pp.
unit processfork;

{$mode objfpc}{$H+}{$COperators on}

interface

uses
  Classes, SysUtils, process, UTF8Process, LazFileUtils;

function RunCommandIndirUTF8(const CurDir: string; const ExeName: string;
  const Commands: array of string; out OutputString: string;
  out ExitCode: integer): integer;

implementation

{$IfDef WINDOWS}

// A hack for Windows users, so they need not to add paths to g++
// if they have Code::Blocks MinGW.
procedure GetNewProcessEnv(Environment: TStrings);
const
  AddToPath: array [0 .. 1] of string = (
    'C:\Program Files (x86)\CodeBlocks\MinGW\bin',
    'C:\Program Files\CodeBlocks\MinGW\bin'
  );
var
  S: string;
  I: integer;
  WasPath, NewPath: string;
begin
  Environment.Clear;
  for I := 0 to GetEnvironmentVariableCount - 1 do
    Environment.Add(GetEnvironmentString(I));
  WasPath := Environment.Values['PATH'];
  NewPath := '';
  for S in AddToPath do
    NewPath += S + ';';
  NewPath += WasPath;
  Environment.Values['PATH'] := NewPath;
end;

{$Else}

// On Linux, we do nothing (everything is already in PATH, hack is not needed)
procedure GetNewProcessEnv(Environment: TStrings);
begin
  Environment.Clear;
end;

{$EndIf}

function ExpandExeName(PathEnv: string; const ExeName: string): string;

  function ValidExecutable(const ExeName: string): boolean;
  begin
    Result := FileExistsUTF8(ExeName) and FileIsExecutable(ExeName);
  end;

var
  Path: string;
  NewExeName: string;
  Items: TStringArray;
begin
  if PathEnv = '' then
    PathEnv := GetEnvironmentVariable('PATH');
  // if already has slashes, no need to expand it
  if ExeName.Contains(DirectorySeparator) then
    Exit(ExeName);
  {$IfDef WINDOWS}
  // search the current directory
  if ValidExecutable(ExeName) then
    Exit(ExeName);
  {$EndIf}
  // search in PATH env
  Items := PathEnv.Split([PathSeparator]);
  for Path in Items do
  begin
    NewExeName := AppendPathDelim(Path) + ExeName;
    if ValidExecutable(NewExeName) then
      Exit(NewExeName);
  end;
  // not found, return what we've been passed
  Result := ExeName;
end;

{
  The following two procedures were copied from FCL's process.pp
  They are modified
  Patches are:
    1. They were designed to capture stderr, not only stdout
    2. JCF reformated the code :)
    3. Using TProcessUTF8 instead of TProcess
    4. Fixed codestyle (e. g. variable & method names)
    5. Maybe I forgot something?
}

const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

// helperfunction that does the bulk of the work.
function InternalRunCommandUTF8(P: TProcessUTF8; out OutputString: string;
  out StderrString: string; out ExitCode: integer): integer;
var
  NumBytes, BytesRead, Available: integer;
  OutputLength, StderrLength: integer;
  StderrNumBytes, StderrBytesRead: integer;
begin
  Result := -1;
  try
    try
      GetNewProcessEnv(P.Environment);
      P.Executable := ExpandExeName(P.Environment.Values['PATH'], P.Executable);
      // modification here: we append stderr to stdout.
      P.Options := [poUsePipes, poStderrToOutPut];
      P.ShowWindow := swoHide;
      // end of modification
      BytesRead := 0;
      OutputLength := 0;
      StderrBytesRead := 0;
      StderrLength := 0;
      P.Execute;
      while P.Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts both on output and stderr pipes. PM.
        Available := P.Output.NumBytesAvailable;
        if Available > 0 then
        begin
          if (BytesRead + Available > OutputLength) then
          begin
            OutputLength := BytesRead + READ_BYTES;
            SetLength(OutputString, OutputLength);
          end;
          NumBytes := P.Output.Read(OutputString[1 + BytesRead], Available);
          if NumBytes > 0 then
            Inc(BytesRead, NumBytes);
        end
        // The check for assigned(P.Stderr) is mainly here so that
        // if we use poStderrToOutput in P.Options, we do not access invalid memory.
        else if Assigned(P.StdErr) and (P.StdErr.NumBytesAvailable > 0) then
        begin
          Available := P.StdErr.NumBytesAvailable;
          if (StderrBytesRead + Available > StderrLength) then
          begin
            StderrLength := StderrBytesRead + READ_BYTES;
            SetLength(StderrString, StderrLength);
          end;
          StderrNumBytes := P.StdErr.Read(StderrString[1 + StderrBytesRead], Available);
          if StderrNumBytes > 0 then
            Inc(StderrBytesRead, StderrNumBytes);
        end
        else
          Sleep(15);
      end;
      // Get left output after end of execution
      Available := P.Output.NumBytesAvailable;
      while Available > 0 do
      begin
        if (BytesRead + Available > OutputLength) then
        begin
          OutputLength := BytesRead + READ_BYTES;
          SetLength(OutputString, OutputLength);
        end;
        NumBytes := P.Output.Read(OutputString[1 + BytesRead], Available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        Available := P.Output.NumBytesAvailable;
      end;
      SetLength(OutputString, BytesRead);
      while Assigned(P.Stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        Available := P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + Available > StderrLength) then
        begin
          StderrLength := StderrBytesRead + READ_BYTES;
          SetLength(StderrString, StderrLength);
        end;
        StderrNumBytes := P.StdErr.Read(StderrString[1 + StderrBytesRead], Available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
      SetLength(StderrString, StderrBytesRead);
      ExitCode := P.ExitCode;
      if ExitCode = 0 then
        ExitCode := P.ExitStatus;
      Result := 0; // we came to here, document that.
    except
      on E: Exception do
      begin
        Result := 1;
        SetLength(OutputString, BytesRead);
      end;
    end;
  finally
    P.Free;
  end;
end;

function RunCommandIndirUTF8(const CurDir: string; const ExeName: string;
  const Commands: array of string; out OutputString: string;
  out ExitCode: integer): integer;
var
  P: TProcessUTF8;
  I: integer;
  ErrorString: string;
begin
  P := TProcessUTF8.Create(nil);
  P.Executable := ExeName;
  if CurDir <> '' then
    P.CurrentDirectory := CurDir;
  if High(Commands) >= 0 then
    for I := Low(Commands) to High(Commands) do
      P.Parameters.Add(Commands[I]);
  Result := InternalRunCommandUTF8(P, OutputString, errorstring, ExitCode);
end;

end.
