{
  This file is part of Tester

  Copyright (c) 1999-2000 by the Free Pascal development team
  Copyright (c) 2017 Kernozhitsky Alexander <sh200105@mail.ru>

  This program is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{
  This unit is a modification of RunCommandInDir from FreePascal's process.pp.
  RunCommandInDir from this unit redirects stderr to stdout, which is nessesary
  to show compilation error from gcc and g++ (they write to stderr rather than
  stdout). Also it uses TProcessUTF8 instead of TProcess (for Unicode support)
}
unit processfork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, UTF8Process;

function RunCommandIndirUTF8(const curdir: string; const exename: string;
  const commands: array of string; var outputstring: string;
  var exitcode: integer): integer;

implementation

// The following two procedures were copied from FCL's process.pp
// The second one is a little bit modified

const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

// helperfunction that does the bulk of the work.
// We need to also collect stderr output in order to avoid
// lock out if the stderr pipe is full.
function internalRuncommandUTF8(p: TProcessUTF8; var outputstring: string;
  var stderrstring: string; var exitcode: integer): integer;
var
  numbytes, bytesread, available: integer;
  outputlength, stderrlength: integer;
  stderrnumbytes, stderrbytesread: integer;
begin
  Result := -1;
  try
    try
      // modification here: we append stderr to stdout.
      p.Options := [poUsePipes, poStderrToOutPut];
      p.ShowWindow := swoHide;
      // end of modification
      bytesread := 0;
      outputlength := 0;
      stderrbytesread := 0;
      stderrlength := 0;
      p.Execute;
      while p.Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts both on output and stderr pipes. PM.
        available := P.Output.NumBytesAvailable;
        if available > 0 then
        begin
          if (BytesRead + available > outputlength) then
          begin
            outputlength := BytesRead + READ_BYTES;
            Setlength(outputstring, outputlength);
          end;
          NumBytes := p.Output.Read(outputstring[1 + bytesread], available);
          if NumBytes > 0 then
            Inc(BytesRead, NumBytes);
        end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
        begin
          available := P.StdErr.NumBytesAvailable;
          if (StderrBytesRead + available > stderrlength) then
          begin
            stderrlength := StderrBytesRead + READ_BYTES;
            Setlength(stderrstring, stderrlength);
          end;
          StderrNumBytes := p.StdErr.Read(stderrstring[1 + StderrBytesRead], available);
          if StderrNumBytes > 0 then
            Inc(StderrBytesRead, StderrNumBytes);
        end
        else
          Sleep(15);
      end;
      // Get left output after end of execution
      available := P.Output.NumBytesAvailable;
      while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
        begin
          outputlength := BytesRead + READ_BYTES;
          Setlength(outputstring, outputlength);
        end;
        NumBytes := p.Output.Read(outputstring[1 + bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available := P.Output.NumBytesAvailable;
      end;
      setlength(outputstring, BytesRead);
      while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available := P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
        begin
          stderrlength := StderrBytesRead + READ_BYTES;
          Setlength(stderrstring, stderrlength);
        end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1 + StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
      setlength(stderrstring, StderrBytesRead);
      exitcode := p.ExitCode;
      Result := 0; // we came to here, document that.
    except
      on e: Exception do
      begin
        Result := 1;
        setlength(outputstring, BytesRead);
      end;
    end;
  finally
    p.Free;
  end;
end;

function RunCommandIndirUTF8(const curdir: string; const exename: string;
  const commands: array of string; var outputstring: string;
  var exitcode: integer): integer;
var
  p: TProcessUTF8;
  i: integer;
  ErrorString: string;
begin
  p := TProcessUTF8.Create(nil);
  p.Executable := exename;
  if curdir <> '' then
    p.CurrentDirectory := curdir;
  if high(commands) >= 0 then
    for i := low(commands) to high(commands) do
      p.Parameters.add(commands[i]);
  Result := internalruncommandUTF8(p, outputstring, errorstring, exitcode);
end;

end.
