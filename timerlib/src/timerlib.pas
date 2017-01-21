{
  timerlib.pas - FreePascal binding for libtimer.a library
 
  Copyright (C) 2017 Kernozhitsky Alexander <sh200105@mail.ru>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.
 
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
}
unit timerlib;

interface

{$mode objfpc}{$h+}
{$packrecords c}

{$IfDef Windows}
  // On Windows, you should specify your paths to these C libraries.
  // To do this, you can use your fp.cfg file or do this in Lazarus project options.
  // Also magic skills required.
  {$IfDef Win32}
    {$linklib libtimer-win32}
    {$linklib libkernel32}
    {$linklib libmsvcrt}
    {$linklib libcrtdll}
    {$linklib libgcc}
  {$EndIf}
  {$IfDef Win64}
    {$Warning I don't know how to make timerlib for Win64 link...}
    {$linklib libtimer-win64}
    {$linklib libkernel32}
    {$linklib libmsvcr100}
    {$linklib libmsvcrt}
    {$linklib libcrtdll}
    {$linklib libgcc}
  {$EndIf}
{$Else}
  {$linklib libtimer-linux}
  {$linklib c}
  {$linklib libgcc}
{$EndIf}
  
type
  PChar = ^char;
  PInteger = ^integer;

type
  TTimerResult = (
    trOK,
    trTimeLimit,
    trRealTimeLimit,
    trMemoryLimit,
    trRuntimeError,
    trRunFail
    );

function LaunchTimer(WorkingDir, ExeName, StdinRedir, StdoutRedir, StderrRedir: PChar;
  TimeLimit, RealtimeLimit, SetMemoryLimit, MemoryLimit: integer;
  WorkTime, WorkRealtime, WorkMemory, ExitCode: PInteger): TTimerResult;
  cdecl; external name 'launch_timer';

implementation

procedure InitTimer; cdecl; external name 'init_timer';

initialization
  InitTimer;
  
end.
