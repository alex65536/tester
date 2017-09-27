{
  timerlib.pas - FreePascal binding for libtimer.a library
 
  Copyright (C) 2017 Alexander Kernozhitsky <sh200105@mail.ru>

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

{.$Define TimerlibDyn} // uncomment it if you want to use Timerlib as DLL.
{$mode objfpc}{$h+}
{$packrecords c}

// Specifying libs for static linking
{$IfNDef TimerlibDyn}
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
      {$Warning Cannot link with static library in Win64. Using dynamic linking}
      {$Define TimerlibDyn}
    {$EndIf}
  {$Else}
    {$linklib libtimer-linux}
    {$linklib c}
    {$linklib libgcc}
  {$EndIf}
{$EndIf}

// Determining lib paths for dynamic linking
{$IfDef TimerlibDyn}
  {$IfDef Windows}
    {$IfDef Win32}
      const
        TimerLibName = 'libtimer0.1.2-32.dll';
    {$EndIf}
    {$IfDef Win64}
      const
        TimerLibName = 'libtimer0.1.2-64.dll';
    {$EndIf}
  {$Else}
    const
      TimerLibName = 'libtimer-0.1.2.so';
    {$linklib c}
    {$linklib libgcc}
  {$EndIf}
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
  cdecl; external {$IfDef TimerlibDyn} TimerLibName {$EndIf} Name 'launch_timer';

procedure EnableGuiMode;

implementation

var
  FGuiModeEnabled: boolean = False;

procedure InitTimer; cdecl; external {$IfDef TimerlibDyn} TimerLibName {$EndIf} Name 'init_timer';
procedure GuiMode; cdecl; external {$IfDef TimerlibDyn} TimerLibName {$EndIf} Name 'gui_mode';

procedure EnableGuiMode;
begin
  if not FGuiModeEnabled then
  begin
    FGuiModeEnabled := True;
    GuiMode;
  end;
end;

initialization
  InitTimer;

end.
