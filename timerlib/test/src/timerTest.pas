program TimerTest;

{$mode objfpc}{$h+}

uses timerlib;

var
  WorkTime, WorkRealtime, WorkMemory, ExitCode: Integer;

begin
  LaunchTimer('.', 'hello', '', '', '', 1000, 2000, 
    256 * 1048576, 256 * 1048576, @WorkTime, @WorkRealtime, @WorkMemory, @ExitCode);
  WriteLn('Time = ', WorkTime, ', RealTime = ', WorkRealtime, 
          ', Memory = ', WorkMemory, ', ExitCode = ', ExitCode);
end.
