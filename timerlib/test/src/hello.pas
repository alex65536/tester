{$mode objfpc}

var
  I: Integer;
  //Arr: Array [0 .. 10000000] of Integer;
  A: Integer;
  
begin
  writeLn('Hello!');
  for I := 0 to 10000000 do
    A := I mod 42;
  Halt(42);
end.
