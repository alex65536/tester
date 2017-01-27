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
unit parsertest;

// This is just a test for the parser base system, it will be removed later.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, propsparserbase, parserlists, jsonsaver, checkers;

type

  { TFailableParser }

  TFailableParser = class(TPropertiesParserBase) // 'f'
  protected
    function DoParse: boolean; override;
  end;

  { TNoneParser }

  TNoneParser = class(TPropertiesParserBase) // 'n'
  protected
    function DoParse: boolean; override;
  end;

  { TWaitParser }

  TWaitParser = class(TPropertiesParserBase) // 'w'
  protected
    function DoParse: boolean; override;
  end;

  { TAllParser }

  TAllParser = class(TPropertiesParserBase) // 'a'
  protected
    function DoParse: boolean; override;
  end;

  { TStrParser }

  TStrParser = class(TPropertiesParserBase) // 's'
  protected
    function DoParse: boolean; override;
  end;

  { TIntParser }

  TIntParser = class(TPropertiesParserBase) // 'i'
  protected
    function DoParse: boolean; override;
  end;

  { TChkParser }

  TChkParser = class(TPropertiesParserBase) // 'c'
  protected
    function DoParse: boolean; override;
  end;

  { TTestParser }

  TTestParser = class(TPropertiesParserBase) // 't'
  protected
    function DoParse: boolean; override;
  end;

procedure RunParserTesting;

implementation

var
  AThread: TPropertiesParserThread;

type

  { TThreadWatcher }

  TThreadWatcher = class
    procedure ThreadTerminate(Sender: TObject);
  end;

procedure RunParserTesting;
var
  AWatcher: TThreadWatcher;
  S: string;
  I: integer;

  procedure TerminateThread;
  begin
    if AThread <> nil then
    begin
      AThread.Terminate;
      AThread.WaitFor;
    end;
  end;

begin
  WriteLn('  Parser testing mode');
  WriteLn('  Type q to quit');
  WriteLn('  Type x to terminate current thread');
  WriteLn('  Type ? to synchronize with threads');
  WriteLn('  Type string of letters f, n, w, a, s, i, c, t');
  WriteLn('to start a thread with the specified parsers');
  WriteLn('-------------------');
  AWatcher := TThreadWatcher.Create;
  AThread := nil;
  try
    while True do
    begin
      ReadLn(S);
      if S = '?' then
       CheckSynchronize
      else if S = 'q' then
      begin
        TerminateThread;
        Break;
      end
      else if S = 'x' then
      begin
        if AThread = nil then
          WriteLn('Nothing to terminate!')
        else
          TerminateThread;
      end
      else
      begin
        TerminateThread;
        AThread := TPropertiesParserThread.Create(True);
        with AThread.List.Items do
        begin
          for I := 1 to Length(S) do
            case S[I] of
              'f': Add(TFailableParser);
              'n': Add(TNoneParser);
              'w': Add(TWaitParser);
              'a': Add(TAllParser);
              's': Add(TStrParser);
              'i': Add(TIntParser);
              'c': Add(TChkParser);
              't': Add(TTestParser);
              else
                WriteLn('Unexpected letter "', I, '"');
            end;
        end;
        AThread.OnTerminate := @AWatcher.ThreadTerminate;
        WriteLn('Thread ID = ', AThread.ThreadID, ' was started.');
        AThread.FreeOnTerminate := True;
        AThread.Start;
      end;
    end;
  finally
    FreeAndNil(AWatcher);
  end;
end;

{ TThreadWatcher }

procedure TThreadWatcher.ThreadTerminate(Sender: TObject);
begin
  with Sender as TThread do
  begin
    WriteLn('Thread ID = ', AThread.ThreadID, ' was terminated.');
    WriteLn('Status = ', AThread.Status);
    if AThread.Status <> ppTerminated then
      WriteLn('Outcome = ', SavePropsToJSONStr(AThread.List.Properties));
    AThread := nil;
  end;
end;

{ TTestParser }

function TTestParser.DoParse: boolean;
const
  TestPrefixes: array [0 .. 3] of string = ('apples', 'oranges', 'paintbox', 'point');
var
  I, X: integer;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  X := Random(4);
  for I := 1 to 10 do
    with Properties.TestList.Add do
    begin
      InputFile := Format('%s%.2d.in', [TestPrefixes[X], I]);
      OutputFile := Format('%s%.2d.out', [TestPrefixes[X], I]);
      Cost := 42;
    end;
end;

{ TChkParser }

function TChkParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  case Random(3) of
    0: Properties.Checker := TFileCompareChecker.Create;
    1: Properties.Checker := TTextChecker.Create;
    2: Properties.Checker := TTestlibChecker.Create
    else;
  end;
end;

{ TIntParser }

function TIntParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  Properties.TimeLimit := Random(42);
  Properties.MemoryLimit := Random(42);
end;

{ TStrParser }

function TStrParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  Properties.InputFile := IntToStr(Random(42));
  Properties.OutputFile := IntToStr(Random(42));
end;

{ TAllParser }

function TAllParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  Properties.InputFile := 'alex256.in';
  Properties.OutputFile := 'alex256.out';
  Properties.TimeLimit := 4242;
  Properties.MemoryLimit := 256256;
  Properties.Checker := TTestlibChecker.Create;
  (Properties.Checker as TTestlibChecker).CheckerFileName := 'megaCheck.exe';
  with Properties.TestList.Add do
  begin
    InputFile := '42';
    OutputFile := '42.a';
    Cost := 42;
  end;
end;

{ TWaitParser }

function TWaitParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  Sleep(1024);
end;

{ TNoneParser }

function TNoneParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  // do nothing
end;

{ TFailableParser }

function TFailableParser.DoParse: boolean;
begin
  Result := True;
  WriteLn('Launching ', ClassName);
  Result := False;
end;

end.

