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
unit propsparserbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemprops;

type

  { TPropertiesParserBase }

  TPropertiesParserBase = class
  private
    FIsTerminated: boolean;
    FProperties: TProblemProperties;
  protected
    function DoParse: boolean; virtual; abstract;
  public
    property IsTerminated: boolean read FIsTerminated;
    procedure Terminate;
    property Properties: TProblemProperties read FProperties;
    function Parse: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TProblemPropsCollector }

  TProblemPropsCollector = class
  private
    FProperties: TProblemProperties;
  public
  const
    UnknownStr = '%*unknown*%';
    UnknownInt = -2147483648;
  public
    property Properties: TProblemProperties read FProperties;
    class function CleanProperties: TProblemProperties;
    function Merge(Props: TProblemProperties): boolean;
    procedure Finalize;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPropertiesParserBase }

procedure TPropertiesParserBase.Terminate;
begin
  FIsTerminated := True;
end;

function TPropertiesParserBase.Parse: boolean;
begin
  FIsTerminated := False;
  Result := DoParse;
  FIsTerminated := True;
end;

constructor TPropertiesParserBase.Create;
begin
  FProperties := TProblemProperties.Create;
  FIsTerminated := False;
end;

destructor TPropertiesParserBase.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

{ TProblemPropsCollector }

class function TProblemPropsCollector.CleanProperties: TProblemProperties;
begin
  Result := TProblemProperties.Create;
  try
    with Result do
    begin
      InputFile := UnknownStr;
      OutputFile := UnknownStr;
      TimeLimit := UnknownInt;
      MemoryLimit := UnknownInt;
      Checker := nil;
      TestList.Clear;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TProblemPropsCollector.Merge(Props: TProblemProperties): boolean;

  function MergeStr(const Str1, Str2: string; var Success: boolean): string;
  begin
    if Str1 = UnknownStr then // str1 unknown
      Result := Str2
    else if Str2 = UnknownStr then // str2 unknown
      Result := Str1
    else if Str1 = Str2 then // all are equal
      Result := Str1
    else
    begin // all are known, but differ (this is not good)
      Success := False;
      Result := Str1;
    end;
  end;

  function MergeInt(Int1, Int2: integer; var Success: boolean): integer;
  begin
    if Int1 = UnknownInt then // int1 unknown
      Result := Int2
    else if Int2 = UnknownInt then // int2 unknown
      Result := Int1
    else if Int1 = Int2 then // all are equal
      Result := Int1
    else
    begin // all are known, but differ (this is not good)
      Success := False;
      Result := Int1;
    end;
  end;

  function MergeChecker(Chk1, Chk2: TProblemChecker; var Success: boolean): TProblemChecker;
  begin
    if Chk1 = nil then // chk1 unknown
      Result := Chk2
    else if Chk2 = nil then // chk2 unknown
      Result := Chk1
    else if Chk1.Equals(Chk2) then // all are equal
      Result := Chk1
    else
    begin // all are known, but differ (this is not good)
      Success := False;
      Result := Chk1;
    end;
  end;

  procedure MergeTests(BaseTst, MergeTst: TProblemTestList; var Success: boolean);
  var
    I: integer;
  begin
    // check for conflicts
    if BaseTst.Count = MergeTst.Count then
    begin
      for I := 0 to BaseTst.Count - 1 do
        if not BaseTst[I].Equals(MergeTst[I]) then
        begin
          Success := False;
          Break;
        end;
    end
    else if (BaseTst.Count <> 0) and (MergeTst.Count <> 0) then
      Success := False;
    // add tests from MergeTst that don't exist in BaseTst

    // TODO: Impement it !!!

  end;

begin
  Result := True;
  with FProperties do
  begin
    InputFile := MergeStr(InputFile, Props.InputFile, Result);
    OutputFile := MergeStr(OutputFile, Props.OutputFile, Result);
    TimeLimit := MergeInt(TimeLimit, Props.TimeLimit, Result);
    MemoryLimit := MergeInt(MemoryLimit, Props.MemoryLimit, Result);
    Checker := CloneChecker(MergeChecker(Checker, Props.Checker, Result));
    MergeTests(TestList, Props.TestList, Result);
  end;
end;

procedure TProblemPropsCollector.Finalize;
begin
  with FProperties do
  begin
    // TODO: Implement it !!!
  end;
end;

constructor TProblemPropsCollector.Create;
begin
  FProperties := CleanProperties;
end;

destructor TProblemPropsCollector.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

end.
