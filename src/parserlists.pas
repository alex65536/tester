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
unit parserlists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, propsparserbase, problemprops, fgl;

type
  TPropertiesParserClassList = specialize TFPGList<TPropertiesParserClass>;

  TPropertiesParserStatus = (ppNone, ppOK, ppParserFail, ppMergeConflicts, ppNotFullInfo, ppTerminated);

  { TPropertiesParserList }

  TPropertiesParserList = class
  private
    FItems: TPropertiesParserClassList;
    FCollector: TProblemPropsCollector;
    FIsTerminated: boolean;
    function GetCount: integer;
    function GetParsers(I: integer): TPropertiesParserClass;
    function GetProperties: TProblemProperties;
  public
    property Properties: TProblemProperties read GetProperties;
    property IsTerminated: boolean read FIsTerminated;
    property Items: TPropertiesParserClassList read FItems;
    property Parsers[I: integer]: TPropertiesParserClass read GetParsers;
    property Count: integer read GetCount;
    procedure AddParser(AClass: TPropertiesParserClass);
    procedure Terminate;
    function Run: TPropertiesParserStatus;
    constructor Create;
    destructor Destroy; override;
  end;

  { TPropertiesParserThread }

  TPropertiesParserThread = class(TThread)
  private
    FList: TPropertiesParserList;
    FStatus: TPropertiesParserStatus;
  protected
    procedure Execute; override;
  public
    property List: TPropertiesParserList read FList;
    property Status: TPropertiesParserStatus read FStatus;
    procedure Terminate;
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize);
    destructor Destroy; override;
  end;

implementation

{ TPropertiesParserThread }

procedure TPropertiesParserThread.Execute;
begin
  FStatus := FList.Run;
end;

procedure TPropertiesParserThread.Terminate;
begin
  FList.Terminate;
end;

constructor TPropertiesParserThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FList := TPropertiesParserList.Create;
  FStatus := ppNone;
  inherited;
end;

destructor TPropertiesParserThread.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

{ TPropertiesParserList }

function TPropertiesParserList.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TPropertiesParserList.GetParsers(I: integer): TPropertiesParserClass;
begin
  Result := FItems[I];
end;

function TPropertiesParserList.GetProperties: TProblemProperties;
begin
  if FCollector = nil then
    Result := nil
  else
    Result := FCollector.Properties;
end;

procedure TPropertiesParserList.AddParser(AClass: TPropertiesParserClass);
begin
  FItems.Add(AClass);
end;

procedure TPropertiesParserList.Terminate;
begin
  FIsTerminated := True;
end;

function TPropertiesParserList.Run: TPropertiesParserStatus;
var
  I: integer;
  MergeConflicts: boolean;
  NotFullInfo: boolean;
  ParserFail: boolean;
  AParser: TPropertiesParserBase;
begin
  FIsTerminated := False;
  try
    // init
    FreeAndNil(FCollector);
    FCollector := TProblemPropsCollector.Create;
    Result := ppTerminated;
    MergeConflicts := False;
    NotFullInfo := False;
    ParserFail := False;
    // launch parsers from the list
    for I := 0 to FItems.Count - 1 do
    begin
      AParser := FItems[I].Create;
      try
        // parse
        if AParser.Parse then
        begin
          // merge
          if not FCollector.Merge(AParser.Properties) then
            MergeConflicts := True;
        end
        else
          ParserFail := True;
      finally
        FreeAndNil(AParser);
      end;
      if IsTerminated then
        Exit;
    end;
    // finalize collector
    if not FCollector.Finalize then
      NotFullInfo := True;
    // determine result
    if ParserFail then
      Result := ppParserFail
    else if NotFullInfo then
      Result := ppNotFullInfo
    else if MergeConflicts then
      Result := ppMergeConflicts
    else
      Result := ppOK;
  finally
    FIsTerminated := True;
  end;
end;

constructor TPropertiesParserList.Create;
begin
  FCollector := nil;
  FItems := TPropertiesParserClassList.Create;
end;

destructor TPropertiesParserList.Destroy;
begin
  FreeAndNil(FCollector);
  FreeAndNil(FItems);
  inherited Destroy;
end;

end.
