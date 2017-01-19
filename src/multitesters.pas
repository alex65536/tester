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
unit multitesters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemtesting, problemprops;

type
  TTesterUpdateKind = (ukStart, ukCompile, ukTest, ukFinish);
  TTesterUpdateEvent = procedure(Sender: TObject; TesterID: integer;
    Kind: TTesterUpdateKind) of object;
  TTesterExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  { TMultiTesterItem }

  TMultiTesterItem = class(TCollectionItem)
  private
    FTester: TProblemTester;
  public
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Tester: TProblemTester read FTester;
  end;

  { TMultiTesterList }

  TMultiTesterList = class(TCollection)
  private
    procedure SetItem(Index: integer; AValue: TMultiTesterItem);
    function GetItem(Index: integer): TMultiTesterItem;
  public
    function Add: TMultiTesterItem;
    function Insert(Index: integer): TMultiTesterItem;
    property Items[Index: integer]: TMultiTesterItem read GetItem write SetItem; default;
    constructor Create;
  end;

  TMultiTester = class(TPersistent)
  private
    FIsTerminated: boolean;
    FItems: TMultiTesterList;
    FOnFinish: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnUpdate: TTesterUpdateEvent;
    FProperties: TProblemProperties;
    FSources: TStringList;
    function GetTesterCount: integer;
    function GetTesters(I: integer): TProblemTester;
    procedure SetOnFinish(AValue: TNotifyEvent);
    procedure SetOnStart(AValue: TNotifyEvent);
    procedure SetOnUpdate(AValue: TTesterUpdateEvent);
    procedure SetProperties(AValue: TProblemProperties);
    procedure TesterStart(Sender: TObject);
    procedure TesterCompile(Sender: TObject);
    procedure TesterTest(Sender: TObject; TesterIndex: integer);
    procedure TesterFinish(Sender: TObject);
  protected
    procedure DoStart; virtual;
    procedure DoUpdate(TesterID: integer; Kind: TTesterUpdateKind); virtual;
    procedure DoFinish; virtual;
  public
    property IsTerminated: boolean read FIsTerminated;
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;
    property OnUpdate: TTesterUpdateEvent read FOnUpdate write SetOnUpdate;
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property Testers[I: integer]: TProblemTester read GetTesters;
    property TesterCount: integer read GetTesterCount;
    property Sources: TStringList read FSources;
    constructor Create;
    destructor Destroy; override;
    procedure Launch;
    procedure Terminate;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Properties: TProblemProperties read FProperties write SetProperties;
    property Items: TMultiTesterList read FItems;
  end;

  { TMultiTesterThread }

  TMultiTesterThread = class(TThread)
  private
    FExc: Exception;
    FKind: TTesterUpdateKind;
    FMultiTester: TMultiTester;
    FOnException: TTesterExceptionEvent;
    FOnFinish: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnUpdate: TTesterUpdateEvent;
    FTesterID: integer;
    procedure SetOnException(AValue: TTesterExceptionEvent);
    procedure SetOnFinish(AValue: TNotifyEvent);
    procedure SetOnStart(AValue: TNotifyEvent);
    procedure SetOnUpdate(AValue: TTesterUpdateEvent);
    procedure TesterStart(Sender: TObject);
    procedure TesterUpdate(Sender: TObject; TesterID: integer; Kind: TTesterUpdateKind);
    procedure TesterFinish(Sender: TObject);
  protected
    property TesterID: integer read FTesterID;
    property Kind: TTesterUpdateKind read FKind;
    property Exc: Exception read FExc;
    procedure DoStart; virtual;
    procedure DoUpdate; virtual;
    procedure DoFinish; virtual;
    procedure DoException; virtual;
    procedure Execute; override;
  public
    property MultiTester: TMultiTester read FMultiTester;
    property OnStart: TNotifyEvent read FOnStart write SetOnStart;
    property OnUpdate: TTesterUpdateEvent read FOnUpdate write SetOnUpdate;
    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property OnException: TTesterExceptionEvent read FOnException write SetOnException;
    constructor Create(CreateSuspended: boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    procedure Terminate;
    destructor Destroy; override;
  end;

implementation

{ TMultiTesterThread }

procedure TMultiTesterThread.SetOnFinish(AValue: TNotifyEvent);
begin
  if FOnFinish = AValue then
    Exit;
  FOnFinish := AValue;
end;

procedure TMultiTesterThread.SetOnException(AValue: TTesterExceptionEvent);
begin
  if FOnException = AValue then
    Exit;
  FOnException := AValue;
end;

procedure TMultiTesterThread.SetOnStart(AValue: TNotifyEvent);
begin
  if FOnStart = AValue then
    Exit;
  FOnStart := AValue;
end;

procedure TMultiTesterThread.SetOnUpdate(AValue: TTesterUpdateEvent);
begin
  if FOnUpdate = AValue then
    Exit;
  FOnUpdate := AValue;
end;

procedure TMultiTesterThread.TesterStart(Sender: TObject);
begin
  Synchronize(@DoStart);
end;

procedure TMultiTesterThread.TesterUpdate(Sender: TObject; TesterID: integer;
  Kind: TTesterUpdateKind);
begin
  FTesterID := TesterID;
  FKind := Kind;
  Synchronize(@DoUpdate);
end;

procedure TMultiTesterThread.TesterFinish(Sender: TObject);
begin
  Synchronize(@DoFinish);
end;

procedure TMultiTesterThread.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TMultiTesterThread.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, TesterID, Kind);
end;

procedure TMultiTesterThread.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

procedure TMultiTesterThread.DoException;
begin
  if Assigned(FOnException) then
    FOnException(Self, Exc);
end;

procedure TMultiTesterThread.Terminate;
begin
  FMultiTester.Terminate;
end;

procedure TMultiTesterThread.Execute;
begin
  try
    FMultiTester.OnStart := @TesterStart;
    FMultiTester.OnUpdate := @TesterUpdate;
    FMultiTester.OnFinish := @TesterFinish;
    FMultiTester.Launch;
  except
    on E: Exception do
    begin
      FExc := E;
      Synchronize(@DoException);
      Synchronize(@DoFinish);
    end;
  end;
end;

constructor TMultiTesterThread.Create(CreateSuspended: boolean;
  const StackSize: SizeUInt);
begin
  FMultiTester := TMultiTester.Create;
  inherited;
end;

destructor TMultiTesterThread.Destroy;
begin
  FreeAndNil(FMultiTester);
  inherited Destroy;
end;

{ TMultiTester }

function TMultiTester.GetTesterCount: integer;
begin
  Result := FItems.Count;
end;

function TMultiTester.GetTesters(I: integer): TProblemTester;
begin
  Result := FItems[I].Tester;
end;

procedure TMultiTester.SetOnFinish(AValue: TNotifyEvent);
begin
  if FOnFinish = AValue then
    Exit;
  FOnFinish := AValue;
end;

procedure TMultiTester.SetOnStart(AValue: TNotifyEvent);
begin
  if FOnStart = AValue then
    Exit;
  FOnStart := AValue;
end;

procedure TMultiTester.SetOnUpdate(AValue: TTesterUpdateEvent);
begin
  if FOnUpdate = AValue then
    Exit;
  FOnUpdate := AValue;
end;

procedure TMultiTester.SetProperties(AValue: TProblemProperties);
begin
  if FProperties = AValue then
    Exit;
  FProperties := AValue;
end;

procedure TMultiTester.TesterStart(Sender: TObject);
begin
  DoUpdate((Sender as TProblemTester).Tag, ukStart);
end;

procedure TMultiTester.TesterCompile(Sender: TObject);
begin
  DoUpdate((Sender as TProblemTester).Tag, ukCompile);
end;

procedure TMultiTester.TesterTest(Sender: TObject; TesterIndex: integer);
begin
  TesterIndex := TesterIndex; // to prevent hints
  DoUpdate((Sender as TProblemTester).Tag, ukTest);
end;

procedure TMultiTester.TesterFinish(Sender: TObject);
begin
  DoUpdate((Sender as TProblemTester).Tag, ukFinish);
end;

procedure TMultiTester.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TMultiTester.DoUpdate(TesterID: integer; Kind: TTesterUpdateKind);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, TesterID, Kind);
end;

procedure TMultiTester.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

constructor TMultiTester.Create;
begin
  FItems := TMultiTesterList.Create;
  FSources := TStringList.Create;
end;

destructor TMultiTester.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FSources);
  inherited Destroy;
end;

procedure TMultiTester.Launch;
var
  I: integer;
begin
  FIsTerminated := False;
  FItems.Clear;
  for I := 0 to FSources.Count - 1 do
    with FItems.Add do
    begin
      Tester.Properties := Properties;
      Tester.SourceFile := FSources[I];
      Tester.Tag := I;
      Tester.OnStart := @TesterStart;
      Tester.OnCompile := @TesterCompile;
      Tester.OnTest := @TesterTest;
      Tester.OnFinish := @TesterFinish;
      Tester.Prepare;
    end;
  DoStart;
  for I := 0 to FItems.Count - 1 do
  begin
    if IsTerminated then
      Break;
    FItems[I].Tester.Launch;
  end;
  DoFinish;
  FIsTerminated := True;
end;

procedure TMultiTester.Terminate;
var
  I: integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].Tester.Terminate;
  FIsTerminated := True;
end;

procedure TMultiTester.AssignTo(Dest: TPersistent);
begin
  with Dest as TMultiTester do
  begin
    Items.Assign(Self.Items);
    Sources.Assign(Self.Sources);
    Properties := Self.Properties;
  end;
end;

{ TMultiTesterList }

procedure TMultiTesterList.SetItem(Index: integer; AValue: TMultiTesterItem);
begin
  inherited SetItem(Index, AValue);
end;

function TMultiTesterList.GetItem(Index: integer): TMultiTesterItem;
begin
  Result := (inherited GetItem(Index)) as TMultiTesterItem;
end;

function TMultiTesterList.Add: TMultiTesterItem;
begin
  Result := (inherited Add) as TMultiTesterItem;
end;

function TMultiTesterList.Insert(Index: integer): TMultiTesterItem;
begin
  Result := (inherited Insert(Index)) as TMultiTesterItem;
end;

constructor TMultiTesterList.Create;
begin
  inherited Create(TMultiTesterItem);
end;

{ TMultiTesterItem }

procedure TMultiTesterItem.AssignTo(Dest: TPersistent);
begin
  with Dest as TMultiTesterItem do
  begin
    Tester.Assign(Self.Tester);
  end;
end;

constructor TMultiTesterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTester := TProblemTester.Create;
end;

destructor TMultiTesterItem.Destroy;
begin
  FreeAndNil(FTester);
  inherited Destroy;
end;

end.
