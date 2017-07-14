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
unit srcviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms,
  ButtonPanel, AvgLvlTree, SynHighlighterCpp, SynEditHighlighter, strconsts,
  LazUTF8, Graphics, StdCtrls, baseforms;

type
  ESourceViewer = class(Exception);

  { TSourceViewer }

  TSourceViewer = class(TBaseForm)
    ButtonPanel1: TButtonPanel;
    SourceViewer: TSynEdit;
    SynCppSyn: TSynCppSyn;
    SynFreePascalSyn: TSynFreePascalSyn;
    procedure FormShow(Sender: TObject);
  private
    FFileName: string;
    FHighlighters: TStringToPointerTree;
    procedure SetFileName(AValue: string);
  protected
    procedure RegisterHighligher(const Extension: string;
      AHighlighter: TSynCustomHighlighter);
  public
    property FileName: string read FFileName write SetFileName;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SourceViewer: TSourceViewer;

implementation

{$R *.lfm}

{ TSourceViewer }

procedure TSourceViewer.FormShow(Sender: TObject);
begin
  // fix for a small bug: by some reason, sometimes ScrollBars aren't shown
  // after testing stops.
  SourceViewer.ScrollBars := ssNone;
  SourceViewer.ScrollBars := ssBoth;
end;

procedure TSourceViewer.SetFileName(AValue: string);
var
  Extension: string;
begin
  //if FFileName = AValue then
  //  Exit;
  Extension := ExtractFileExt(AValue);
  if not FHighlighters.Contains(Extension) then
    raise ESourceViewer.CreateFmt(SSourceNotSupported, [Extension]);
  SourceViewer.Lines.LoadFromFile(AValue);
  FFileName := AValue;
  SourceViewer.Highlighter := TSynCustomHighlighter(FHighlighters[Extension]);
end;

procedure TSourceViewer.RegisterHighligher(const Extension: string;
  AHighlighter: TSynCustomHighlighter);
begin
  FHighlighters[Extension] := AHighlighter;
end;

constructor TSourceViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHighlighters := TStringToPointerTree.Create(False);
  RegisterHighligher('.pas', SynFreePascalSyn);
  RegisterHighligher('.pp', SynFreePascalSyn);
  RegisterHighligher('.c', SynCppSyn);
  RegisterHighligher('.cpp', SynCppSyn);
  RegisterHighligher('.c11', SynCppSyn);
end;

destructor TSourceViewer.Destroy;
begin
  FreeAndNil(FHighlighters);
  inherited Destroy;
end;

end.
