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
unit checkereditorbase;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Controls, problemprops, AvgLvlTree, strconsts;

type
  ECheckerEditor = class(Exception);

  { TCheckerEditor }

  TCheckerEditor = class
  protected
    function GetControl: TControl; virtual; abstract;
    function GetChecker: TProblemChecker; virtual; abstract;
    procedure SetChecker(AValue: TProblemChecker); virtual; abstract;
  public
    property Control: TControl read GetControl;
    property Checker: TProblemChecker read GetChecker write SetChecker;
    constructor Create; virtual;
  end;

  TCheckerEditorClass = class of TCheckerEditor;

procedure SetEditor(AChecker: TProblemCheckerClass; AEditor: TCheckerEditorClass);
function GetEditor(AChecker: TProblemCheckerClass): TCheckerEditorClass;

implementation

var
  EditorMap: TPointerToPointerTree;

procedure SetEditor(AChecker: TProblemCheckerClass; AEditor: TCheckerEditorClass);
begin
  EditorMap[AChecker] := AEditor;
end;

function GetEditor(AChecker: TProblemCheckerClass): TCheckerEditorClass;
begin
  if not EditorMap.Contains(AChecker) then
    raise ECheckerEditor.Create(SCheckerEditorNotFound);
  Result := TCheckerEditorClass(EditorMap[AChecker]);
end;

{ TCheckerEditor }

constructor TCheckerEditor.Create;
begin
end;

initialization
  EditorMap := TPointerToPointerTree.Create;

finalization
  FreeAndNil(EditorMap);

end.
