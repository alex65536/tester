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
unit logfile;

{$mode objfpc}{$H+}

{.$Define WriteLog} // Comment this for releases!

interface

uses
  Classes, SysUtils;

procedure WriteLog(const S: string); inline;
procedure WriteLogFmt(const S: string; Args: array of const);

implementation

{$IfDef WriteLog}
uses
  DateUtils;

var
  TheLogFile: TextFile;

procedure WriteLog(const S: string); inline;
begin
  WriteLn(TheLogFile, FormatDateTime('dd.mm.yyyy hh:nn:ss.zzzz :: ', Now), S);
  Flush(TheLogFile);
end;

procedure WriteLogFmt(const S: string; Args: array of const);
begin
  WriteLog(Format(S, Args));
end;

initialization
  AssignFile(TheLogFile, 'tester.log');
  Rewrite(TheLogFile);
  WriteLog('Tester started');

finalization
  WriteLog('Tester successfully terminated');
  CloseFile(TheLogFile);

{$Else}

{$Hints Off}
procedure WriteLog(const S: string); inline;
begin
  // do nothing
end;

procedure WriteLogFmt(const S: string; Args: array of const);
begin
  // do nothing
end;
{$Hints On}

{$EndIf}

end.

