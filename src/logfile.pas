unit logfile;

{$mode objfpc}{$H+}

{$Define WriteLog} // Comment this for releases!

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
procedure WriteLog(const S: string); inline;
begin
  // do nothing
end;

procedure WriteLogFmt(const S: string; Args: array of const);
begin
  // do nothing
end;
{$EndIf}

end.

