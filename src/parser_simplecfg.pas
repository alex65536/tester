unit parser_simplecfg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, problemprops, checkers, propsparserbase, logfile, FileUtil,
  LazFileUtils, testerfileutil, compilers, testerprimitives;

type

  { TSimpleCfgPropertiesParser }

  TSimpleCfgPropertiesParser = class(TPropertiesParserBase)
    function DoParse: boolean; override;
  end;

implementation

{ TSimpleCfgPropertiesParser }

function TSimpleCfgPropertiesParser.DoParse: boolean;

  function Parser(ALines: TStringList): boolean;

    function DelSeparators(const S: string): string;
    const
      Letters = ['A' .. 'Z', 'a' .. 'z', '0' .. '9'];
    var
      I: integer;
    begin
      Result := '';
      for I := 1 to Length(S) do
        if S[I] in Letters then
          Result := Result + S[I];
    end;

    procedure ParseChecker(ACheckerSrc: string; var Success: boolean);
    var
      CompilerOutput: string;
      CheckerExe: string;
    begin
      ACheckerSrc := CorrectFileName(AppendPathDelim(WorkingDir) + ACheckerSrc);
      CheckerExe := AppendPathDelim(WorkingDir) + 'checker';
      {$IfDef Windows}
      CheckerExe := CheckerExe + '.exe';
      {$EndIf}
      if CompileFile(ACheckerSrc, CheckerExe, CompilerOutput) = cvSuccess then
        Properties.Checker :=
          TTestlibChecker.Create(CreateRelativePath(CheckerExe, WorkingDir))
      else
        Success := False;
    end;

  var
    I, P: integer;
    AName, AValue: string;
    TestCount: integer;
    TestDir: string;
  begin
    Result := True;
    TestCount := 0;
    for I := 0 to ALines.Count - 1 do
    begin
      // determine name and value
      P := Pos('=', ALines[I]);
      if P = 0 then
        Continue;
      AName := Copy(ALines[I], 1, P - 1);
      AValue := Copy(ALines[I], P + 1, Length(ALines[I]) - P);
      AName := LowerCase(DelSeparators(AName));
      AValue := Trim(AValue);
      // parse value by name
      try
        if AName = 'timelimit' then
          Properties.TimeLimit := StrToInt(AValue)
        else if AName = 'memorylimit' then
          Properties.MemoryLimit := StrToInt(AValue)
        else if AName = 'inputfile' then
          Properties.InputFile := 'input.txt'
        else if AName = 'outputfile' then
          Properties.OutputFile := 'output.txt'
        else if AName = 'checker' then
          ParseChecker(AValue, Result)
        else if AName = 'testscount' then
          TestCount := StrToInt(AValue);
      except
        Result := False;
      end;
      if IsTerminated then
        Break;
    end;
    TestDir := AppendPathDelim('tests');
    if not IsTerminated then
      AddTestsFmt(TestDir + '%d.in', TestDir + '%d.ans', TestCount);
  end;

var
  Lines: TStringList;
  CfgFileName: string;
begin
  Result := True;
  CfgFileName := CorrectFileName(AppendPathDelim(WorkingDir) + 'package.cfg');
  if not FileExistsUTF8(CfgFileName) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(CfgFileName);
    Result := Parser(Lines);
  finally
    FreeAndNil(Lines);
  end;
end;

end.
