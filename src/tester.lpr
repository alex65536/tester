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
program tester;

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
  cmem, {$ENDIF} {$ENDIF}
  heaptrc,
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  Classes,
  mainunit,
  problemprops,
  testresults,
  propseditor,
  testsdlg,
  lazcontrols,
  jsonsaver,
  checkers,
  stdexecheckeredit,
  checkereditors,
  checkerselector,
  compilers,
  srcviewer,
  processfork,
  runtimers,
  problemtesting,
  testerprimitives,
  randomname,
  testerframes,
  compilerinfo,
  verdictcolors,
  testinfo,
  multitesters,
  testerforms,
  about,
  licenseforms,
  imgkeeper,
  testtemplates,
  testtemplatedlg,
  logfile,
  propsparserbase,
  parserlists,
  parserforms,
  parser_alltest,
  testerfileutil,
  textcheckedit,
  parser_xml,
  versioninfo,
  parser_simplecfg,
  parser_findfile,
  editcostsdlg,
  htmlexport,
  baseforms,
  parserutils,
  parser_ejudgecfg,
  solutioninfo,
  problemstats;

{$R *.res}

begin
  Application.Title := 'Tester';
  RequireDerivedFormResource := True;
  Application.Initialize;
  InitVersionInfo;
  Application.CreateForm(TImageKeeper, ImageKeeper);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTestDialog, TestDialog);
  Application.CreateForm(TSourceViewer, SourceViewer);
  Application.CreateForm(TTesterForm, TesterForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TLicenseForm, LicenseForm);
  Application.CreateForm(TTestTemplateDialog, TestTemplateDialog);
  Application.CreateForm(TParserForm, ParserForm);
  Application.CreateForm(TEditCostsDialog, EditCostsDialog);
  Application.Run;
end.
