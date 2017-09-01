{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ts_testerutil;

interface

uses
  fcutils, logfile, processfork, randomname, strconsts, testerfileutil, 
  versioninfo, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ts_testerutil', @Register);
end.
