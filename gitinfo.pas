{
  gitinfo: Git info utility
  Author: Jerry Jian (emisjerry@gmail.com)
}
program gitinfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Windows, IniFiles, Dos, Process
  { you can add units after this };

type

  { TMyApplication }
  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  const
    _DEBUG: Boolean = true;
    _VERSION:String = '0.2 2014/09/02';

{ TMyApplication }

function help(): Boolean;
begin
  writeln('gitinfo - Git info utility v' + _VERSION + ' [written by Jerry Jian (emisjerry@gmail.com)]');
  writeln('');
  writeln('Display the information of working directory, as "svn info".');
  writeln('Put git-info.exe to <Git innstall folder>\libexec\git-core will make git-info.exe as a Git command.');
  writeln('git-info.exe will generate a git-prompt.bat which changes the prompt of your DOS-box.');
  writeln('');
  writeln('Settings file (git-info.ini) can be found in the git-info.exe''s folder.');
  Result := true;
end;

function exec(sTitle: String; sCmd: String; s: String): String;
var _sCmd, _sSubCmd, _sParam, _sRunResult: String;
  _iPos: Integer;
begin
  s := sTitle + #13#10;
  _iPos := Pos(' ', sCmd);
  if (_iPos > 0) then begin
    _sCmd := Copy(sCmd, 1, _iPos-1);
    _sParam := Copy(sCmd, _iPos+1, 1024);
    _iPos := Pos(' ', _sParam);
    if (_iPos > 0) then begin
      _sSubCmd := Copy(_sParam, 1, _iPos-1);
      _sParam := Copy(_sParam, _iPos+1, 1024);
    end else begin
      _sSubCmd := _sParam;
      _sParam := '';
    end;
  end else begin
    _sCmd := sCmd;
    _sSubCmd := '';
    _sParam := '';
  end;
  _sRunResult := '';
  if RunCommand(_sCmd, [_sSubCmd, _sParam], _sRunResult) then s := Concat(s, _sRunResult);
  Result := Concat(s, #13#10);
end;

procedure TMyApplication.DoRun;
var i: Integer;
  _sParam, _sExeFileDir, _sCurrentDir, _sTitle, _sCmd: String;
  s, _sResult: AnsiString;
  _oIni: TIniFile;
begin
  { add your program here }
  _sParam := '';
  if (ParamCount = 1) then _sParam := ParamStr(1);
  if (_sParam = '-?') or (_sParam = '-help') then begin
    help();
    Terminate;
    Exit;
  end;

  _sExeFileDir := ExtractFilePath(ExeName);
  _sCurrentDir := GetCurrentDir();

  if not FileExists('.git/HEAD') then begin
    WriteLn('This folder is not a Git working directory.');
    Terminate;
    Exit;
  end;
  _oIni := TIniFile.Create(_sExeFileDir + 'git-info.ini');
  if not FileExists(_sExeFileDir + 'git-info.ini') then begin
    _oIni.WriteString('Command', 'cmd1.title', '===Remote URL:');
    _oIni.WriteString('Command', 'cmd1.exec', 'git remote -v');
    _oIni.WriteString('Command', 'cmd2.title', '===All branches:');
    _oIni.WriteString('Command', 'cmd2.exec', 'git branch -a');
    _oIni.WriteString('Command', 'cmd3.title', '===Recent commits:');
    _oIni.WriteString('Command', 'cmd3.exec', 'git log --pretty=format:"%C(yellow)%h %C(cyan)%ai %C(bold green)[%cn]%C(bold red)%d %C(bold green)%s%C(reset)" -10  --abbrev-commit --abbrev=4');
    _oIni.WriteString('Command', 'cmd4.title', '===Current Status:');
    _oIni.WriteString('Command', 'cmd4.exec', 'git status');
  end;

  _sResult := '';

  for i:=1 to 99 do begin
    _sTitle := _oIni.ReadString('Command', 'cmd' + IntToStr(i) + '.title', '');
    if (_sTitle = '') then break;

    _sCmd := _oIni.ReadString('Command', 'cmd' + IntToStr(i) + '.exec', '');
    s := '';
    s := exec(_sTitle, _sCmd, s);
    _sResult := Concat(_sResult, s);
  end;

  _sResult := Concat(_sResult, '===Notes:'#13#10);
  _sResult := Concat(_sResult, 'Help: git info -?');
  _oIni.Free;
  Writeln(_sResult);

  Terminate;
  Exit;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  {$IFDEF WINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

