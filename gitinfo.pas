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
    _VERSION:String = '0.1 2014/09/01';

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

{ URL: http://ascii-table.com/ansi-escape-sequences.php
Esc[Value;...;Valuem

Text attributes
0	All attributes off
1	Bold on
4	Underscore (on monochrome display adapter only)
5	Blink on
7	Reverse video on
8	Concealed on

Foreground colors
30	Black
31	Red
32	Green
33	Yellow
34	Blue
35	Magenta
36	Cyan
37	White

Background colors
40	Black
41	Red
42	Green
43	Yellow
44	Blue
45	Magenta
46	Cyan
47	White
}
function getEscapeColor(sColor: String; sType: String): String;
var _iPos: Integer;
  _sColor: String;
  _sBold, _sType: String;
begin
  sColor := LowerCase(sColor);
  _iPos := Pos(' ', sColor);
  _sBold := '0';
  if (_iPos > 0) then begin
    _sBold := Copy(sColor, 1, _iPos-1);
    if (_sBold = 'light') then _sBold := '1'
    else _sBold := '0';
  end;
  _sColor := Copy(sColor, _iPos+1, 20);
  if (sType = 'FG') then _sType := '3' else _sType := '4';
  //writeln('color=' + _sColor + ',bold=' + _sBold);

  if (_sColor = 'black') then begin
    _sColor := '0';
  end else if (_sColor = 'red') then begin
    _sColor := '1';
  end else if (_sColor = 'green') then begin
    _sColor := '2';
  end else if (_sColor = 'green') then begin
    _sColor := '2';
  end else if (_sColor = 'yellow') then begin
    _sColor := '3';
  end else if (_sColor = 'blue') then begin
    _sColor := '4';
  end else if (_sColor = 'magenta') then begin
    _sColor := '5';
  end else if (_sColor = 'cyan') then begin
    _sColor := '6';
  end else begin // white
    _sColor := '7';
  end;
  if (sType = 'FG') then Result := _sBold + ';' + _sType + _sColor
  else Result := ';' + _sType + _sColor;
end;

function exec(sTitle: String; sCmd: String; s: String): String;
var _sTitle, _sCmd, _sSubCmd, _sParam, _sRunResult: String;
  _iPos, i: Integer;
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
  if RunCommand(_sCmd, [_sSubCmd, _sParam], _sRunResult) then s := Concat(s, _sRunResult);
  Result := Concat(s, #13#10);
end;

procedure TMyApplication.DoRun;
var _iPos, i: Integer;
  _sBranchName, _sCurrentDir, _sText, _sExeFileDir: String;
  _sTempDir, _sDefaultFGColor, _shighlightFGColor: String;
  _sDefaultBGColor, _shighlightBGColor, _sParam: String;
  _sTitle, _sCmd, _sPrompt: String;
  s, _sResult, _sPretty, _sMaxCount, _sBatchFile: AnsiString;
  _oFileHEAD : TextFile;
  _oFileBatch: TextFile;
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
    _oIni.WriteString('Prompt', 'DefaultFG', 'light green');
    _oIni.WriteString('Prompt', 'DefaultBG', 'black');
    _oIni.WriteString('Prompt', 'HighlightFG', 'light yellow');
    _oIni.WriteString('Prompt', 'HighlightBG', 'black');
    _oIni.WriteString('Prompt', 'PromptBatch', 'd:\util\git-prompt.bat');
  end;

  _sResult := '';

  for i:=1 to 99 do begin
    _sTitle := _oIni.ReadString('Command', 'cmd' + IntToStr(i) + '.title', '');
    if (_sTitle = '') then break;

    _sCmd := _oIni.ReadString('Command', 'cmd' + IntToStr(i) + '.exec', '');
    s := exec(_sTitle, _sCmd, s);
    _sResult := Concat(_sResult, s);
  end;

  AssignFile(_oFileHEAD, '.git/HEAD');
  FileMode := fmOpenRead;
  Reset(_oFileHEAD);

  ReadLn(_oFileHEAD, _sText);
  CloseFile(_oFileHEAD);

  _iPos := Pos('heads/', _sText);
  if (_iPos > 0) then begin
    _sDefaultFGColor := _oIni.ReadString('Prompt', 'DefaultFG', 'white');
    _sDefaultFGColor := getEscapeColor(_sDefaultFGColor, 'FG');
    _sHighlightFGColor := _oIni.ReadString('Prompt', 'HighlightFG', 'white');
    _sHighlightFGColor := getEscapeColor(_sHighlightFGColor, 'FG');

    _sDefaultBGColor := _oIni.ReadString('Prompt', 'DefaultBG', 'black');
    _sDefaultBGColor := getEscapeColor(_sDefaultBGColor, 'BG');
    _sHighlightBGColor := _oIni.ReadString('Prompt', 'HighlightBG', 'black');
    _sHighlightBGColor := getEscapeColor(_sHighlightBGColor, 'BG');

    _sTempDir := GetEnv('TEMP');
    _sBatchFile := _oIni.ReadString('Prompt', 'PromptBatch', _sTempDir + '\git-prompt.bat');
    AssignFile(_oFileBatch, _sBatchFile);
    ReWrite(_oFileBatch);

    _sBranchName := Copy(_sText, _iPos+6, 99);
    _sPrompt := '@prompt $p ($E[' + _sHighlightFGColor + _sHighlightBGColor + 'm' +
         _sBranchName + '$E[' + _sDefaultFGColor + _sDefaultBGColor + 'm)$g';

    s := '@echo off' + #13#10 +
         'if exist .git\HEAD goto GIT' + #13#10 +
         'goto NOT_GIT' + #13#10 +
         ':GIT' + #13#10 +
         '  ' + _sPrompt + #13#10 +
         '  goto END' + #13#10 +
         ':NOT_GIT' + #13#10 +
         '  prompt $p$g' + #13#10 +
         ':END' + #13#10;
    Writeln(_oFileBatch, s);
    CloseFile(_oFileBatch);

    _sResult := Concat(_sResult, '===Notes:'#13#10);
    _sResult := Concat(_sResult, 'Change prompt batch: ' + _sBatchFile);
    _sResult := Concat(_sResult, #13+#10+'You can use [git info && git-prompt] to execute'#13#10);
    _sResult := Concat(_sResult, 'Help: git info -?');
  end;
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

