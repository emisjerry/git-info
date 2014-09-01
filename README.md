##README

There's no "git info" command, so i decide to write one for myself, a Git newbie.

Written in Free Pascal via [Lazarus IDE v1.2.4](http://www.lazarus.freepascal.org/).

Windows only currently.

###Setup
1. Copy git-info.exe into **<Git install folder>\libexec\git-core**. We can use "**git info**" to execute this command from now on.
2. After run "git info", git-info.ini will be generated in git-core folder, youn can modify git-info.ini to statisfy your own needs.
3. gitinfo.exe will generated a **git-prompt.bat** in your Windows temporary folder, you can change the DOS-box prompt by executing git-prompt.bat. You can change the folder of git-prompt.bat in git-info.ini.

###Usage
```batchfile
    git info
    git info && %TEMP%\git-prompt.bat
    git info && git-prompt.bat
    git info -?
```

###git-info.ini sample:
```INI
[Command]
  ;; You can add up to 99 cmd pairs, from cmd1.xxx to cmd99.xxx
  cmd1.title=====Remote URL:
  cmd1.exec=git remote -v
  cmd2.title=====All branches:
  cmd2.exec=git branch -a
  cmd3.title=====Recent commits:
  cmd3.exec=git log --pretty=format:"%C(yellow)%h %C(cyan)%ai %C(bold green)[%cn]%C(bold red)%d %C(bold green)%s%C(reset)" -10  --abbrev-commit --abbrev=4 
  cmd4.title=====Status:
  cmd4.exec=git status
  
[Prompt]
  DefaultFG=light green
  DefaultBG=black
  HighlightFG=light yellow
  HighlightBG=black
  PromptBatch=d:\util\git-prompt.bat
```

###Version info
* v0.01 2014/09/01 Initial version
