@echo off
REM This batchfile byte-compiles the ECB lisp files.

REM Make sure you byte-compile ECB with the semantic-and eieio-version you
REM load into (X)Emacs (see below)!

REM =======================================================================
REM user configurable section

REM Define here the correct paths to your (X)Emacs-executable and the
REM required packages (use always forward slashes in the paths!)

set EMACS=D:/Programme/Tools/Editor/emacs-21.2/bin/emacs.exe
set SEMANTIC=../semantic
set EIEIO=../eieio
set JDE=../jde/lisp

REM Call "make" to byte-compile the ECB. You can savely ignore the messages.

REM end of user configurable section
REM =======================================================================


REM Do not change anything below!


set EL=tree-buffer.el ecb-util.el ecb-mode-line.el ecb-help.el ecb-layout.el ecb-navigate.el ecb.el ecb-eshell.el ecb-cycle.el ecb-face.el ecb-compilation.el ecb-upgrade.el

echo Byte-compiling ECB with LOADPATH= %SEMANTIC% %EIEIO% %JDE% 

if exist ecb-compile-script-init del ecb-compile-script-init
if exist ecb.elc del *.elc

echo (add-to-list 'load-path nil) > ecb-compile-script-init
echo (add-to-list 'load-path "%SEMANTIC%") >> ecb-compile-script-init
echo (add-to-list 'load-path "%EIEIO%") >> ecb-compile-script-init
echo (add-to-list 'load-path "%JDE%") >> ecb-compile-script-init

echo (setq debug-on-error t) >> ecb-compile-script-init
%EMACS% -batch -no-site-file -l ecb-compile-script-init -f batch-byte-compile %EL%

del ecb-compile-script-init

REM End of make.bat
