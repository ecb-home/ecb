@echo off
REM This batchfile byte-compiles the ECB lisp files.

REM $Id: make.bat,v 1.22 2003/01/29 14:36:39 berndl Exp $

REM Make sure you byte-compile ECB with the semantic-and eieio-version you
REM load into (X)Emacs (see below)!

REM =======================================================================
REM user configurable section

REM Define here the correct paths to your (X)Emacs-executable and the
REM required packages (use always FORWARD SLASHES in the paths!)

set EMACS=C:/Programme/emacs-21/bin/emacs.exe
set SEMANTIC=../semantic
set EIEIO=../eieio

REM Call "make" to byte-compile the ECB.
REM If there are any warning messages during byte-compilation (normally
REM there aren't any) you can savely ignore them!

REM end of user configurable section
REM =======================================================================


REM Do not change anything below!

echo Byte-compiling ECB with LOADPATH= %SEMANTIC% %EIEIO%

if exist ecb-compile-script-init del ecb-compile-script-init
if exist ecb.elc del *.elc

echo (add-to-list 'load-path nil) > ecb-compile-script-init
echo (add-to-list 'load-path "%SEMANTIC%") >> ecb-compile-script-init
echo (add-to-list 'load-path "%EIEIO%") >> ecb-compile-script-init
echo (require 'ecb) >> ecb-compile-script-init
echo (setq debug-on-error t) >> ecb-compile-script-init

%EMACS% -batch -no-site-file -l ecb-compile-script-init --eval "(ecb-byte-compile t)"

del ecb-compile-script-init

REM End of make.bat
