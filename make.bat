@echo off
REM This batchfile compiles the ECB lisp files. It assumes that the ECB is
REM installed in the same directory as that packages that it requires, e.g.,
REM 
REM root
REM   emacs
REM 	site-lisp
REM 	  ecb
REM 	  semantic-1.3.3
REM 
REM If your installation is different, edit this batchfile to reflect the
REM actual locations of the required packages relative to the ECB lisp
REM directory.
REM 
REM Make sure you compile ECB with the semantic version you load into Emacs
REM (see below)!

REM Define here the correct path to your Emacs or XEmacs
set EMACSPROG=emacs

REM Adapted by Klaus Berndl

if exist ecb-compile-script-init del ecb-compile-script-init
if exist ecb.elc del *.elc
echo (add-to-list 'load-path nil) > ecb-compile-script-init
echo (add-to-list 'load-path "../semantic-1.3.3") >> ecb-compile-script-init
echo (setq debug-on-error t) >> ecb-compile-script-init
%EMACSPROG% -batch -no-site-file -l ecb-compile-script-init -f batch-byte-compile *.el
del ecb-compile-script-init

REM End of make.bat
