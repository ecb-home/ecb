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
REM actual locations of the required packages (use always forward-slashes as
REM directory-separator even with MS Windows systems).
REM 
REM Make sure you compile ECB with the semantic version you load into Emacs
REM (see below)!
REM
REM Call "make" to byte-compile the ECB. You can savely ignore the messages.

REM Define here the correct path to your Emacs or XEmacs
set EMACSPROG=emacs

echo Byte-compiling ECB with make.bat...

if exist ecb-compile-script-init del ecb-compile-script-init
if exist ecb.elc del *.elc
echo (add-to-list 'load-path nil) > ecb-compile-script-init

REM !!! Check this line and change it if necessary (see comments above) !!!
echo (add-to-list 'load-path "../semantic-1.4beta6") >> ecb-compile-script-init

echo (setq debug-on-error t) >> ecb-compile-script-init
%EMACSPROG% -batch -no-site-file -l ecb-compile-script-init -f batch-byte-compile *.el
del ecb-compile-script-init

REM End of make.bat
