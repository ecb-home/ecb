Readme for the Emacs Code Browser (ECB) v1.10
---------------------------------------------

About
-----

This package contains a code browser for several languages for
Emacs. For instructions on how to use the browser read the ecb.el
file. The latest version of this package can be found at:

http://home.swipnet.se/mayhem/ecb.html


Requirements
------------

ECB requires version 1.2.1 or higher of Eric's semantic bovinator
(http://www.ultranet.com/~zappo/semantic.shtml).  If Java code is
edited the ECB works best when the JDE package
(http://sunsite.auc.dk/jde) is installed.


Installation
------------

1. Unpack the ECB archive.
2. Add the new directory to your load-path.
3. Add to your .emacs or site-start.el file the line:
   (require 'ecb)

Optional compilation steps:

1. Check the makefile in the ECB directory if the semantic-path is
   correct.  By default the makefile assumes that semantic is placed
   in the directory where ECB was unpacked. But you always have to
   check the correct semantic-version!

2. Call "make all" to byte-compile the ECB. You can savely ignore the
   messages.


Usage
-----

Call "M-x ecb-activate".


Contacts
--------

Send comments, bug reports and improvement suggestions to:

Jesper Nordenberg, mayhem@home.se
