(require 'html-helper)

(setq version "1.20")
(setq dirname (concat "ecb-" version))
(setq zipname (concat dirname ".zip"))
(setq download-url "http://ftp1.sourceforge.net/ecb/")
(setq url (concat "http://ftp1.sourceforge.net/ecb/" zipname))
(setq date "2001-04-28")

(h-doc
 "ecb.html"
 "ECB - Emacs Code Browser"
 (h-h2 "ECB - Emacs Code Browser")
 (h-p "Updated: " date)
 (h-p (h-b "Download the latest version: " (h-link url zipname)))

 (h-p (h-b "This project is hosted by " (h-link "http://sourceforge.net/projects/ecb" (h-img "http://sourceforge.net/sflogo.php?group_id=17484&type=1" "width='88' height='31' border='0' alt='SourceForge Logo'"))))
 (h-p (h-b "The CVS tree can be accessed " (h-link "http://cvs.sourceforge.net/cgi-bin/cvsweb.cgi/?cvsroot=ecb" "here") "."))

 (h-h3 "About")
 (h-p "ECB is source code browser for Emacs. It displays a few windows that can be used to browse directories, files and methods. It supports method parsing for Java, C, C++, Elisp etc. " (h-link "ecb.png" "Here's") " a screenshot of the browser in action.")

 (h-h3 "Dependencies")
 (h-p "ECB requires version 1.3.3 of " (h-link "http://cedet.sourceforge.net/semantic.shtml" "Eric Ludlam's semantic bovinator") ". If you use ECB for Java development you also need version 2.1.9 or higher of " (h-link "http://sunsite.auc.dk/jde/" "JDE") ".")
 (h-p "If you use XEmacs you must have the fsf-compat package installed (contains overlay.el).")

 (h-h3 "Installation and Setup")
 (h-numbered-list
  "Download and unzip the latest version of ECB."
  (concat "Put the directory '" dirname "' in your Emacs load path.")
  "Add \"(require 'ecb)\" to your .emacs file."
  "Call \"ecb-activate\"."
  "Select the '*ECB Directories*' window (usually top-left) and press F2. This will open the customization buffer for ECB."
  "Add the paths to your source files under 'Ecb Directories' -> 'Ecb Source Path'."
  "Save the settings. Done!")
 
 (h-h3 "Usage")
 
 (h-h4 "Directories Buffer")
 (h-p "Select directories and, if enabled, source files, in the \"*ECB Directories*\" buffer by clicking the left mouse button on the package name or by hitting ENTER/RETURN when the cursor is placed on the item line. Package names with a \"[+]\" symbol after them can be expanded/collapsed by left-clicking on the symbol, pressing the TAB key when the cursor is placed on the package line or clicking the middle mouse button on the item. Right clicking on an item will open a popup menu where different operations on the item under the mouse cursor can be performed.")
 (h-p "Pressing F1 in the packages buffer will update it. Pressing F2 will open the ECB customization group in the edit window.")

 (h-h4 "Source and History Buffer")
 (h-p "Source files can be select by clicking the left mouse button or hitting ENTER/RETURN on the class row in the \"*ECB Sources*\" or \"*ECB History*\" windows. Clicking on the source file with the middle mouse button will open the class file in the other edit window. Right clicking on a source file will open a popup menu where different operation on the item under the mouse cursor can be performed.")
 
 (h-h4 "Methods Buffer")
 (h-p "The \"*ECB Methods*\" buffer contains the methods in the selected source file. When a method is selected with the left mouse button or ENTER/RETURN the edit buffer will jump to the method. Clicking on a method with the middle mouse button will jump to the method in the other edit window.")

 (h-h3 "Emacs Tips")
 (h-p "It's easier to navigate and scroll the ECB buffers if you install " (h-link "follow-mouse.el") " and activate your " (h-link "mwheel.el" "wheel mouse") " in Emacs.")
 
 (h-h3 "Download")
 (h-p "The latest version: " (h-link url zipname))
 (h-p "Older versions:" h-br
      (h-link (concat download-url "ecb-1.10.zip") "ecb-1.10.zip") h-br
      (h-link (concat download-url "ecb-1.0.zip") "ecb-1.0.zip") h-br
      (h-link "jde-jcb-0.04.zip") h-br
      (h-link "jde-jcb-0.03.zip") h-br
      (h-link "jde-jcb-0.02.zip") h-br
      (h-link "jde-jcb-0.01.zip"))

 (h-h3 "Developers")
 (h-p (h-email "mayhem@home.se" "Jesper Nordenberg") h-br
      (h-email "klaus.berndl@sdm.de" "Klaus Berndl") h-br
      (h-email "burton@apache.org" "Kevin A. Burton"))

 (h-h3 "Comments, Suggestions etc.")
 (h-p "Please feel free to contact me if you find the code browser useful, if you have found some bugs or if you some suggestions on how to improve it.\n"
      (h-email "mayhem@home.se" "Jesper Nordenberg"))

 (h-p "The page's WebCounter count says that you are visitor number "
      (h-img "http://counter.digits.com/wc/-d/4/javabrowser" "ALIGN=middle WIDTH=60 HEIGHT=20 BORDER=0 HSPACE=4 VSPACE=2")
      " since 2000-07-28.")
 )