(require 'html-helper)

(setq version "0.04")
(setq dirname (concat "jde-jcb-" version))
(setq zipname (concat dirname ".zip"))
(setq date "2000-12-05")

(h-doc
 "javabrowser.html"
 "Java Class Browser for Emacs"
 (h-h2 "Java Class Browser (JCB) for Emacs")
 (h-p "Updated: " date)
 (h-p (h-b "Download the latest version: " (h-link zipname)))

 (h-p (h-b "This project is hosted by " (h-link "http://sourceforge.net/projects/jcb" (h-img "http://sourceforge.net/sflogo.php?group_id=jcb&type=1" "width='88' height='31' border='0' alt='SourceForge Logo'"))))
 (h-p (h-b "The CVS tree can be accessed " (h-link "http://cvs.sourceforge.net/cgi-bin/cvsweb.cgi/?cvsroot=jcb" "here") "."))

(h-h3 "About")
 (h-p "JCB is a Java class browser for Emacs. It can be used to browse directories of Java source files and browse methods in the classes. It's not limited to just Java source files, you can browse any type of source file. Currently only Java, C and Lisp files is parsed for methods though.")

 (h-h3 "Dependencies")
 (h-p "JCB requires version 1.2.x of " (h-link "http://www.ultranet.com/~zappo/semantic.shtml" "Eric Ludlam's semantic bovinator") " (version 1.3 will be supported in the next version). To parse Java files, and to do any serious Java development in Emacs, you also need version 2.1.9 or higher of " (h-link "http://sunsite.auc.dk/jde/" "JDE") ".")

 (h-h3 "Installation and Setup")
 (h-p "1. Download and unzip the latest version of JCB.")
 (h-p "2. Put the directory '" dirname "' in your Emacs load path.")
 (h-p "3. Add \"(require 'jde-jcb)\" to your .emacs file.")
 (h-p "4. Optionally bind a key to your favorite JCB layout (0-8). This is my setup:" h-br "(global-set-key [f1] '(lambda() (interactive) (jde-jcb-set-layout 0 t) (split-window-horizontally)))")
 (h-p "5. Re-start Emacs and press the key you chose. If you get an error when setting the layout, try making your Emacs window bigger or choose a \"smaller\" layout (for example 1). Your Emacs frame should now look something like this: " (h-link "jcb.png" "JCB screen capture") ".")
 (h-p "6. Select the '*JCB Packages*' window (usually top-left) and press F2. This will open the customization buffer for JCB.")
 (h-p "7. Add the paths to your source files under 'Jde Jcb Source Path'.")
 (h-p "8. Save the settings. Done!")
 
 (h-h3 "Usage")
 
 (h-h4 "Packages Buffer")
 (h-p "Select packages (directories), and if enabled class files, in the \"*JCB Packages*\" buffer by clicking the left mouse button on the package name or by hitting ENTER/RETURN when the cursor is placed on the item line. Package names with a \"[+]\" symbol after them can be expanded/collapsed by left-clicking on the symbol or by pressing the TAB key when the cursor is placed on the package line. Right clicking on a package or class name will open a popup menu where different operations on the item under the mouse cursor can be performed.")
 (h-p "Pressing F1 in the packages buffer will update it. Pressing F2 will open the jde-jcb customization group in the edit window.")

 (h-h4 "Classes and History Buffer")
 (h-p "Class files can be select by clicking the left mouse button or hitting ENTER/RETURN on the class row in the \"*JCB Classes*\" or \"*JCB History*\" windows. Clicking on the class with the middle mouse button will open the class file in another window. Right clicking on a class row will open a popup menu where different operation on the class under the mouse cursor can be performed.")
 
 (h-h4 "Methods Buffer")
 (h-p "The \"*JCB Methods*\" buffer contains the methods in the selected class file. Currently only Java, C and Lisp files are parsed for methods. When a method is selected with the left mouse button or ENTER/RETURN the edit buffer will jump to the method. Right clicking on a method will jump to the method in another window.")

 (h-h3 "Emacs Tips")
 (h-p "It's easier to navigate and scroll the JCB buffers if you install " (h-link "http://www.deja.com/getdoc.xp?AN=610250245&fmt=raw" "follow-mouse.el") " and activate your " (h-link "ftp://ftp.cs.indiana.edu/pub/elisp/mwheel.el" "wheel mouse") " in Emacs.")
 
 (h-h3 "Download")
 (h-p "The latest version: " (h-link zipname))
 (h-p "Older versions:" h-br
      (h-link "jde-jcb-0.03.zip") h-br
      (h-link "jde-jcb-0.02.zip") h-br
      (h-link "jde-jcb-0.01.zip"))

 (h-h3 "Comments, Suggestions etc.")
 (h-p "Please feel free to contact me if you find the browser useful, if you have found some bugs or if you some suggestions on how to improve it.\n"
      (h-email "mayhem@home.se" "Jesper Nordenberg"))

 (h-p "The page's WebCounter count says that you are visitor number "
      (h-img "http://counter.digits.com/wc/-d/4/javabrowser" "ALIGN=middle WIDTH=60 HEIGHT=20 BORDER=0 HSPACE=4 VSPACE=2")
      " since 2000-07-28.")
 )