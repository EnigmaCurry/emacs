;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'jde)
(setq jde-jdk-registry (quote (("1.6.0" . "/etc/opt/java"))))
(setq jde-jdk (quote ("1.6.0")))
(setq jde-complete-function (quote jde-complete-in-line))

(add-to-list 'auto-mode-alist '("\\.js\\'" . java-mode))
(setq author "Ryan McGuire")

;; Redefine RET to do special things in special circumstances
(define-key c-mode-base-map "\C-m" 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My custom skeletons
;; (Note: Good recipe for skeletons at 
;;  http://www.panix.com/~tehom/my-code/skel-recipe.txt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put the following in your .emacs so that the
;; abbrev table is set correctly in all modes.
;; (Not just for java)
;;
;; (add-hook 'pre-abbrev-expand-hook 'abbrev-table-change)
;; (defun abbrev-table-change (&optional args)
;;   (setq local-abbrev-table
;; 	(if (eq major-mode 'jde-mode)
;; 	    (if (jde-parse-comment-or-quoted-p)
;; 		text-mode-abbrev-table
;; 	      java-mode-abbrev-table)
;; 	  (if (eq major-mode 'python-mode)
;; 	      (if (inside-comment-p)
;; 		  text-mode-abbrev-table
;; 		python-mode-abbrev-table
;; 		))))
;;   )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-abbrev java-mode-abbrev-table "psvm" "" 'java-skeleton-psvm)
(define-skeleton java-skeleton-psvm
  "Insert a main function declaration" nil
  "public static void main(String[] args){"
  \n > _
  \n "}" >
  )
(define-abbrev java-mode-abbrev-table "print" "" 'java-skeleton-println)
(define-skeleton java-skeleton-println
  "Insert a println statement" nil
  "System.out.println("_");"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  These are a bunch of adhoc skeletons I got from Spence Koehler.
;;  Very useful.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook
	  (function
	   (lambda()
	     (make-local-variable 'compile-command)
	     (setq compile-command (concat "buildone " buffer-file-name))
	     (let ((currentbuffer (current-buffer)))
	       ;(ecb-activate)
	       (switch-to-buffer currentbuffer)
	       )
	     (setq c-basic-offset 2)
	     )))

(defun java-class-skeleton (&optional arg)
  "Creates a java class skeleton in the current buffer (named *.java)"
  (interactive "P")
  (let ((path (split-string buffer-file-name "/")))
    (insert "public class ")
    ; insert the last element on the path (filename) minus the ".java" part
    (insert (substring (car (last path)) 0 -5))
    
    (add-braces)
    (execute-kbd-macro [?\C-a ?\C-p])
    (add-javadoc-comment)
    (add-package)
    (insert "import java.util.*;\nimport java.io.*;\n")
    (execute-kbd-macro [?\C-n ?\C-n ?\C-e ? ])
    ))

(defun java-test-class-skeleton (&optional arg)
  "Creates a JUnit skeleton class in the current buffer (named *.java)"
  (interactive "P")
  (let ((path (split-string buffer-file-name "/")))
    ;let name be the last element on the path (filename) minus the ".java" part
    (let ((name (substring (car (last path)) 0 -5)))
      (insert "public class " name " extends TestCase")
      (add-braces)
      (insert "\npublic " name "(String name) {\n"
              "super(name);\n"
              "}\n"
              "\n"
              "public void testX() {\n"
              "//Write your test here\n"
              "}\n"
              "\n"
              "public static Test suite() {\n"
              "TestSuite suite = new TestSuite(" name ".class);\n"
              "return suite;\n"
              "}\n"
              "\n"
              "public static void main(String[] args) {\n"
              "junit.textui.TestRunner.run(suite());\n"
              "}\n")       
      (mark-whole-buffer)
      (c-indent-line-or-region)
      (add-javadoc-comment)
      (add-package)
      (insert "import java.util.*;\nimport junit.framework.Test;\n"
              "import junit.framework.TestCase;\n"
              "import junit.framework.TestSuite;\n")
      (execute-kbd-macro [?\C-n ?\C-n ?\C-e ? ])
      )))

(defun add-braces (&optional arg)
  "Inserts a pair of braces at the current cursor position."
  (interactive "P")
  (let ((macro
	 [?\C-e ?  ?{ ?\C-m ?} ?\C-a ?\C-o ?\C-i]))
    (execute-kbd-macro macro)))

(defun add-javadoc-comment (&optional arg)
  "Inserts javadoc comments at the current cursor position."
  (interactive "P")
  ;(push-mark)
  (let ((pre-macro [?\C-i ?/ ?* ?* ?\C-m ?* ?\C-m ?* ?  ?< ?p ?> ?\C-m ?* ?  ?@ ?a ?u ?t ?h ?o ?r ?  ])
        (post-macro [?\C-m ?* ?/ ?\C-m ?\C-i]))
    (execute-kbd-macro (vconcat pre-macro author post-macro))))

(defun add-package (&optional arg)
  "Inserts a 'package' line at the top of the java file.
  NOTE: It is assumed that the package is named beginning with an 'com.' directory."
  (interactive "P")
  (let ((preMacro [?\M-< ?\C-o]))
    (execute-kbd-macro preMacro)
    (insert "package ")
    (insert (classpath2package (filepath2classpath buffer-file-name)))
    (insert ";\n\n")
    ))

(defun classpath2package (classpath)
  (let ((ppos (position-from-end ?. classpath)))
    (if ppos (substring classpath 0 (position-from-end ?. classpath)) classpath)))

(defun filepath2classpath (filepath)
  (let*
      ((sub0 (replace-regexp-in-string "/" "." filepath))
       (orgpos (string-match "com\." sub0))
       (thepos (if (null orgpos) (string-match "org\." sub0) orgpos))
       (sub1 (substring sub0 thepos))
       (sub2 (replace-regexp-in-string ".java" "" sub1)))
  sub2))

(defun position-from-end (char string)
  (let ((result nil)
        (i (length string)))
    (while (and (null result) (> i 0))
      (setq i (1- i))
      (if (eq (aref string i) char)
        (setq result i)))
    result))

(defun execute-java-file (&optional arg)
  (interactive "P")
  (let (
        (command "java -Xmx640m ")
        (test-command "java -Xmx640m junit.textui.TestRunner ")
        ;(command "j ")
	;(test-command "jt ")
        (javaclass (filepath2classpath buffer-file-name)))
    (execute-kbd-macro [?\C-c ?s ?\M->])
    (if (string-match ".Test" javaclass) (insert test-command) (insert command))
    (insert javaclass)
    ))

(defun import-classes (&optional arg)
  "Finds possible classes for the word at the current point, adding an 'import'
   statement to the java file (interactively if more than one to choose)."
  (interactive "P")
  (let* ((name (thing-at-point 'word))
         (paths-string
          ; note: relies on shell command "cpfinder name"
          (shell-command-to-string
           (concat "cpfinder " name)))
         (len (length paths-string))
         path)

    (when (> len 0)
      ; return to point where we started when done
      (save-excursion

        ; position point at end of imports or under "package"
        (unless (re-search-backward "^import " nil t)
                                        ;go to beginning of buffer and down
          (goto-char (point-min))
          (forward-line 1)
          )
        (forward-line 1)

        ; deal with possibilities
        (let ((paths (split-string paths-string "\n")))
          (setq
           path
           (if (= (length paths) 1)
                                        ; only 1: don't need to ask
               (insert-import paths "0")
                                        ; more than 1: create prompt with choices and ask
             (let ((prompt "") choice (count 0))
               (dolist (path paths)
                 (when (> (length path) 0)
                   (setq prompt (concat prompt "(" (number-to-string count) ") " path "\n"))
                   (setq count (+ count 1))))
               (setq prompt (concat prompt "choice [0-" (number-to-string (- count 1)) "]: "))
               (setq choice (read-no-blanks-input prompt))
               (insert-import paths choice))))
          ))
;      (ding)
      (momentary-string-display "" (point) nil (concat "imported " path))
      )))

(defun insert-import (paths choice)
  "auxiliary to import-classes for inserting the 'import' statement"
  (let* ((pos (string-to-number choice))
         (path (elt paths pos))
         (import (concat "import " path ";")))
    ;todo: don't import when in same package?
    ; only import if not already there
    (unless (re-search-backward (concat "^" import) nil t)
      (insert import)
      (insert "\n"))
    path))

(setq sandbox-name "/storage/subversion/sdnew/projects")

(setq sandbox-name-as-path (concat "/" sandbox-name "/"))

;; find project root
(defun find-project-root (filename &optional arg)
  "Function to find the project root for the named file."
  (let* ((sandbox-name-pos (string-match sandbox-name-as-path filename))
         (next-slash-pos (string-match "/" filename (+ sandbox-name-pos (length sandbox-name-as-path)))))
    (substring filename 0 next-slash-pos))
  )

;; goto-file macro
(defun find-source (&optional arg)
  "Macro to find the source files under the project root of the class at point."
  (interactive "P")
  (let ((pre-macro [?\M-b ?\C- ?\M-f ?\C-c ?c])
        (project-root (find-project-root buffer-file-name))
        (post-macro [?\C-c ?\C-f ?\C-m ?\C-y ?. ?j ?a ?v ?a ?\C-m]))
    (execute-kbd-macro pre-macro)
    (find-file-other-window project-root)
    (execute-kbd-macro post-macro)))
                                        ;(fset 'find-source
                                        ; '[?\M-b ?\C- ?\M-f ?\C-c ?c ?\C-x ?4 ?f ?~ ?/ ?c ?o ?/ ?s ?e ?a ?r ?c ?h ?- ?s ?u ?b ?s ?y ?s ?\C-m ?\C-c ?\C-f ?\C-m ?\C-y ?. ?j ?a ?v ?a ?\C-m])

;; find-usages macro
(defun find-usages (&optional arg)
  "Macro to find usages of the current symbol at point."
  (interactive "P")
  (let ((pre-macro [?\M-b ?\C- ?\M-f ?\C-c ?c])
        (project-root (find-project-root buffer-file-name))
        (post-macro [?\C-c ?f ?\C-m ?\C-y ?\C-m]))
    (execute-kbd-macro pre-macro)
    (find-file-other-window project-root)
    (execute-kbd-macro post-macro)))
                                        ;(fset 'find-usages
                                        ; '[?\M-b ?\C- ?\M-f ?\C-c ?c ?\C-x ?4 ?f ?~ ?/ ?c ?o ?/ ?s ?e ?a ?r ?c ?h ?- ?s ?u ?b ?s ?y ?s ?\C-m ?\C-c ?f ?\C-m ?\C-y ?\C-m])





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-cb" 'compile)          ;C-c b  <=>  compile (build)
(global-set-key "\C-ce" 'execute-java-file)
(global-set-key "\C-cs" 'shell)            ;C-c s  <=>  M-x shell
(global-set-key "\C-cc" 'clipboard-kill-ring-save)  ;C-c c  <=>  copy
(global-set-key [f7] 'compile)             ;<F7>   <=>  M-x compile
(global-set-key "\C-cC" 'clipboard-kill-ring-save)  ;C-c C <=> copy
(global-set-key "\C-cl" 'goto-line)        ;C-c l  <=> goto-line
(global-set-key "\C-cq" 'query-replace-regexp) ;C-c q <=> query-replace-regexp
(global-set-key "\C-cr" 'replace-string)   ;C-c r  <=> replace-string
(global-set-key "\C-cf" 'find-grep-dired)  ;C-c g  <=> find-grep-dired
(global-set-key "\C-c\C-f" 'find-name-dired)  ;C-c C-F  <=> find-name-dired
(global-set-key "\C-cF" 'grep-find)        ;C-c F  <=> grep-find
(global-set-key "\C-cg" 'grep)             ;C-c g  <=> grep
(global-set-key "\C-cm" 'count-matches)    ;C-c m  <=> count-matches
(global-set-key "\C-ck" 'global-set-key)   ;C-c k  <=> global-set-key
(global-set-key "\C-cd" 'speedbar)
(global-set-key "\C-c\C-d" 'ediff-buffers) ;C-c C-d <=> ediff-buffers
(global-set-key "\C-c\C-i" 'indent-region)
(global-set-key "\C-cu" 'rename-uniquely)
(global-set-key "\C-c\C-l" 'run-lisp)
(global-set-key "\C-cB" 'browse-url-lynx-emacs)
(global-set-key "\C-c\C-j" 'show-only-java)
(global-set-key "\C-cj" 'add-javadoc-comment)
(global-set-key "\C-ci" 'add-braces)
(global-set-key "\C-cp" 'add-package)
(global-set-key "\C-cJ" 'java-class-skeleton)
(global-set-key "\C-cT" 'java-test-class-skeleton)
(global-set-key "\C-c-SPC" 'jde-complete-menu)
(global-set-key "\C-cG" 'find-source)   ;C-c G <=> goto file
(global-set-key "\C-cU" 'find-usages)
