
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pure.scm
;; DESCRIPTION : Initialize Pure plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, (C) 2012  Albert Graef
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here are a few sample Pure sessions for TeXmacs. You might want to add
;; other session types as needed.

;; Configurable items. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convenient keybindings. In particular, the toggle-session-math-input
;; binding (Ctrl+Shift+M by default) provides a quick way to toggle between
;; program/verbatim and math mode on the session input line.

(define pure-keymap
'(;; math input toggle
  ("C-M" (toggle-session-math-input))))

;; The default "<symbol> space" math bindings are quite annoying when entering
;; Pure expressions, so we disable them here; there are other ways to get
;; these "invisible" symbols with the Tab key. (The only one that's relevant
;; in Pure is the invisible comma which you can also get with ", Tab Tab".)

;; We also make both $ and " self-inserting in math mode; we really need them
;; in Pure and it's annoying if we always have to escape these characters.
;; Note that you still have to escape the backslash (Shift-F5 \) to enter a
;; Pure lambda; the default binding is preserved in this case so that you can
;; enter special TeXmacs commands such as \eqnarray and \binom in math mode.

(define pure-math-keymap
'(;; get rid of the default "<symbol> space" bindings for invisible symbols
  ;; (comment the following lines if you really can't live without these)
  (", space" (insert ", "))
  (". space" (insert ". "))
  ("+ space" (insert "+ "))
  ;; make $ and " self-inserting
  ("$"  (insert "$"))
  ("\"" (insert "\""))))

;; Some configuration variables. We allow these to be overridden by
;; corresponding definitions in the user's init file.

;; Additional TeXmacs-specific include paths to search for Pure scripts.
(if (not (defined? 'pure-texmacs-includes))
(define pure-texmacs-includes
  ;; TEXMACS_HOME_PATH and TEXMACS_PATH should always be set
  (list (with texmacs-home (getenv "TEXMACS_HOME_PATH")
	      (string-append texmacs-home "/plugins/pure/progs"))
	(with texmacs-dir (getenv "TEXMACS_PATH")
	      (string-append texmacs-dir "/plugins/pure/progs")))))

;; Scripts to be preloaded (if present) by the Pure plugins. Filenames without
;; a slash in them are looked for first in the pure-texmacs-includes
;; directories and then in the Pure library directory.
(if (not (defined? 'pure-scripts))
(define pure-scripts (list "texmacs.pure")))

;; Default Pure library path. This is normally auto-detected (see below), but
;; if the auto-detection doesn't work for you then you'll have to set this
;; variable to the path where your Pure library scripts are to be found.
(if (not (defined? 'pure-default-lib-path))
(define pure-default-lib-path "/usr/local/lib"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Kludge to deal with old TeXmacs versions which have a slightly different
;; interface for defining keyboard maps and menus.
(define (pure-texmacs-version)
  (map string->number
       (string-tokenize (texmacs-version)
			(char-set-complement (char-set #\.)))))
(define (pure-compare xs ys)
  (cond ((null? xs) (nnull? ys))
	((null? ys) #f)
	((== (car xs) (car ys)) (pure-compare (cdr xs) (cdr ys)))
	(else (< (car xs) (car ys)))))
;; I believe that the new interface was introduced somewhere around 1.0.7.16,
;; but if the following doesn't work for you then you might want to try
;; decreasing this number.
(define pure-old-texmacs? (pure-compare (pure-texmacs-version) '(1 0 7 16)))

;; You'll have to adjust these accordingly if adding more session and script
;; plugin types below.
(define (pure-session?) (or (in-pure?) (in-pure-debug?) (in-pure-math?)))
(define (pure-script?) (or (pure-script-scripts?) (pure-script-math-scripts?)))

;; Detect the Pure library path (make a good guess on Windows).
(use-modules (ice-9 popen))
(define pure-lib-path
  ;; XXXFIXME: The Windows check is a horrible kludge, does TeXmacs provide a
  ;; better way to do this?
  (if (url-exists? "c:/Windows")
      ;; The popen stuff doesn't seem to work on Windows (throws an
      ;; exception), so we're checking some common locations instead.
      (let ((str (cond ((url-exists? "c:/msys64/mingw32/lib/pure")
		       "c:/msys64/mingw32/lib")
		      ((url-exists? "c:/msys32/mingw32/lib/pure")
		       "c:/msys32/mingw32/lib")
		      (else pure-default-lib-path))))
	str)
      ;; This needs pkg-config, and a working popen.
      (let* ((port (open-input-pipe "pkg-config pure --variable libdir"))
	     (str (read-line port)))
	(close-pipe port)
	(if (string? str) str pure-default-lib-path))))

;; Check if the given script exists on the library path or in one of the
;; texmacs-specific paths. Return the full script name if present, ""
;; otherwise.
(define (pure-script-if-present name)
  (if (string-index name #\/)
      ;; filename contains a path designation, take as is
      name
      (let ((l (append pure-texmacs-includes
		       (list (string-append pure-lib-path "/pure"))))
	    (fullname ""))
	(while (and (string-null? fullname) (nnull? l))
	       (with s (string-append (car l) "/" name)
		     (if (url-exists? s)
			 (set! fullname s)))
	       (set! l (cdr l)))
	fullname)))

;; Convenience function to create a command running the Pure interpreter with
;; the given scripts (if present).
(define (pure-cmd cmd scripts)
  (string-join
   (append (list cmd)
	   (map (lambda (s)
		  (string-append "-I " (format #f "~s" s)))
		pure-texmacs-includes)
	   (map (lambda (s)
		  (format #f "~s" (pure-script-if-present s)))
		scripts))
   " "))

;; This provides an entry point to the interpreter to query the current
;; document directory, so that the interpreter's cwd can be set accordingly.
(define (pure-cdd)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session"))
	 (name (url->unix (current-buffer)))
	 (dir (if (url-scratch? name) ""
		  (with
		   l (string-tokenize name (char-set-complement (char-set #\/)))
		   (string-join (reverse (cdr (reverse l))) "/" 'prefix)))))
    ;; (format #t "(chdir ~s)\n" dir)
    (connection-write-string lan ses
     (string-append (char->string #\020)
		    (format #f "(chdir ~s)\n" dir)))))

;; Pure menu. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This provides the usual kind of plugin menu for the Pure language. If the
;; TeXmacs-formatted documentation is available, this also includes a submenu
;; with the most important help files in it.

(define pure-texmacs-help-available
  (url-exists? (string-append pure-lib-path "/pure/docs/index.tm")))

(import-from (doc help-funcs))

(menu-bind pure-help-menu
 ("Pure help"
  (load-help-buffer
   (string-append pure-lib-path "/pure/docs/index.tm")))
 ---
 ("Pure manual"
  (load-help-buffer
   (string-append pure-lib-path "/pure/docs/pure.tm")))
 ("Pure library manual"
  (load-help-buffer
   (string-append pure-lib-path "/pure/docs/purelib.tm")))
 ---
 ("Module index"
  (load-help-buffer
   (string-append pure-lib-path "/pure/docs/pure-modindex.tm")))
 ("Index"
  (load-help-buffer
   (string-append pure-lib-path "/pure/docs/genindex.tm"))))

(menu-bind pure-menu
 (if (not-in-session?)
     (link scripts-eval-menu)
     ---
     (link scripts-eval-toggle-menu)
     (if pure-texmacs-help-available
	 ---))
 (if pure-texmacs-help-available
     (link pure-help-menu)))

(menu-bind pure-help-icons
  (if (and (pure-session?) pure-texmacs-help-available)
      |
      (=> (balloon (icon "tm_help.xpm") "Pure Help")
	  (link pure-help-menu))))

;; Activate the menus and keymaps.
(cond
 (pure-old-texmacs?
  ;; Old-style keyboard and menu interface.
  (eval `(kbd-map ,@pure-keymap))
  (eval `(kbd-map ,@pure-math-keymap))
  (menu-extend texmacs-extra-menu
   (if (or (pure-session?) (and (not-in-session?) (pure-script?)))
       (=> "Pure" (link pure-menu))))
  (menu-extend session-help-icons
   (link pure-help-icons)))
 (else
  (eval `(kbd-map (:require (pure-session?))
		  ,@pure-keymap))
  (eval `(kbd-map (:require (and (in-math?) (pure-session?)))
		  ,@pure-math-keymap))
  (menu-bind plugin-menu
   (:require (or (pure-session?) (and (not-in-session?) (pure-script?))))
   (=> "Pure" (link pure-menu)))
  (menu-bind session-help-icons
   (:require (pure-session?))
   (link pure-help-icons))))

;; We also offer an entry point for the Pure interpreter to pop up help
;; requested with the Pure help command in a TeXmacs window.

(define (pure-decompose s ch)
  (with i (string-index s ch)
    (if (not i) (list s "")
	(list (substring s 0 i) (substring s (+ i 1) (string-length s))))))

(define (pure-help url)
  (with
   (name label) (pure-decompose url #\#)
   (let* ((l
	   (string-tokenize name (char-set-complement (char-set #\/))))
	  (dir
	   (if (nnull? l)
	       (string-join (reverse (cdr (reverse l))) "/" 'prefix)
	       ""))
	  (basename
	   (if (nnull? l)
	       (with (basename extension)
		     (pure-decompose (car (last-pair l)) #\.)
		     basename)
	       ""))
	  (tm-name (string-append dir "/" basename ".tm")))
     (if (url-exists? tm-name)
	 ;; tm documentation exists, use that instead of the html docs
	 (set! name tm-name)))
     (cond ((== name "") (go-to-label label))
	   ((== label "")
	    (open-window)
	    (load-help-buffer name))
	   (else
	    (open-window)
	    (load-help-buffer name)
	    (go-to-label label)))))

;; Session plugin definitions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pure-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (pure-input) pure)
  (lazy-input-converter (pure-input) pure-debug)
  (lazy-input-converter (pure-input) pure-math)
  (lazy-input-converter (pure-input) pure-script)
  (lazy-input-converter (pure-input) pure-script-math))

(define (pure-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (verbatim-serialize lan t)
	;; (write-line s)
	s))

;; A basic session plugin with conversions for math etc. You might want to add
;; options like --plain, -q etc. to the launch command as needed.
(plugin-configure pure
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd "pure -i --texmacs" pure-scripts))
  (:serializer ,pure-serialize)
  (:tab-completion #t)
  (:session "Pure"))

;; Debugging session.
(plugin-configure pure-debug
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd "pure -i -g --texmacs" pure-scripts))
  (:serializer ,pure-serialize)
  (:tab-completion #t)
  (:session "Pure-debug"))

;; Math session. This has math output enabled (and Reduce loaded) by default.
(plugin-configure pure-math
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd "pure -i --texmacs --enable tmmath" pure-scripts))
  (:serializer ,pure-serialize)
  (:tab-completion #t)
  (:session "Pure-math"))

;; Scripting support. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is like pure-serialize, but adds the terminating ';' automatically.
(define (pure-script-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (string-append
	   (string-replace (verbatim-serialize lan t) "\n" " ") ";\n")
	;; (write-line s)
	s))

;; The script plugin. Note that we keep this separate from the other plugins,
;; so that it can have its own input serialization.
(plugin-configure pure-script
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd "pure -i -q --texmacs" pure-scripts))
  (:serializer ,pure-script-serialize)
  (:tab-completion #t)
  (:scripts "Pure"))

;; A variation of the above which has math output enabled by default.
(plugin-configure pure-script-math
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd "pure -i -q --texmacs --enable tmmath" pure-scripts))
  (:serializer ,pure-script-serialize)
  (:tab-completion #t)
  (:scripts "Pure-math"))
