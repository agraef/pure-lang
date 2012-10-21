
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

;; INSTALLATION: Drop this file along with pure-input.scm and texmacs.pure
;; into your ~/.TeXmacs/plugins/pure/progs folder (create the path if needed).

;; Configurable items. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convenient keybindings. The toggle-session-math-input binding (Ctrl+$ by
;; default) provides a quick way to toggle between program/verbatim and math
;; mode on the session input line.

(kbd-map
 (:mode in-session?)
 ("C-$" (toggle-session-math-input)))

;; Uncomment this to make math input the default when this module is loaded.
;;(if (not (session-math-input?)) (toggle-session-math-input))

;; Additional TeXmacs-specific include path to search for Pure scripts. By
;; default we set this to your ~/.TeXmacs/plugins/pure/progs folder.
(define pure-texmacs-includes
  (with texmacs-home (getenv "TEXMACS_HOME_PATH") ;; this should always be set
	(string-join (list texmacs-home "plugins/pure/progs") "/")))

;; Script file with additional Pure definitions to be preloaded by the Pure
;; script plugin (see below at the end of this file). You might want to edit
;; this script as needed.
(define pure-texmacs-script
  (string-join (list pure-texmacs-includes "texmacs.pure") "/"))

;; Scripts to be preloaded (if present) by the Pure script plugin. Filenames
;; without a slash in them are looked for in the Pure library directory.

;; NOTE: This is just an example; you should adjust this to your needs. Also
;; note that these scripts are *only* preloaded by the Pure script plugin,
;; since this doesn't provide a simple way to import the modules that you
;; need. The session plugins always start up with just the Pure prelude
;; loaded, so there you have to use Pure's 'using' declaration to load any
;; additional modules that you need. Both the scripting plugin and the session
;; plugins have the ~/.TeXmacs/plugins/pure/progs folder in the Pure module
;; search path, however, so that any Pure scripts located there can be
;; imported without any ado.

(define pure-scripts
  (list
   ;; add any other scripts to be preloaded here
   "reduce.pure" ;; if you want Reduce to be preloaded
   ;; texmacs.pure script
   (if (url-exists? pure-texmacs-script)
       pure-texmacs-script
       ;; fall back to a default texmacs.pure script in the Pure library
       ;; directory, if any
       "texmacs.pure")))

;; Default Pure library path. This is normally auto-detected (see below), but
;; if the auto-detection doesn't work for you then you'll have to set this
;; variable to the path where your Pure library scripts are to be found.
(define pure-default-lib-path "/usr/local/lib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Detect the Pure library path (this needs pkg-config).
(use-modules (ice-9 popen))
(define pure-lib-path
  (let* ((port (open-input-pipe "pkg-config pure --variable libdir"))
	 (str (read-line port)))
    (close-pipe port)
    (if (string? str) str pure-default-lib-path)))

;; Check if the given script exists on the library path. Return the full
;; script name if present, "" otherwise.
(define (pure-script-if-present name)
  (with s (if (string-index name #\/)
	      ;; filename contains a path designation, take as is
	      name
	      ;; look in the library dir
	      (string-join (list pure-lib-path "pure" name) "/"))
	(if (url-exists? s) s "")))

;; Convenience function to create a command running the Pure interpreter with
;; the given scripts (if present).
(define (pure-cmd cmd scripts)
  (string-join (append (list cmd)
		       (if (string? pure-texmacs-includes)
			   (list "-I" pure-texmacs-includes)
			   (list))
		       (map pure-script-if-present scripts)) " "))

;; Entry point for Pure help commands.
(define (decompose-url s)
  (with i (string-index s #\#)
    (if (not i) (list s "")
	(list (substring s 0 i) (substring s (+ i 1) (string-length s))))))

(define (pure-help url)
  (with (name label) (decompose-url url)
	(cond ((== name "") (go-to-label label))
	      ((== label "")
	       (set-message `(concat "Loaded " ,name) "Pure help")
	       (with u (url-relative (buffer-master) name)
		     (load-buffer-in-new-window u)))
	      (else
	       (set-message `(concat "Loaded " ,name) "Pure help")
	       (with u (url-relative (buffer-master) name)
		     (load-buffer-in-new-window u)
		     (go-to-label label))))))

;; Session plugin definitions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pure-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (pure-input) pure))

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
  (:launch ,(pure-cmd "pure -i --texmacs" '()))
  (:serializer ,pure-serialize)
  (:tab-completion #t)
  (:session "Pure"))

;; Debugging session.
(plugin-configure pure-debug
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd "pure -i -g --texmacs" '()))
  (:serializer ,pure-serialize)
  (:tab-completion #t)
  (:session "Pure-debug"))

;; Scripting support. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pure-script-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (string-append
	   (string-replace (verbatim-serialize lan t) "\n" " ") ";\n")
	;; (write-line s)
	s))

;; The script plugin. Note that we keep this separate from the other plugins,
;; so that it can have its own environment and input serialization.
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
