
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

;; NOTE: We allow these to be overridden by corresponding definitions in the
;; user's init file. FIXME: Doesn't TeXmacs have a standard way of doing this?

;; Convenient keybindings. The toggle-session-math-input binding (Ctrl+$ by
;; default) provides a quick way to toggle between program/verbatim and math
;; mode on the session input line.

(kbd-map
 (:mode in-session?)
 ("C-$" (toggle-session-math-input)))

;; Uncomment this to make math input the default when this module is loaded.
;;(if (not (session-math-input?)) (toggle-session-math-input))

;; Additional TeXmacs-specific include paths to search for Pure scripts.
(if (not (defined? 'pure-texmacs-includes))
(define pure-texmacs-includes
  ;; TEXMACS_HOME_PATH and TEXMACS_PATH should always be set
  (list (with texmacs-home (getenv "TEXMACS_HOME_PATH")
	      (string-append texmacs-home "/plugins/pure/progs"))
	(with texmacs-dir (getenv "TEXMACS_PATH")
	      (string-append texmacs-dir "/plugins/pure/progs")))))

;; Scripts to be preloaded (if present) by the Pure script plugin. Filenames
;; without a slash in them are looked for first in the pure-texmacs-includes
;; directories and then in the Pure library directory.
(if (not (defined? 'pure-scripts))
(define pure-scripts (list "reduce.pure" "texmacs.pure")))

;; Default Pure library path. This is normally auto-detected (see below), but
;; if the auto-detection doesn't work for you then you'll have to set this
;; variable to the path where your Pure library scripts are to be found.
(if (not (defined? 'pure-default-lib-path))
(define pure-default-lib-path "/usr/local/lib"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Detect the Pure library path (this needs pkg-config).
(use-modules (ice-9 popen))
(define pure-lib-path
  (let* ((port (open-input-pipe "pkg-config pure --variable libdir"))
	 (str (read-line port)))
    (close-pipe port)
    (if (string? str) str pure-default-lib-path)))

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
