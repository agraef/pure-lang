;; Here are a few sample Pure sessions for TeXmacs. You might want to add
;; other session types as needed. NOTE: To have TeXmacs find these, drop this
;; file (along with the pure-input.scm file) into your
;; ~/.TeXmacs/plugins/pure/progs folder (create the path if needed).

;; Scripts to be preloaded (if present) in the pure-script plugin. Note that
;; this is just an example; you should adjust this to your needs.
(define pure-scripts (list "reduce.pure"))

(define (pure-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (pure-input) pure))

(define (pure-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (verbatim-serialize lan t)
	;; (write-line s)
	s))

;; A basic session plugin with conversions for math etc. You might want to add
;; options like --plain, -q etc. as needed.
(plugin-configure pure
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch "pure -i --texmacs")
  (:serializer ,pure-serialize)
  (:session "Pure"))

;; Debugging session.
(plugin-configure pure-debug
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch "pure -i -g --texmacs")
  (:serializer ,pure-serialize)
  (:session "Pure-debug"))

;; Scripting support.

;; Detect the Pure library path (this needs pkg-config, if you don't have this
;; then you probably need to set a suitable default below).
(define pure-default-lib-path "/usr/local/lib")
(use-modules (ice-9 popen))
(define pure-lib-path
  (let* ((port (open-input-pipe "pkg-config pure --variable libdir"))
	 (str (read-line port)))
    (close-pipe port)
    (if (string? str) str pure-default-lib-path)))

;; Check if the given script exists on the library path. Return the full
;; script name if present, "" otherwise.
(define (pure-script-if-present name)
  (with s (string-join (list pure-lib-path "pure" name) "/")
	(if (url-exists? s) s "")))

;; Convenience function to create a command running the Pure interpreter with
;; the given scripts on the Pure library path (if present).
(define (pure-cmd scripts)
  (string-join (append (list "pure -i -q --texmacs")
		       (map pure-script-if-present scripts)) " "))

(define (pure-script-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with s (string-append (verbatim-serialize lan t) ";\n")
	(write-line s)
	s))

;; The script plugin. Note that we keep this separate from the Pure session
;; plugins, so that it has its environment and scripts can be loaded
;; automatically at startup.
(plugin-configure pure-script
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch ,(pure-cmd pure-scripts))
  (:serializer ,pure-script-serialize)
  (:scripts "Pure"))
