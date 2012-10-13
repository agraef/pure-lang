;; Here are a few sample Pure sessions for TeXmacs. You might want to add
;; other session types as needed. NOTE: To have TeXmacs find these, drop this
;; file (along with the pure-input.scm file) into your
;; ~/.TeXmacs/plugins/pure/progs folder (create the path if needed).

;; Scripts to be preloaded (if present) in the interpreter used for
;; pure-script below. Please note that this is just an example; you should
;; adjust this to your needs.
(define pure-scripts (list "reduce.pure"))

;; Some stuff mostly pilfered from the gnuplot plugin. This provides some
;; basic conversions to make common math stuff work with Pure (see
;; pure-input.scm), but this is still incomplete at present.

(define (pure-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (pure-input) pure))

(define (pure-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim s) "\n"))))

(define (pure-script-initialize)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (pure-script-input) pure-script))

(define (pure-script-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with s (texmacs->code u)
      (string-append (escape-verbatim (string-replace s "\n" " ")) ";\n"))))

;; A basic session plugin. You might want to add options like --plain, -q
;; etc. as needed.
(plugin-configure pure
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-initialize))
  (:launch "pure -i --texmacs")
  (:serializer ,pure-serialize)
  (:session "Pure"))

;; Debugging session.
(plugin-configure pure-debug
  (:require (url-exists-in-path? "pure"))
  (:launch "pure -i -g --texmacs")
  (:session "Pure-debug"))

;; Basic scripting support.

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

;; The actual script plugin definition. Note that we keep this separate from
;; the Pure session plugins, since special input conversions are needed here.
(plugin-configure pure-script
  (:require (url-exists-in-path? "pure"))
  (:initialize (pure-script-initialize))
  (:launch ,(pure-cmd pure-scripts))
  (:serializer ,pure-script-serialize)
  (:scripts "Pure"))
