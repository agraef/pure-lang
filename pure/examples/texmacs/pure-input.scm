
;; Basic input conversions for the scripting support, pilfered from TeXmacs'
;; gnuplot plugin and the sample input plugin.

(texmacs-module (pure-input)
  (:use (utils plugins plugin-convert)))

;; This only covers a small part of TeXmacs' math mode right now. I have no
;; idea what else to put in here, so just add the things that you need.
;; See http://www.texmacs.org/tmweb/manual/webman-write-itf.en.html, section
;; "Mathematical and customized input" on how these conversions work.

(define (pure-rsub r)
  (display "!(")
  (plugin-input (car r))
  (display ")"))

(define (pure-rsup r)
  (display "^(")
  (plugin-input (car r))
  (display ")"))

(define (pure-frac t)
  (display "((")
  (plugin-input (car t))
  (display ")/(")
  (plugin-input (cadr t))
  (display "))"))

(plugin-input-converters pure
  (rows plugin-input-var-rows)
  (rsub pure-rsub)
  (rsup pure-rsup)
  (frac pure-frac)
  ("<mathe>" "e")
  ("<mathpi>" "pi")
  ("<backslash>" "\\")
  ("<ldots>" "..")
  ("<cdots>" "..")
  ("<cdot>" "*")
  ("<times>" "*")
  ("<neq>" "~=")
  ("<less>" "<")
  ("<gtr>" ">")
  ("<neg>" "~")
  ("<vee>" "||")
  ("<wedge>" "&&"))

(plugin-input-converters pure-script
  (rsub pure-rsub)
  (rsup pure-rsup)
  (frac pure-frac)
  ("<mathe>" "e")
  ("<mathpi>" "pi")
  ("<backslash>" "\\")
  ("<ldots>" "..")
  ("<cdots>" "..")
  ("<cdot>" "*")
  ("<times>" "*")
  ("<neq>" "~=")
  ("<less>" "<")
  ("<gtr>" ">")
  ("<neg>" "~")
  ("<vee>" "||")
  ("<wedge>" "&&"))
