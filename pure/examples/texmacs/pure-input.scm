
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : pure-input.scm
;; DESCRIPTION : Pure input conversions
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, (C) 2012  Albert Graef
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pure input conversions by Albert Graef <Dr.Graef@t-online.de>. This was
;; mostly pilfered from various TeXmacs plugins and the generic plugin code.

;; NOTE: Some kludges are needed to bring some constructs such as limits and
;; big operators (sums, integrals, etc.) into a form which prevents Pure
;; syntax errors and allows easy interaction with Reduce. I hope that these
;; will work correctly in most cases; if it doesn't, please submit a bug
;; report with the offending expression and I will try to fix it.

;; Equations and equation arrays don't work properly in the input line. I
;; tried really hard to fix this, but TeXmacs stubbornly refuses to apply any
;; custom conversion rules for these. If you have an idea how to fix this,
;; please let me know.

;; So this means that you'll have to stick to ordinary inline formulas for
;; now. A quick way to get these is to use the Ctrl+$ shortcut to toggle the
;; input line between verbatim and math mode. For executable input fields and
;; spreadsheat tables you can just use the normal $ shortcut to enter math
;; mode.

(texmacs-module (pure-input)
  (:use (utils plugins plugin-convert)))

;; do something reasonable with sub- and superscripts

(define (pure-rsub-lim r)
  (cond ((and (string? (car r)) (string-contains (car r) "<rightarrow>"))
	 ;; An awful kludge to bring limits into a more digestible form.
	 ;; This isn't really fool-proof but it's the best that we can do
	 ;; since TeXmacs has no special markup for limits.
	 (display " (")
	 (plugin-input (string-replace (car r) "<rightarrow>" ") (")))
	(else (display "!(") (plugin-input (car r))))
  (display ")"))

;; Use this instead if pure-rsub-lim above gives you trouble.
(define (pure-rsub r)
  (display "!(")
  (plugin-input (car r))
  (display ")"))

(define (pure-rsup r)
  (display "^(")
  (plugin-input (car r))
  (display ")"))

;; primes and accents

(define (pure-rprime r)
  (plugin-input (car r)))

(define (pure-wide t)
  (let* ((op (car t))
	 (acc (cadr t)))
    (display "(")
    (cond ((and (string? acc) (== acc "^"))
	   (display "hat"))
	  ((and (string? acc) (== acc "~"))
	   (display "tilde"))
	  (else (plugin-input acc)))
    (display " (")
    (plugin-input op)
    (display "))")))

;; fractions

(define (pure-frac t)
  (display "((")
  (plugin-input (car t))
  (display ")/(")
  (plugin-input (cadr t))
  (display "))"))

;; Brackets: |.| and ||.|| are mapped to abs and norm, respectively, others
;; are left as is.

(define (pure-around t)
  (let* ((left (car t))
	 (op (cadr t))
	 (right (caddr t)))
    (cond ((and (string? left) (== left "|"))
	   (display "abs (")
	   (plugin-input op)
	   (display ")"))
	  ((and (string? left) (== left "<||>"))
	   (display "norm (")
	   (plugin-input op)
	   (display ")"))
	  (else
	   (plugin-input left)
	   (plugin-input op)
	   (plugin-input right)))))

;; This removes special markup around some math constructs. It also covers the
;; case of a singleton | (needed for comprehensions in Pure), which can be
;; entered as a "middle |" (Alt+M |) in TeXmacs.

(define (pure-math t)
  (plugin-input (car t)))

;; Matrix support (pilfered from maxima-input.scm, slightly massaged to make
;; it work better with Pure).

(define (pure-var-row r)
  (if (nnull? r)
      (begin
	(display ", ")
	(plugin-input (car r))
	(pure-var-row (cdr r)))))

(define (pure-row r)
  (display "{")
  (plugin-input (car r))
  (pure-var-row (cdr r))
  (display "}"))

(define (pure-var-rows t)
  (if (nnull? t)
      (begin
	(display "; ")
	(pure-row (car t))
	(pure-var-rows (cdr t)))))

(define (pure-rows t)
  (display "{")
  (pure-row (car t))
  (pure-var-rows (cdr t))
  (display "}"))

(define (pure-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (pure-descend-last (cdr args))))

(define (pure-det args)
  (display "det (")
  (pure-descend-last args)
  (display ")"))

;; roots (also pilfered from maxima-input.scm)

(define (pure-sqrt args)
  (if (= (length args) 1)
      (begin
        (display "sqrt(")
        (plugin-input (car args))
        (display ")"))
      (begin
        (display "(")
        (plugin-input (car args))
        (display ")^(1/(")
        (plugin-input (cadr args))
        (display "))"))))

;; Sums, integrals etc. This is quite different from the Maxima code to
;; accommodate the Pure syntax. It also offers support for entering aggregates
;; of list comprehensions (sum, prod), if you leave away the superscript and
;; specify the generator and filter clauses of the comprehension in the
;; subscript.

(define (pure-comp op body args)
  (display op)
  (display " [")
  (plugin-input body)
  (if (nnull? args)
      (if (nnull? (cdr args))
          (begin ;; both lower and upper index
            (display "|")
            (plugin-input (car args))
            (display "..")
            (plugin-input (cadr args))
            (display "]"))
          (begin ;; lower index only
            (display "|")
            (plugin-input (car args))
            (display "]")))
      (display "]")))

(define (pure-rewrite-body s)
  (if (string? s)
      (string-replace s "<mathd>" ") (")
      s))

(define (pure-big op body args)
  (display op)
  (display " (")
  (plugin-input
   (if (== op "intg")
       ;; Kludge: We need to rewrite <mathd> to ") (" here to get the
       ;; integration variable as a separate argument, as required by Reduce.
       (if (list? body)
	   (map pure-rewrite-body body)
	   (pure-rewrite-body body))
       body))
  (if (nnull? args)
      (let* ((sub (car args)) (sup (cdr args)))
        (display ") (")
	;; Yet another awful kludge which massages a subscript of the form
	;; "n = start" into two separate arguments as Reduce wants them. Where
	;; things get really awful is when the subscript is a concat node.
	;; Is there a better way to do this?
	(cond ((string? sub)
	       (display (string-replace sub "=" ") (")))
	      ((and (list? sub) (nnull? sub) (eq? (car sub) 'concat)
		    (string? (cadr sub)))
		 (with s (string-replace (cadr sub) "=" ") (")
		       (plugin-input (append (list 'concat s) (cddr sub)))))
	      (else (plugin-input sub)))
        (if (nnull? sup)
            (begin
              (display ") (")
              (plugin-input (car sup))))
        (display ")"))
      (display ")")))

(define (pure-big-around args)
  (let* ((b `(big-around ,@args))
	 (op (big-name b))
	 (sub (big-subscript b))
	 (sup (big-supscript b))
	 (body (big-body b))
	 (l (cond ((and sub sup) (list sub sup))
		  (sub (list sub))
		  (else (list)))))
    (cond ((or (== op "intlim") (== op "int"))
	   ;; supported by Pure/Reduce
	   (pure-big "intg" body l))
	  ((and sub sup (or (== op "sum") (== op "prod")))
	   ;; supported by Pure/Reduce if both lower and upper bound is given
	   (pure-big op body l))
	  ;; anything else is assumed to be a Pure aggregate which translates
	  ;; to a list comprehension
	  (else (pure-comp op body l)))))

(plugin-input-converters pure
  (rows pure-rows)
  (det pure-det)
  (sqrt pure-sqrt)
  (big-around pure-big-around)
  (around pure-around)
  (around* pure-around)
  (mid pure-math)
  (math-bf pure-math)
  (math-it pure-math)
  (math-sl pure-math)
  (math-ss pure-math)
  (math-tt pure-math)
  (math-up pure-math)
  (rprime pure-rprime)
  (rsub pure-rsub-lim)
  (rsup pure-rsup)
  (wide pure-wide)
  (frac pure-frac)
  (dfrac pure-frac)
  (tfrac pure-frac)
  (frac* pure-frac)

;; These are mostly from the generic converter. Note that some of these aren't
;; defined in Pure by default, but you might want to declare them yourself.
;; There's probably stuff missing here and you might want to change some of
;; these assignments.

  ("<longequal>" "==")
  ("<assign>" ":=")
  ("<plusassign>" "+=")
  ("<minusassign>" "-=")
  ("<timesassign>" "*=")
  ("<overassign>" "/=")
  ("<lflux>" "<<")
  ("<gflux>" ">>")
  ("<partial>" " d ")
  ("<mathd>" " d ")
  ("<mathe>" " e ")
  ("<mathpi>" " pi ")
  ("<backslash>" "\\")

  ("<implies>" "=<gtr>")
  ("<Rightarrow>" "=<gtr>")
  ("<Leftrightarrow>" "<less>=<gtr>")
  ("<neg>" "~")
  ("<wedge>" "&&")
  ("<vee>" "||")
  ("<equiv>" "===") ; syntactic identity in Pure
  ("<nequiv>" "~==") ; dito
  ("<neq>" "~=")
  ("<less>" "<less>")
  ("<gtr>" "<gtr>")
  ("<leq>" "<less>=")
  ("<geq>" "<gtr>=")
  ("<leqslant>" "<less>=")
  ("<geqslant>" "<gtr>=")
  ("<ll>" "<less><less>")
  ("<gg>" "<gtr><gtr>")
  ("<into>" "-<gtr>")
  ("<mapsto>" "|-<gtr>")
  ("<rightarrow>" "-<gtr>")
  ("<transtype>" ":<gtr>")

  ("<bar>" "bar")
  ("<vect>" "vect")
  ("<check>" "check")
  ("<breve>" "breve")
  ("<dot>" "dot")
  ("<ddot>" "ddot")
  ("<accute>" "accute")
  ("<grave>" "grave")

  ("<lfloor>" "floor (")
  ("<rfloor>" ")")
  ("<lceil>" "ceil (")
  ("<rceil>" ")")
  ("<langle>" "(")
  ("<rangle>" ")")
  ("<llbracket>" "[")
  ("<rrbracket>" "]")
  ("<nobracket>" " ")

  ("<um>" "-")
  ("<upl>" "") ; unary plus not supported in Pure
  ("<times>" "*")
  ("<ast>" "*")
  ("<cdot>" "*")
  ("<cdots>" "..")
  ("<ldots>" "..")
  ("<colons>" "::")
  ("<sharp>" "#")
  ("<circ>" ".") ; function composition in Pure

;; Here are a few other operators that might be useful. I'm too lazy to do
;; them all, so add others as needed. Note that none of these except div is
;; predefined in Pure, so you'll have to declare them as infix symbols if you
;; want to use them.

  ("<pm>" " pm ")
  ("<mp>" " mp ")
  ("<div>" " div ")
  ("<cap>" " bcap ")
  ("<cup>" " bcup ")
  ("<uplus>" " uplus ")
  ("<oplus>" " oplus ")
  ("<ominus>" " ominus ")
  ("<otimes>" " otimes ")
  ("<oslash>" " oslash ")

;; Special glyphs available in TeXmacs. Unicode actually has equivalents for
;; most of these in the MathML character set, which may be used as identifier
;; constituents in Pure. Unfortunately, those code points are different from
;; what TeXmacs uses internally, so it won't display the MathML characters
;; correctly. Thus, in order to make these glyphs work in Pure without causing
;; too much havoc, for the time being we map them to ordinary Latin letters
;; instead. Note that this means that Pure won't be able to distinguish, say,
;; 𝔄, 𝓐 or 𝔸 from A, so these will all denote the same identifier in Pure.

  ("<bbb-A>" "A")
  ("<bbb-B>" "B")
  ("<bbb-C>" "C")
  ("<bbb-D>" "D")
  ("<bbb-E>" "E")
  ("<bbb-F>" "F")
  ("<bbb-G>" "G")
  ("<bbb-H>" "H")
  ("<bbb-I>" "I")
  ("<bbb-J>" "J")
  ("<bbb-K>" "K")
  ("<bbb-L>" "L")
  ("<bbb-M>" "M")
  ("<bbb-N>" "N")
  ("<bbb-O>" "O")
  ("<bbb-P>" "P")
  ("<bbb-Q>" "Q")
  ("<bbb-R>" "R")
  ("<bbb-S>" "S")
  ("<bbb-T>" "T")
  ("<bbb-U>" "U")
  ("<bbb-V>" "V")
  ("<bbb-W>" "W")
  ("<bbb-X>" "X")
  ("<bbb-Y>" "Y")
  ("<bbb-Z>" "Z")

  ("<bbb-a>" "a")
  ("<bbb-b>" "b")
  ("<bbb-c>" "c")
  ("<bbb-d>" "d")
  ("<bbb-e>" "e")
  ("<bbb-f>" "f")
  ("<bbb-g>" "g")
  ("<bbb-h>" "h")
  ("<bbb-i>" "i")
  ("<bbb-j>" "j")
  ("<bbb-k>" "k")
  ("<bbb-l>" "l")
  ("<bbb-m>" "m")
  ("<bbb-n>" "n")
  ("<bbb-o>" "o")
  ("<bbb-p>" "p")
  ("<bbb-q>" "q")
  ("<bbb-r>" "r")
  ("<bbb-s>" "s")
  ("<bbb-t>" "t")
  ("<bbb-u>" "u")
  ("<bbb-v>" "v")
  ("<bbb-w>" "w")
  ("<bbb-x>" "x")
  ("<bbb-y>" "y")
  ("<bbb-z>" "z")

  ("<cal-A>" "A")
  ("<cal-B>" "B")
  ("<cal-C>" "C")
  ("<cal-D>" "D")
  ("<cal-E>" "E")
  ("<cal-F>" "F")
  ("<cal-G>" "G")
  ("<cal-H>" "H")
  ("<cal-I>" "I")
  ("<cal-J>" "J")
  ("<cal-K>" "K")
  ("<cal-L>" "L")
  ("<cal-M>" "M")
  ("<cal-N>" "N")
  ("<cal-O>" "O")
  ("<cal-P>" "P")
  ("<cal-Q>" "Q")
  ("<cal-R>" "R")
  ("<cal-S>" "S")
  ("<cal-T>" "T")
  ("<cal-U>" "U")
  ("<cal-V>" "V")
  ("<cal-W>" "W")
  ("<cal-X>" "X")
  ("<cal-Y>" "Y")
  ("<cal-Z>" "Z")

  ("<frak-A>" "A")
  ("<frak-B>" "B")
  ("<frak-C>" "C")
  ("<frak-D>" "D")
  ("<frak-E>" "E")
  ("<frak-F>" "F")
  ("<frak-G>" "G")
  ("<frak-H>" "H")
  ("<frak-I>" "I")
  ("<frak-J>" "J")
  ("<frak-K>" "K")
  ("<frak-L>" "L")
  ("<frak-M>" "M")
  ("<frak-N>" "N")
  ("<frak-O>" "O")
  ("<frak-P>" "P")
  ("<frak-Q>" "Q")
  ("<frak-R>" "R")
  ("<frak-S>" "S")
  ("<frak-T>" "T")
  ("<frak-U>" "U")
  ("<frak-V>" "V")
  ("<frak-W>" "W")
  ("<frak-X>" "X")
  ("<frak-Y>" "Y")
  ("<frak-Z>" "Z")

  ("<frak-a>" "a")
  ("<frak-b>" "b")
  ("<frak-c>" "c")
  ("<frak-d>" "d")
  ("<frak-e>" "e")
  ("<frak-f>" "f")
  ("<frak-g>" "g")
  ("<frak-h>" "h")
  ("<frak-i>" "i")
  ("<frak-j>" "j")
  ("<frak-k>" "k")
  ("<frak-l>" "l")
  ("<frak-m>" "m")
  ("<frak-n>" "n")
  ("<frak-o>" "o")
  ("<frak-p>" "p")
  ("<frak-q>" "q")
  ("<frak-r>" "r")
  ("<frak-s>" "s")
  ("<frak-t>" "t")
  ("<frak-u>" "u")
  ("<frak-v>" "v")
  ("<frak-w>" "w")
  ("<frak-x>" "x")
  ("<frak-y>" "y")
  ("<frak-z>" "z")

  ("<b-up-A>" "A")
  ("<b-up-B>" "B")
  ("<b-up-C>" "C")
  ("<b-up-D>" "D")
  ("<b-up-E>" "E")
  ("<b-up-F>" "F")
  ("<b-up-G>" "G")
  ("<b-up-H>" "H")
  ("<b-up-I>" "I")
  ("<b-up-J>" "J")
  ("<b-up-K>" "K")
  ("<b-up-L>" "L")
  ("<b-up-M>" "M")
  ("<b-up-N>" "N")
  ("<b-up-O>" "O")
  ("<b-up-P>" "P")
  ("<b-up-Q>" "Q")
  ("<b-up-R>" "R")
  ("<b-up-S>" "S")
  ("<b-up-T>" "T")
  ("<b-up-U>" "U")
  ("<b-up-V>" "V")
  ("<b-up-W>" "W")
  ("<b-up-X>" "X")
  ("<b-up-Y>" "Y")
  ("<b-up-Z>" "Z")

  ("<b-a>" "a")
  ("<b-b>" "b")
  ("<b-c>" "c")
  ("<b-d>" "d")
  ("<b-e>" "e")
  ("<b-f>" "f")
  ("<b-g>" "g")
  ("<b-h>" "h")
  ("<b-i>" "i")
  ("<b-j>" "j")
  ("<b-k>" "k")
  ("<b-l>" "l")
  ("<b-m>" "m")
  ("<b-n>" "n")
  ("<b-o>" "o")
  ("<b-p>" "p")
  ("<b-q>" "q")
  ("<b-r>" "r")
  ("<b-s>" "s")
  ("<b-t>" "t")
  ("<b-u>" "u")
  ("<b-v>" "v")
  ("<b-w>" "w")
  ("<b-x>" "x")
  ("<b-y>" "y")
  ("<b-z>" "z")

  ("<b-0>" "0")
  ("<b-1>" "1")
  ("<b-2>" "2")
  ("<b-3>" "3")
  ("<b-4>" "4")
  ("<b-5>" "5")
  ("<b-6>" "6")
  ("<b-7>" "7")
  ("<b-8>" "8")
  ("<b-9>" "9")

  ("<infty>"      "inf")
  ("<emptyset>"   "[]")
  ("<mathe>"      "e")
  ("<mathpi>"     "pi")
  ("<mathi>"      "i")
  ;; The following makes sense only in comprehensions, so that you can write
  ;; stuff like [2*x|x ∈ 1..10]. If you don't need this then you might want to
  ;; remap this to an infix membership test predicate instead.
  ("<in>"         "=")

  ("<alpha>"      "alpha")
  ("<beta>"       "beta")
  ("<gamma>"      "gamma")
  ("<delta>"      "delta")
  ("<epsilon>"    "epsilon")
  ("<varepsilon>" "epsilon")
  ("<zeta>"       "zeta")
  ("<eta>"        "eta")
  ("<theta>"      "theta")
  ("<vartheta>"   "theta")
  ("<iota>"       "iota")
  ("<kappa>"      "kappa")
  ("<lambda>"     "lambda")
  ("<mu>"         "mu")
  ("<nu>"         "nu")
  ("<xi>"         "xi")
  ("<omicron>"    "omicron")
  ("<pi>"         "pi")
  ("<varpi>"      "pi")
  ("<rho>"        "rho")
  ("<varrho>"     "varrho")
  ("<sigma>"      "sigma")
  ("<varsigma>"   "sigma")
  ("<tau>"        "tau")
  ("<upsilon>"    "upsilon")
  ("<phi>"        "phi")
  ("<varphi>"     "phi")
  ("<chi>"        "chi")
  ("<psi>"        "psi")
  ("<omega>"      "omega")

  ("<Alpha>"      "Alpha")
  ("<Beta>"       "Beta")
  ("<Gamma>"      "Gamma")
  ("<Delta>"      "Delta")
  ("<Epsilon>"    "Epsilon")
  ("<Zeta>"       "Zeta")
  ("<Eta>"        "Eta")
  ("<Theta>"      "Theta")
  ("<Iota>"       "Iota")
  ("<Kappa>"      "Kappa")
  ("<Lambda>"     "Lambda")
  ("<Mu>"         "Mu")
  ("<Nu>"         "Nu")
  ("<Xi>"         "Xi")
  ("<Omicron>"    "Omicron")
  ("<Pi>"         "Pi")
  ("<Rho>"        "Rho")
  ("<Sigma>"      "Sigma")
  ("<Tau>"        "Tau")
  ("<Upsilon>"    "Upsilon")
  ("<Phi>"        "Phi")
  ("<Chi>"        "Chi")
  ("<Psi>"        "Psi")
  ("<Omega>"      "Omega"))
