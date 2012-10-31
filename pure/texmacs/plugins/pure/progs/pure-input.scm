
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

(texmacs-module (pure-input)
  (:use (utils plugins plugin-convert)))

;; NOTE: Some kludges are needed to bring some constructs such as limits and
;; big operators (sums, integrals, etc.) into a form which prevents Pure
;; syntax errors and allows easy interaction with Reduce. I hope that these
;; will work correctly in most cases; if it doesn't, please submit a bug
;; report with the offending expression and I will try to fix it.

;; Symbols in the following list have "big" variants created automatically if
;; they are used as a big operator, e.g.: cap -> bigcap, vee -> bigvee etc.,
;; so that the base symbols can be used as binary functions or operators at
;; the same time. (TODO: This is probably incomplete, add others as needed.)
(define pure-big-ops
  (list "cap" "cup" "sqcap" "sqcup" "vee" "wedge" "curlyvee" "curlywedge"
	"uplus" "box" "oplus" "otimes" "odot"));

;; Please also see the end of this file for input conversions which you might
;; want to adjust for your needs.

;; do something reasonable with sub- and superscripts

(define (pure-rsub r)
  (display "!(")
  (plugin-input (car r))
  (display ")"))

(define (pure-rsup r)
  (display "^(")
  (plugin-input (car r))
  (display ")"))

(define (pure-lsub r)
  (display "(!(")
  (plugin-input (car r))
  (display "))"))

(define (pure-lsup r)
  (display "(^(")
  (plugin-input (car r))
  (display "))"))

(define (pure-above t)
  (display "(above (")
  (plugin-input (car t))
  (display ") (")
  (plugin-input (cadr t))
  (display "))"))

(define (pure-below t)
  (display "(below (")
  (plugin-input (car t))
  (display ") (")
  (plugin-input (cadr t))
  (display "))"))

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

(define (pure-neg r)
  (display "~(")
  (plugin-input (car r))
  (display ")"))

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
    (cond ((and (== left "|") (== right "|"))
	   (display "(abs (")
	   (plugin-input op)
	   (display "))"))
	  ((and (== left "<||>") (== right "<||>"))
	   (display "(norm (")
	   (plugin-input op)
	   (display "))"))
	  ((and (== left "<nobracket>") (== right "<nobracket>"))
	   (display "(")
	   (plugin-input op)
	   (display ")"))
	  (else
	   (plugin-input left)
	   (plugin-input op)
	   (plugin-input right)))))

;; This removes special markup around some math constructs. It also covers the
;; case of a singleton | (needed for comprehensions in Pure), which can be
;; entered either as Shift-F5 | or as a "middle |" (Alt+M |) in TeXmacs.

(define (pure-math t)
  (plugin-input (car t)))

(define (pure-space t)
  (display " "))

;; Support for matrices and various kinds of tables. This was originally
;; pilfered from maxima-input.scm, but has been heavily modified to support
;; various kinds of tables including stacks and choices.

(define pure-inside-case? #f)
(define pure-inside-eqn? #f)

(define pure-matrix-delims (list "{"	"}"	"; "	"{"	"}"	", "))
(define pure-stack-delims  (list " "	" "	" "	""	""	" "))
(define pure-choice-delims (list " "	" "	"; = "	""	""	" "))
(define pure-case-delims   (list " "	" end "	"; "	""	""	" "))
(define pure-delims pure-matrix-delims)

(define (pure-outer-left)   (list-ref pure-delims 0))
(define (pure-outer-right)  (list-ref pure-delims 1))
(define (pure-outer-middle) (list-ref pure-delims 2))
(define (pure-inner-left)   (list-ref pure-delims 3))
(define (pure-inner-right)  (list-ref pure-delims 4))
(define (pure-inner-middle) (list-ref pure-delims 5))

(define (pure-var-row2 r)
  (if (nnull? r)
      (begin
	(display (pure-inner-middle))
	(plugin-input (car r))
	(pure-var-row2 (cdr r)))))

(define (pure-var-row r)
  (if (nnull? r)
      (begin
	(display (pure-inner-middle))
	(plugin-input (car r))
	(pure-var-row2 (cdr r)))))

(define (pure-row r)
  (display (pure-inner-left))
  (plugin-input (car r))
  (pure-var-row (cdr r))
  (display (pure-inner-right)))

(define (pure-var-rows t)
  (if (nnull? t)
      (begin
	(display (pure-outer-middle))
	(pure-row (car t))
	(pure-var-rows (cdr t)))))

(define (pure-rows t)
  (display (pure-outer-left))
  (pure-row (car t))
  (pure-var-rows (cdr t))
  (display (pure-outer-right)))

(define (pure-descend-last args)
  (if (null? (cdr args))
      (plugin-input (car args))
      (pure-descend-last (cdr args))))

(define (pure-det args)
  (with delims pure-delims
	(display "(det ")
	(set! pure-delims pure-matrix-delims)
	(pure-descend-last args)
	(set! pure-delims delims)
	(display ")")))

(define (pure-matrix args)
  (with delims pure-delims
	(set! pure-delims pure-matrix-delims)
	(pure-descend-last args)
	(set! pure-delims delims)))

(define (pure-stack args)
  (with delims pure-delims
	(set! pure-delims pure-stack-delims)
	(pure-descend-last args)
	(set! pure-delims delims)))

(define (pure-choice args)
  (with delims pure-delims
	(set! pure-delims
	      (cond (pure-inside-case? pure-case-delims)
		    (pure-inside-eqn? pure-choice-delims)
		    (else pure-stack-delims)))
	(pure-descend-last args)
	(set! pure-inside-case? #f)
	(set! pure-inside-eqn? #f)
	(set! pure-delims delims)))

(define (pure-binom args)
  (display "binom (")
  (plugin-input (car args))
  (display ") (")
  (plugin-input (cadr args))
  (display ")"))

;; trees

(define (pure-tree-args args)
  (plugin-input (car args))
  (if (nnull? (cdr args))
      (begin
	(display ",")
	(pure-tree-args (cdr args)))))

(define (pure-tree args)
  (display "(tree [")
  (if (nnull? args) (pure-tree-args args))
  (display "])"))

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
      (cond ((string-contains? s "*<mathd>*")
	     (string-replace s "*<mathd>*" "*d*"))
	    ((string-contains? s "<mathd>")
	     (string-replace s "<mathd>" ","))
	    ((== s "d") ",")
	    (else
	     (let* ((toks (string-tokenize
			   s (char-set-complement char-set:whitespace)))
		    (ws1 (if (and (nnull? toks)
				  (char-whitespace? (string-ref s 0)))
			     " " ""))
		    (ws2 (if (and (nnull? toks)
				  (char-whitespace?
				   (string-ref s (1- (string-length s)))))
			     " " "")))
	       (if (and (nnull? toks)
			(or (nnull? (cdr toks))
			    (not (string-null? ws1))
			    (not (string-null? ws2))))
		   (string-append
		    ws1
		    (string-join (map pure-rewrite-body toks) " ")
		    ws2)
		   s))))
      s))

(define (pure-big op body args)
  (display "(")
  (display op)
  (display " (")
  (plugin-input
   (if (== op "intg")
       ;; We need to rewrite <mathd> to "," here to get the integration
       ;; variable as a separate argument, as required by Reduce. (We don't
       ;; want to do this unconditionally since we also use <mathd> for
       ;; differentials.)
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
        (display "))"))
      (display "))")))

(define (pure-big-around args)
  (let* ((b `(big-around ,@args))
	 (op (big-name b))
	 (op (if (list-find pure-big-ops (lambda (s) (== op s)))
		 (string-append "big" op) op))
	 (sub (big-subscript b))
	 (sup (big-supscript b))
	 (body (big-body b))
	 (l (cond ((and sub sup) (list sub sup))
		  (sub (list sub))
		  (else (list)))))
    (cond ((or (== op "intlim") (== op "int") (== op "oint"))
	   ;; not sure about oint, but the rest is supported by Pure/Reduce
	   (pure-big "intg" body l))
	  ((and sub sup (or (== op "sum") (== op "prod")))
	   ;; supported by Pure/Reduce if both lower and upper bound is given
	   (pure-big op body l))
	  ;; anything else is assumed to be a Pure aggregate which translates
	  ;; to a list comprehension
	  (else (pure-comp op body l)))))

(define (pure-check-tag? s t)
  (cond ((null? s) #f)
	((string? s) (string-contains? s t))
	((and (list? s)
	      ;; recursively descend into the last argument
	      (if (nnull? (cdr s))
		  (pure-last-token? (cdr s) t)
		  (pure-last-token? (car s) t))))
	(else #f)))

(define (pure-last-token? s t)
  (cond ((null? s) #f)
	((string? s)
	 (let* ((cs (if (char-set-contains? char-set:letter+digit
					    (string-ref t 0))
			char-set:letter+digit
			(char-set-complement
			 (string->char-set ";<>()[]{}" char-set:whitespace))))
		(toks (string-tokenize s cs))
		(tok (if (null? toks) "" (car (last-pair toks)))))
	   (and (== t tok)
		(string-suffix? t (string-delete s char-set:whitespace)))))
	((and (list? s)
	      ;; recursively descend into the last argument
	      (if (nnull? (cdr s))
		  (pure-last-token? (cdr s) t)
		  (pure-last-token? (car s) t))))
	(else #f)))

(define (pure-is-space? arg)
  (or (func? arg 'space)
      (and (func? arg 'text) (null? (cdr arg)) (string? (car arg))
	   (string-every char-set:whitespace (car arg)))
      (and (string? arg) (string-every char-set:whitespace arg))))

(define (pure-skip-space args)
  (if (or (null? args) (not (pure-is-space? (car args)))) args
      (pure-skip-space (cdr args))))

(define (pure-concat args)
  ;; (format #t "concat: ~s\n" args)
  (cond ((null? args) (noop))
	;; This rule brings limits into a more digestible form. This isn't
	;; 100% fool-proof but it's the best that we can do since TeXmacs has
	;; no special markup for limits.
	((and (pure-last-token? (car args) "lim")
	      (func? (cadr args) 'rsub)
	      (with s (cadr (cadr args))
		    (and (string? s) (string-contains s "<rightarrow>"))))
	 (plugin-input (car args))
	 (display " (")
	 (with s (cadr (cadr args))
	       (plugin-input (string-replace s "<rightarrow>" ") (")))
	 (display ") ")
	 (pure-concat (cddr args)))
	;; These rules are used to format <choice> expressions.
	((and (pure-last-token? (car args) "of")
	      ;; look ahead, skipping over space, to see whether <choice>
	      ;; follows
	      (with l (pure-skip-space (cdr args))
		    (and (nnull? l) (func? (car l) 'choice))))
	 (plugin-input (car args))
	 (set! pure-inside-case? #t)
	 (pure-concat (cdr args)))
	((and (pure-last-token? (car args) "=")
	      ;; look ahead, skipping over space, to see whether <choice>
	      ;; follows
	      (with l (pure-skip-space (cdr args))
		    (and (nnull? l) (func? (car l) 'choice))))
	 (plugin-input (car args))
	 (set! pure-inside-eqn? #t)
	 (pure-concat (cdr args)))
	(else
	 (plugin-input (car args))
	 (pure-concat (cdr args)))))

;; XXXTODO: We'd really like to open this up to the user so that he has an
;; easy way of specifying his own conversions which take precedence over the
;; ones given here.

(plugin-input-converters pure
;; Only change these if you know what you are doing. Many special constructs
;; are defined here.
  (concat pure-concat)
  (tree pure-tree)
  (rows pure-rows)
  (tabular pure-matrix)
  (tabular* pure-matrix)
  (block pure-matrix)
  (block* pure-matrix)
  (matrix pure-matrix)
  (det pure-det)
  (eqnarray pure-stack)
  (eqnarray* pure-stack)
  (equation pure-descend-last)
  (equation* pure-descend-last)
  (stack pure-stack)
  (choice pure-choice)
  (binom pure-binom)
  (sqrt pure-sqrt)
  (big-around pure-big-around)
  (around pure-around)
  (around* pure-around)
  (space pure-space)
  (mid pure-math)
  (really-tiny pure-math)
  (tiny pure-math)
  (very-small pure-math)
  (small pure-math)
  (normal-size pure-math)
  (large pure-math)
  (very-large pure-math)
  (huge pure-math)
  (really-huge pure-math)
  (math-bf pure-math)
  (math-it pure-math)
  (math-sl pure-math)
  (math-ss pure-math)
  (math-tt pure-math)
  (math-up pure-math)
  (rprime pure-rprime)
  (above pure-above)
  (below pure-below)
  (lsub pure-lsub)
  (lsup pure-lsup)
  (rsub pure-rsub)
  (rsup pure-rsup)
  (wide pure-wide)
  (frac pure-frac)
  (dfrac pure-frac)
  (tfrac pure-frac)
  (frac* pure-frac)
  (neg pure-neg)

;; Stuff below you might want to change to add your own symbol mappings or
;; adjust the ones given here. Some the symbols are pilfered from the generic
;; converter, others I gleaned from the menus or by looking at the tree form
;; of math formulas. There's surely some stuff missing here. Note that many of
;; these symbols aren't defined in Pure by default, so you can give them any
;; meaning that you want.

  ;; These all have a predefined meaning in Pure (except matheuler). The e, pi
  ;; and i symbol will only be defined if the math module is loaded, however.
  ("<backslash>"  "\\")
  ("<infty>"      " inf ")
  ("<mathe>"      " e ")
  ("<mathpi>"     " pi ")
  ("<mathi>"      " i ")
  ("<matheuler>"  " matheuler ")
  ;; Differentials. These are taken care of in texmacs.pure.
  ("<partial>" " d ")
  ("<mathd>" " d ")

  ("<longequal>" "==") ;; equality in Pure
  ("<assign>" ":=")
  ("<plusassign>" "+=")
  ("<minusassign>" "-=")
  ("<timesassign>" "*=")
  ("<overassign>" "/=")
  ("<lflux>" "<<")
  ("<gflux>" ">>")

  ("<implies>" "=<gtr>")
  ("<Rightarrow>" "=<gtr>")
  ("<Leftrightarrow>" "<less>=<gtr>")
  ("<neg>" "~") ; negation symbol
  ("<sim>" "~") ; tilde
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

  ;; different kinds of brackets and delimiters
  ("<lfloor>" "floor (")
  ("<rfloor>" ")")
  ("<lceil>" "ceil (")
  ("<rceil>" ")")
  ("<langle>" "(")
  ("<rangle>" ")")
  ("<llbracket>" "[")
  ("<rrbracket>" "]")
  ("<nobracket>" " ")
  ;; what the heck are these good for??
  ("<nocomma>" " ")
  ("<nospace>" " ")
  ("<comma>" ",")

  ("<um>" "-")
  ("<upl>" "") ; unary plus not supported in Pure
  ("<circ>" ".") ; function composition in Pure
  ("<colons>" "::")
  ("<div>" "%")
  ("<times>" "*")
  ("<ast>" "*")
  ("<cdot>" "*")
  ("<ldots>" "..")

;; Here are a few other operators that might be useful. This list is probably
;; incomplete, add others as needed. Note that none of these is predefined in
;; Pure, so you'll have to declare them as infix symbols if you want to use
;; them.

  ("<pm>" " pm ")
  ("<mp>" " mp ")
  ("<in>" " in ")
  ("<angle>" " angle ")
  ("<star>" " star ")
  ("<bullet>" " bullet ")
  ("<cdots>" " cdots ")
  ("<hdots>" " hdots ")
  ("<vdots>" " vdots ")
  ("<ddots>" " ddots ")
  ("<udots>" " udots ")
  ("<flat>" "flat ")
  ("<natural>" "natural ")
  ("<sharp>" "sharp ")
  ("<forall>" "forall ")
  ("<exists>" "exists ")
  ("<curlywedge>" " curlywedge ")
  ("<curlyvee>" " curlyvee ")
  ("<barwedge>" " barwedge ")
  ("<veebar>" " veebar ")
  ("<vdash>" " vdash ")
  ("<Vdash>" " Vdash ")
  ("<Vvdash>" " Vvdash ")
  ("<vDash>" " vDash ")
  ("<dashv>" " dashv ")
  ("<vdash>" " vdash ")
  ("<cap>" " cap ")
  ("<cup>" " cup ")
  ("<sqcap>" " sqcap ")
  ("<sqcup>" " sqcup ")
  ("<uplus>" " uplus ")
  ("<oplus>" " oplus ")
  ("<ominus>" " ominus ")
  ("<otimes>" " otimes ")
  ("<oslash>" " oslash ")
  ("<odot>" " odot ")
  ("<obar>" " obar ")
  ("<varocircle>" " varocircle ")
  ("<circledast>" " circledast ")
  ("<boxplus>" " boxplus ")
  ("<boxminus>" " boxminus ")
  ("<boxtimes>" " boxtimes ")
  ("<boxslash>" " boxslash ")
  ("<boxbox>" " boxbox ")
  ("<boxbar>" " boxbar ")
  ("<boxast>" " boxast ")

  ;; accents
  ("<bar>" "bar ")
  ("<vect>" "vect ")
  ("<check>" "check ")
  ("<breve>" "breve ")
  ("<dot>" "dot ")
  ("<ddot>" "ddot ")
  ("<accute>" "accute ")
  ("<grave>" "grave ")

;; Special glyphs available in TeXmacs. Unicode actually has equivalents for
;; most of these in the MathML character set, which may be used as identifier
;; constituents in Pure. Unfortunately, those code points are different from
;; what TeXmacs uses internally, so it won't display the MathML characters
;; correctly if Pure sends them. Thus, in order to make these glyphs just work
;; without causing too much havoc, for the time being we map them to ordinary
;; Latin letters instead. Note that this means that Pure won't be able to
;; distinguish, say, 𝔄, 𝓐 or 𝔸 from A, so these will all denote the same
;; identifier in Pure.

  ("<bbb-A>" "AA")
  ("<bbb-B>" "BB")
  ("<bbb-C>" "CC")
  ("<bbb-D>" "DD")
  ("<bbb-E>" "EE")
  ("<bbb-F>" "FF")
  ("<bbb-G>" "GG")
  ("<bbb-H>" "HH")
  ("<bbb-I>" "II")
  ("<bbb-J>" "JJ")
  ("<bbb-K>" "KK")
  ("<bbb-L>" "LL")
  ("<bbb-M>" "MM")
  ("<bbb-N>" "NN")
  ("<bbb-O>" "OO")
  ("<bbb-P>" "PP")
  ("<bbb-Q>" "QQ")
  ("<bbb-R>" "RR")
  ("<bbb-S>" "SS")
  ("<bbb-T>" "TT")
  ("<bbb-U>" "UU")
  ("<bbb-V>" "VV")
  ("<bbb-W>" "WW")
  ("<bbb-X>" "XX")
  ("<bbb-Y>" "YY")
  ("<bbb-Z>" "ZZ")

  ("<bbb-a>" "aa")
  ("<bbb-b>" "bb")
  ("<bbb-c>" "cc")
  ("<bbb-d>" "dd")
  ("<bbb-e>" "ee")
  ("<bbb-f>" "ff")
  ("<bbb-g>" "gg")
  ("<bbb-h>" "hh")
  ("<bbb-i>" "ii")
  ("<bbb-j>" "jj")
  ("<bbb-k>" "kk")
  ("<bbb-l>" "ll")
  ("<bbb-m>" "mm")
  ("<bbb-n>" "nn")
  ("<bbb-o>" "oo")
  ("<bbb-p>" "pp")
  ("<bbb-q>" "qq")
  ("<bbb-r>" "rr")
  ("<bbb-s>" "ss")
  ("<bbb-t>" "tt")
  ("<bbb-u>" "uu")
  ("<bbb-v>" "vv")
  ("<bbb-w>" "ww")
  ("<bbb-x>" "xx")
  ("<bbb-y>" "yy")
  ("<bbb-z>" "zz")

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
