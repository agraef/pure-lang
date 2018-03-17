;;; flycheckpure.el --- flycheck checker for Pure

;; Copyright (c) 2018, Albert Graef
;; All rights reserved.

;; Author: Albert Graef <aggraef@gmail.com>
;; Keywords: convenience, tools, pure
;; Version: 0.1.0
;; URL: https://github.com/agraef/pure-lang
;; Package-Requires: ((emacs "24") (flycheck "0.22"))

;; This file is not part of GNU Emacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; Flycheck checker for Pure (http://www.flycheck.org)

;; To install, copy this file to some directory on your load-path (e.g.,
;; /usr/share/emacs/site-lisp), make sure that you have flycheck installed and
;; enable the Pure checker in your .emacs as follows:

;; (require 'flycheck-pure)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-pure-setup))

;;; Code:

(require 'flycheck)

(flycheck-def-option-var flycheck-pure-warnings t pure
  "Enables warnings (t by default)."
  :safe #'booleanp
  :type 'boolean)

(flycheck-define-checker pure
  "A Pure syntax checker using the Pure interpreter.

See URL `https://agraef.github.io/pure-lang/'."
  :command ("pure" "--check" (option-flag "-w" flycheck-pure-warnings) source)
  :error-patterns
  ((warning line-start (file-name) ", line " line ": warning: " (message) line-end)
   (error line-start (file-name) ", line " line ": " (message) line-end))
  :modes pure-mode)

;;;###autoload
(defun flycheck-pure-setup ()
  "Flycheck Pure Setup.
Add `pure' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'pure))

(provide 'flycheck-pure)

;;; flycheck-pure.el ends here
