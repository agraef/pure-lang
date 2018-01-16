;;; faust-remote.el --- Pd remote control stuff.

;;; Commentary:

;;; Pure mode also has this, but we want it in Faust mode, too.  Also, this
;;; adds the pd-faust-compile command which lets you quickly compile a Faust
;;; source without a Makefile using the faust2pure script.

;;; Install this anywhere where Emacs finds it (e.g., in the Emacs site-lisp
;;; directory -- usually under /usr/share/emacs/site-lisp on Un*x systems, or
;;; in any directory on the Emacs load-path) and load it in your .emacs as
;;; follows:

;;; (require 'faust-remote)

;;; Code:

(defun pd-send-start-process ()
  "Start a pdsend process to communicate with Pd via UDP port 4711."
  (interactive)
  (start-process "pdsend" nil "pdsend" "4711" "localhost" "udp")
  (process-kill-without-query (get-process "pdsend")))

(defun pd-send-stop-process ()
  "Stops a previously started pdsend process."
  (interactive)
  (delete-process "pdsend"))

(defun pd-send-message (message)
  "Send the given MESSAGE to Pd.  Start the pdsend process if needed."
  (interactive "sMessage: ")
  (unless (get-process "pdsend") (pd-send-start-process))
  (process-send-string "pdsend" (concat message "\n")))

;; Some utilities useful for compiling Faust programs for use with pd-faust.

;; The following enables Emacs to properly recognize error messages from the
;; Faust compiler.
(require 'compile)
(add-hook 'faust-mode-hook (lambda ()
    (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\) : \\([0-9]+\\) : ERROR : .*$" 1 2))
    ))

;; This compiles Faust programs using the faust2pure script (available in
;; recent Faust versions or in the pure-faust package).
(defvar pd-faust-compile-command "faust2pure ")
(defun pd-faust-compile (command)
  "Compile the Faust program in the current buffer.
Like \\[compile], this runs COMMAND, a shell command, in a
separate process asynchronously with output going to the buffer
`*compilation*'.  By default, this runs the `faust2pure' script,
which compiles the Faust source to a Pure module loadable with
pd-faust."
  (interactive
   (list
    (let ((command (eval pd-faust-compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))))
  (unless (equal command (eval pd-faust-compile-command))
    (setq pd-faust-compile-command command))
  (setq command
	(if (string-match "[ \t\n\r]+\\'" command)
	    (replace-match "" t t command)
	  command))
  (save-some-buffers)
  (compilation-start
   (concat
    command " "
    (shell-quote-argument (file-relative-name (buffer-file-name))))))

;; Faust mode; this requires Juan Romero's Faust mode available at
;; https://github.com/rukano/emacs-faust-mode
(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))
(autoload 'faust-mode "faust-mode" "FAUST editing mode." t)

;; Juan's Faust mode doesn't have a local keymap, add one.
(defvar faust-mode-map nil)
(cond
 ((not faust-mode-map)
  (setq faust-mode-map (make-sparse-keymap))
  ;; Some convenient keybindings for Faust mode. NOTE: The first message needs
  ;; a custom pure-remote to work. See faust-remote.pd.
  (define-key faust-mode-map "\C-c\C-k" 'pd-faust-compile)
  (define-key faust-mode-map "\C-c\C-m" 'pd-send-message)
  (define-key faust-mode-map "\C-c\C-x" '(lambda () "Reload" (interactive)
					   (pd-send-message "dspreload")))
  (define-key faust-mode-map "\C-c\C-s" '(lambda () "Start" (interactive)
					   (pd-send-message "play 1")))
  (define-key faust-mode-map "\C-c\C-t" '(lambda () "Stop" (interactive)
					   (pd-send-message "play 0")))
  (define-key faust-mode-map "\C-c\C-g" '(lambda () "Restart" (interactive)
					   (pd-send-message "play 0")
					   (pd-send-message "play 1")))
  (define-key faust-mode-map [(control ?\/)] '(lambda () "Dsp On" (interactive)
						(pd-send-message "pd dsp 1")))
  (define-key faust-mode-map [(control ?\.)] '(lambda () "Dsp Off" (interactive)
						(pd-send-message "pd dsp 0")))
  ))
(add-hook 'faust-mode-hook '(lambda () (use-local-map faust-mode-map)))

(provide 'faust-remote)
;;; faust-remote.el ends here
