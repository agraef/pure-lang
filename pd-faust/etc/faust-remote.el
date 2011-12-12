
; Pd remote control stuff. Pure mode also has this, but we want it in Faust
; mode, too.
(defun pd-send-start-process ()
  "Starts a pdsend process to communicate with Pd via UDP port 4711."
  (interactive)
  (start-process "pdsend" nil "pdsend" "4711" "localhost" "udp")
  (process-kill-without-query (get-process "pdsend")))

(defun pd-send-stop-process ()
  "Stops a previously started pdsend process."
  (interactive)
  (delete-process "pdsend"))

(defun pd-send-message (message)
  "Send a given message to Pd. Start the pdsend process if needed."
  (interactive "sMessage: ")
  (unless (get-process "pdsend") (pd-send-start-process))
  (process-send-string "pdsend" (concat message "\n")))

; Faust mode; this requires Juan Romero's Faust mode available at
; https://github.com/rukano/emacs-faust-mode
(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))
(autoload 'faust-mode "faust-mode" "FAUST editing mode." t)

; Juan's Faust mode doesn't have a local keymap, add one.
(defvar faust-mode-map nil)
(cond
 ((not faust-mode-map)
  (setq faust-mode-map (make-sparse-keymap))
  ;; Some convenient keybindings for Faust mode. NOTE: The first message needs
  ;; a custom pure-remote to work. See faust-remote.pd.
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
