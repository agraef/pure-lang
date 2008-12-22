;; Some Elisp functions to communicate with a pure-remote control. To make
;; this work, you need to have pdsend installed (it's normally included with
;; Pd), and your patch must contain an instance of the accompanying
;; pure-remote.pd abstraction.

;; Add this to your .emacs file.

(defun pd-send-start-process ()
  "Starts a process to communicate with Pd via UDP port 4711."
  (interactive)
  (start-process "pdsend" nil "pdsend" "4711" "localhost" "udp")
  (process-kill-without-query (get-process "pdsend")))

(defun pd-send-message (message)
  "Send a given message to Pd."
  (interactive "sMessage: ")
  (process-send-string "pdsend" (concat message "\n")))

(defun pd-send-dsp-on ()
  "Send a 'dsp 1' message to Pd."
  (interactive)
  (pd-send-message "pd dsp 1"))

(defun pd-send-dsp-off ()
  "Send a 'dsp 0' message to Pd."
  (interactive)
  (pd-send-message "pd dsp 0"))

(defun pd-send-bang ()
  "Send a 'bang' message to Pd."
  (interactive)
  (pd-send-message "bang"))

(defun pd-send-start ()
  "Send a 'play 1' message to Pd."
  (interactive)
  (pd-send-message "play 1"))

(defun pd-send-stop ()
  "Send a 'play 0' message to Pd."
  (interactive)
  (pd-send-message "play 0"))

(defun pd-send-restart ()
  "Send 'play 0, play 1' messages to Pd."
  (interactive)
  (pd-send-message "play 0")
  (pd-send-message "play 1"))

;; Some convenient keybindings.

(pd-send-start-process)
(global-set-key "\C-c\C-x" 'pd-send-bang)
(global-set-key "\C-c\C-m" 'pd-send-message)
(global-set-key "\C-c\C-s" 'pd-send-start)
(global-set-key "\C-c\C-t" 'pd-send-stop)
(global-set-key "\C-c\C-g" 'pd-send-restart)
(global-set-key "\C-c1" 'pd-send-dsp-on)
(global-set-key "\C-c0" 'pd-send-dsp-off)
