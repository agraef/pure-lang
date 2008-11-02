;; Some convenient definitions to communicate with a pure-remote control. This
;; requires that you have pdsend installed.

;; Add this to your .emacs file.

(defun pd-send-start-process ()
  "Starts a process to communicate with Pd via UDP port 4711."
  (interactive)
  (start-process "pdsend" nil "pdsend" "4711" "localhost" "udp")
  (process-kill-without-query (get-process "pdsend")))

(defun pd-send-reload ()
  "Send a reload message to Pd."
  (interactive)
  (process-send-string "pdsend" "reload\n"))

(defun pd-send-start ()
  "Send a play 1 message to Pd."
  (interactive)
  (process-send-string "pdsend" "play 1\n"))

(defun pd-send-stop ()
  "Send a play 0 message to Pd."
  (interactive)
  (process-send-string "pdsend" "play 0\n"))

(defun pd-send-restart ()
  "Send play 0, play 1 messages to Pd."
  (interactive)
  (process-send-string "pdsend" "play 0\n")
  (process-send-string "pdsend" "play 1\n"))

(pd-send-start-process)
(global-set-key "\C-c\C-x" 'pd-send-reload)
(global-set-key "\C-c\C-s" 'pd-send-start)
(global-set-key "\C-c\C-t" 'pd-send-stop)
(global-set-key "\C-c\C-g" 'pd-send-restart)
