;; Some Elisp functions to communicate with a pure-remote control. To make
;; this work, you need to have pdsend installed (it's normally included with
;; Pd), and your patch must contain an instance of the accompanying
;; pure-remote.pd abstraction. Please see the pd-pure documentation for
;; details.

;; NOTE: If you're running pure-mode then you don't need these definitions,
;; because pure-mode already provides equivalent functionality, which can be
;; enabled by going to the pure-mode customization buffer (option "Customize"
;; in the "Pure" menu) and setting the "Pd Pure Support" option. If you don't
;; want to run Pure mode but still want to be able to use these bindings in
;; Emacs, you can simply add the following lines to your .emacs file instead.

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

;; Some convenient keybindings.

(global-set-key "\C-c\C-m" 'pd-send-message)
(global-set-key "\C-c\C-x" '(lambda () "Quick Reload" (interactive)
			      (pd-send-message "bang")))
(global-set-key "\C-c\M-x" '(lambda () "Full Reload" (interactive)
			      (pd-send-message "reload")))
(global-set-key "\C-c\C-s" '(lambda () "Start" (interactive)
			      (pd-send-message "play 1")))
(global-set-key "\C-c\C-t" '(lambda () "Stop" (interactive)
			      (pd-send-message "play 0")))
(global-set-key "\C-c\C-g" '(lambda () "Restart" (interactive)
			      (pd-send-message "play 0")
			      (pd-send-message "play 1")))
(global-set-key [(control ?\/)] '(lambda () "Dsp On" (interactive)
				   (pd-send-message "pd dsp 1")))
(global-set-key [(control ?\.)] '(lambda () "Dsp Off" (interactive)
				   (pd-send-message "pd dsp 0")))
