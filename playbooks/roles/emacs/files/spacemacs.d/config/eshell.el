(require 'em-tramp)
(require 'esh-module)

;; Use 'completion-at-point for tab completion
(add-hook 'eshell-mode-hook
          '(lambda ()
             (add-to-list 'eshell-modules-list 'eshell-tramp)
             (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
             ))

(setq
 eshell-prefer-lisp-functions t
 eshell-prefer-lisp-variables t
 password-cache t
 password-cache-expiry (* 60 10)
 )

;; eshell aliases
;; (eshell/alias "rm" "echo 'rm disabled. try trash-{put,rm} or *rm to force.'")

(defun* personal//eshell-send-command
    (command
     &key
     (buffer (get-buffer eshell-buffer-name))
     (send-input t))
  "Send the COMMAND to the eshell BUFFER.

This will kill any existing input."
  (switch-to-buffer buffer)
  (goto-char (point-max))
  ;; Should this be optional?
  (eshell-kill-input)
  (insert command)
  (if send-input (eshell-send-input)))

(defun personal//is-eshell-buffer (buffer-pair)
  "Return whether the `(name . buffer)` BUFFER-PAIR refers to an eshell buffer."
  (with-current-buffer (cdr buffer-pair) (eq major-mode 'eshell-mode)))

(defun personal//get-eshell-buffer (&optional prefixed)
  "Get the eshell buffer, ask the user for the name if PREFIXED."
  (let (eshell-buffer (get-buffer eshell-buffer-name))
    (if prefixed
        (read-buffer "eshell buffer" eshell-buffer t 'personal//is-eshell-buffer)
      eshell-buffer)))

;; eshell helper functions
(defun eshell-cd (directory &optional prefixed)
  "Change the eshell's present working directory to the BUFFER's directory."
  (interactive "fChange directory to: \nP")
  (switch-to-buffer (personal//get-eshell-buffer prefixed))
  (cd directory)
  ;; this makes a pretty big assumption re the input being empty
  (eshell-send-input))

;; eshell helper functions
(defun eshell-cd-here (&optional prefixed)
  "Change the eshell's present working directory to the BUFFER's directory."
  (interactive "P")
  (eshell-cd (current-dir (file-name-directory (buffer-file-name (current-buffer)))) prefixed))

(defun eshell-send-command (command &optional prefixed)
  "Interactive command to send the COMMAND to the eshell BUFFER.

Wraps `personal//eshell-send-command`."
  (interactive "seshell command: \nP")
  (let ((eshell-buffer (personal//get-eshell-buffer prefixed))
        (send-input (if prefixed (y-or-n-p "send input") t)))
    (personal//eshell-send-command command :buffer eshell-buffer :send-input send-input)))

(spacemacs/set-leader-keys "aeh" 'eshell-cd-here)
(spacemacs/set-leader-keys "aec" 'eshell-cd)
(spacemacs/set-leader-keys "ae:" 'eshell-send-command)
