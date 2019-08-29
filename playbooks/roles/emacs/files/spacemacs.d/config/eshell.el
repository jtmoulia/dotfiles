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
(when (boundp 'eshell/alias)
  (eshell/alias "rm" "echo 'rm disabled. try trash-{put,rm} or *rm to force.'")
  (eshell/alias "sudo" "eshell/sudo $*"))

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
  (let ((eshell-buffer (get-buffer eshell-buffer-name)))
    (if prefixed
        (read-buffer "eshell buffer" eshell-buffer t 'personal//is-eshell-buffer)
      eshell-buffer)))

;; eshell helper functions
(defun eshell-cd (target &optional prefixed)
  "Change the eshell's present working directory to the BUFFER's directory."
  (interactive "fChange directory to: \nP")
  (let* ((expanded-target (expand-file-name target))
         (directory (if (and (file-exists-p expanded-target)
                             (not (file-directory-p expanded-target)))
                       (file-name-directory expanded-target)
                     expanded-target)))
    (switch-to-buffer (personal//get-eshell-buffer prefixed))
    (cd directory)
    ;; this makes a pretty big assumption re the input being empty
    (eshell-send-input)
    ))

;; eshell helper functions
(defun personal-eshell/cd (directory &optional prefixed)
  "Change the eshell's present working directory to the BUFFER's directory."
  (interactive "fChange directory to: \nP")
  (switch-to-buffer (personal//get-eshell-buffer prefixed))
  (cd directory)
  ;; this makes a pretty big assumption re the input being empty
  (eshell-send-input))

;; eshell helper functions
(defun personal-eshell/cd-here (&optional prefixed)
  "Change the eshell's present working directory to the BUFFER's directory."
  (interactive "P")
  (personal-eshell/cd (current-dir (file-name-directory (buffer-file-name (current-buffer)))) prefixed))

(defun personal-eshell/send-command (command &optional prefixed)
  "Interactive command to send the COMMAND to the eshell BUFFER.

Wraps `personal//eshell-send-command`."
  (interactive "seshell command: \nP")
  (let ((eshell-buffer (personal//get-eshell-buffer prefixed))
        (send-input (if prefixed (y-or-n-p "send input") t)))
    (personal//eshell-send-command command :buffer eshell-buffer :send-input send-input)))

(defun personal-eshell/clear ()
  "Interactive command to clear the eshell buffer. This is useful as eshell
can slow as the buffer gets enormous.

An alternative is `aweshell-clear-buffer'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; Display notification for long-running commands
;; TODO: should be keyed by buffer
(defvar personal-eshell-time-command-minimum 10
  "The minimum command time in seconds that will initiate a notification.")


(defun personal-eshell//get-input ()
  "Get the input from the active eshell buffer."
  (buffer-substring eshell-last-output-end (point-max)))

(defvar tsts nil)
(let (personal-eshell--time-command-start-time
      personal-eshell--time-command-old)

  (defun personal-eshell//pre-time-command-hook ()
    "Record the time the command was started into `personal-eshell--command-start-time'"
    (setf
     personal-eshell--time-command-start-time (float-time)
     personal-eshell--time-command-old (personal-eshell//get-input)
     tsts (personal-eshell//get-input)
     ))

  (defun personal-eshell//post-time-command-hook ()
    "If the command was long-running provide a notification.
See `personal-eshell-time-command-minimum' for min time to provide a notification."
    (when personal-eshell--time-command-start-time
      (let ((runtime (- (float-time) personal-eshell--time-command-start-time)))
        (when (>= runtime personal-eshell-time-command-minimum)
          (notifications-notify
           :title "eshell command complete"
           :body (format "%s\nran in %ds" personal-eshell--time-command-old runtime))))
      (setf personal-eshell--time-command-start-time nil))))


(remove-hook 'eshell-post-command-hook 'personal-eshell-pre-time-command-hook)
(add-hook 'eshell-pre-command-hook 'personal-eshell//pre-time-command-hook)
(add-hook 'eshell-post-command-hook 'personal-eshell//post-time-command-hook)

(spacemacs/set-leader-keys "aeh" 'personal-eshell/cd-here)
(spacemacs/set-leader-keys "aec" 'personal-eshell/cd)
(spacemacs/set-leader-keys "ae:" 'personal-eshell/send-command)

(if (fboundp 'ivy-set-actions)
    (ivy-set-actions
     ;; TODO this shouldn't be bound globally
     t
     '(("c" personal-eshell/cd "eshell cd"))))

;; Configure aweshell
(let ((aweshell-path (expand-file-name "~/repos/aweshell")))
 (add-to-list 'load-path aweshell-path)
 (require 'aweshell)

 (face-spec-set 'epe-pipeline-delimiter-face `((t :foreground ,personal-colors-green)))
 (face-spec-set 'epe-pipeline-user-face `((t :foreground ,personal-colors-red)))
 (face-spec-set 'epe-pipeline-host-face `((t :foreground ,personal-colors-blue)))
 (face-spec-set 'epe-pipeline-time-face `((t :foreground ,personal-colors-yellow)))

 (spacemacs/set-leader-keys "ae'" 'aweshell-dedicated-toggle)
 (spacemacs/set-leader-keys "ae SPC" 'aweshell-next)
 (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
   "'" 'aweshell-dedicated-toggle)
 (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
   "c" 'aweshell-clear-buffer)
 (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
   "r" 'aweshell-search-history))

;; eshell.el ends here
