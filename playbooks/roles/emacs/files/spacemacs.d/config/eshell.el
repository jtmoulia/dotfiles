(require 'em-tramp)
(require 'esh-module)

;; Add tramp to the list of modules loaded on eshell start.
;; (required for `sudo' in Emacs 26)
(add-to-list 'eshell-modules-list 'eshell-tramp)

;; Use 'completion-at-point for tab completion
(add-hook 'eshell-mode-hook
          '(lambda ()
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

;; eshell helper functions
(defun eshell-here ()
  (interactive)
  (let ((current-dir (file-name-directory (buffer-file-name (current-buffer))))
        (eshell-buffer (get-buffer eshell-buffer-name)))
    (switch-to-buffer eshell-buffer)
    (cd current-dir)
    ;; this makes a pretty big assumption re the input being empty
    (eshell-send-input)
    ))

