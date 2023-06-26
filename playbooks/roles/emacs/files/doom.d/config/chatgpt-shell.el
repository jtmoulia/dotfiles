;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/chatgpt-shell.el -*- lexical-binding: t; -*-
(require 'a)

;; HACK: for some reason straight build doesn't include ob-chatgpt-shell. So,
;; instead we add the repo dir to the load-path :shrug:
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/chatgpt-shell")
(require 'ob-chatgpt-shell)
(ob-chatgpt-shell-setup)

;; The "Prorg" prompt just uses the "Programming" prompt with org-mode formatting
(add-to-list 'chatgpt-shell-system-prompts
             `("Prorg" . ,(string-replace
                           "markdown" "org-mode markup"
                           (a-get chatgpt-shell-system-prompts "Programming"))))

;; Load OpenAI API key from .authinfo.gpg
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

;; Fix the ChatGPT shell to vsn 4 with function calling
;; NOTE: function calling isn't supported by chatgpt-shell (yet)
(setq chatgpt-shell-model-version "gpt-4-0613")

;; Use the programming prompt in the shell as it plays well with the formatting
(setq chatgpt-shell-system-prompt
      (cl-position "Programming" chatgpt-shell-system-prompts :key #'car :test #'equal))

;; Use the "Prorg" prompt for org-mode source blocks
(setq org-babel-default-header-args:chatgpt-shell
      `((:results . "raw")
        (:version . "gpt-4-0613")
        (:system . ,(a-get chatgpt-shell-system-prompts "Prorg"))
        (:temperature . 0)))
