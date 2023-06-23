;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/chatgpt-shell.el -*- lexical-binding: t; -*-

;; Load OpenAI API key from .authinfo.gpg
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

;; Fix the ChatGPT shell to vsn 4 with function calling
;; NOTE: function calling isn't supported by chatgpt-shell (yet)
(setq chatgpt-shell-model-version "gpt-4-0613")

;; HACK: for some reason straight build doesn't include ob-chatgpt-shell. So,
;; instead we add the repo dir to the load-path :shrug:
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/chatgpt-shell")
(require 'ob-chatgpt-shell)
(ob-chatgpt-shell-setup)

(setq org-babel-default-header-args:chatgpt-shell
      `((:results . "code")
        (:version . "gpt-4-0613")
        (:system . ,(a-get chatgpt-shell-system-prompts "Programming"))
        (:temperature . 0)
        (:wrap . "EXPORT markdown")))
