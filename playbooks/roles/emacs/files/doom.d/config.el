;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Private doom eamcs config

;; Let's dash this config. Dash is love. f it, too
(require 'dash)
(require 'f)

(defvar my-sync-dir "~/Sync"
  "Directory to store synchronized state to.")

;; Setup paths
(defun my//eval-config-after-load (mode config)
  "Helper for loading a file after a module is loaded."
  (let ((path (f-join "~/.doom.d" config)))
    (eval `(after! ,mode (load-file ,path)))))

;; Guix specific mu4e: Add mu4e to load path if the directory exists
(let ((mu4e-load-path (concat (getenv "GUIX_PROFILE") "/share/emacs/site-lisp/mu4e")))
  (if (file-directory-p mu4e-load-path)
      (add-to-list 'load-path mu4e-load-path)))

;; Use firefox for browsing
(setq browse-url-browser-function 'browse-url-firefox)

;; Evil config
;; TODO this should likely be ported to its own config file
(after! 'evil-escape
  ;; some heavy modes make the default delay of 0.15s too short
  (setq evil-escape-delay 0.5))

;; MacOS specific mu4e: Add mu4e to load path if the directory exists
(if (eq system-type 'darwin)
    (load-file (f-join "~/.doom.d" "config/darwin.el")))

(load-file (f-join "~/.doom.d" "config/text.el"))
(my//eval-config-after-load 'ansible "config/ansible.el")
(my//eval-config-after-load 'hyperbole "config/hyperbole.el")
(my//eval-config-after-load 'js-mode "config/js-mode.el")
(my//eval-config-after-load 'mu4e "config/mu4e.el")
(my//eval-config-after-load 'org "config/org.el")
(my//eval-config-after-load 'python "config/python.el")
(my//eval-config-after-load 'sql "config/sql.el")
(my//eval-config-after-load 'web-mode "config/web-mode.el")
(my//eval-config-after-load 'deft "config/deft.el")
(my//eval-config-after-load 'elfeed "config/elfeed.el")

;; Mastodon config, to refactor out
(after! 'mastodon
  (setq mastodon-media--enable-image-caching 't))
(setq mastodon-active-user "jtmoulia")
(setq mastodon-instance-url "https://mstdn.social")

(map! :leader :desc "Hyperbole" :n "o h" #'hyperbole)

;; global keybindings need to be defined at this top level
;; Add a global keybinding for opening deft
(map! :leader :desc "Open deft note search" :n "o n" #'deft)

;; Add a global keybinding for listing passwords with consult
(map! :leader :desc "List passwords" :n "o l" #'+pass/consult)

;; style and fonts: looking good, feeling good
(setq doom-theme 'doom-dracula)
(setq doom-font (cl-case system-type
                  ('darwin (font-spec :family "DejaVu Sans Mono" :size 14))
                  (otherwise (font-spec :family "DejaVu Sans Mono" :size 12))))
(setq doom-font-increment 1)

;; Spacemacs style `,' local leader.
(setq doom-localleader-key ",")

;; magit-status if in a project, otherwise find-file
(defun my//magit-or-find-file (project-path)
  "Project switcher will attmept to open magit-status otherwise use a
find-file picker."
  (require 'magit)
  (if (magit-get-current-branch)
      (magit-status)
    (doom-project-find-file project-path)))
(setq +workspaces-switch-project-function #'my//magit-or-find-file)

;; config: evil-snipe
;; allow evil-snipe to work the entire buffer as opposed to single line
(setq evil-snipe-scope 'buffer)
(setq evil-snipe-repeat-scope 'buffer)
;; If no matches are found spill over to the entire buffer, before and after
(setq evil-snipe-spillover-scope 'whole-buffer)

;; Hugo blahgin
(setq org-hugo-date-format "%Y-%m-%dT%T")

;; chatGPT
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))
(setq chatgpt-shell-model-version "gpt-4")

;; GitHub Copilot configuration
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
