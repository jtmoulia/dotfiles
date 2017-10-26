(defvar dotspacemacs-mu-root
  (expand-file-name "~/repos/mu")
  "mu's source path")
(defvar dotspacemacs-slack-el
  (expand-file-name (concat dotspacemacs-directory "/slack.el"))
  "the path to the slack elisp file")

(when (eq system-type 'gnu/linux)
  (setq
   browse-url-firefox-program "firefox-developer"
   browse-url-browser-function 'browse-url-firefox))

(setq dotspacemacs-additional-packages '(ob-ipython))

(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-configuration-layers
                '(
                  ; elixir
                  ; nginx
                  ivy
                  spacemacs
                  dash
                  csv
                  ansible
                  auto-completion
                  ; common-lisp
                  deft
                  emacs-lisp
                  git
                  html
                  javascript
                  ; itome-react  ; react
                  react
                  markdown
                  ;; nginx
                  org
                  prodigy
                  (python
                   :variables python-test-runner 'pytest)
                  ; rcirc
                  ; restclient
                  slack
                  spacemacs-layouts
                  ; sql
                  syntax-checking
                  typography
                  yaml
                  (mu4e :variables
                        mu4e-installation-path
                        (concat dotspacemacs-mu-root "/mu4e"))
                  ;; private layers
                  ; org-page
                  ; artist
                  ; lfe
                  )))

(defvar personal--web-indent
  2
  "Indentation across js, css, html")

(setq-default
 ;; general js + json
 js-indent-level personal--web-indent
 ;; js2-mode
 js2-basic-offset personal--web-indent
 ;; web-mode
 css-indent-offset personal--web-indent
 web-mode-markup-indent-offset personal--web-indent
 web-mode-css-indent-offset personal--web-indent
 web-mode-code-indent-offset personal--web-indent
 web-mode-attr-indent-offset personal--web-indent)

(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

;; erlang config
;; Use man pages initalled by homebrew when on OS X
(if (string-equal system-type "darwin")
    (setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang"))

(setq-default dotspacemacs-themes '(sanityinc-tomorrow-eighties
                                    grandshell
                                    leuven))


(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
   :channels ("#switchboard-dev" "#elixir-lang" "#emacs"))))


(setq-default dotspacemacs-default-font '("Source Code Pro"
                                          :size 26
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.1))

(defun dotspacemacs/user-config ()
  "Post spacemacs init config."
  ;; Yas fun
  (defun my-yas-get-first-name-from-to-field ()
    (let ((rlt "AGENT_NAME") str)
      (save-excursion
        (goto-char (point-min))
        ;; first line in email could be some hidden line containing NO to field
        (setq str (buffer-substring-no-properties (point-min) (point-max))))
      (if (string-match "^To: \"\\([^ ,]+\\)" str)
          (setq rlt (match-string 1 str)))
      (message "rlt=%s" rlt)
      rlt))

  ;; ivy
  (spacemacs/set-leader-keys
    "bo" 'ivy-switch-buffer-other-window)

  ;; dash
  (with-eval-after-load 'counsel-dash
    (add-hook 'python-mode
              (setq-local counsel-dash-docsets '("Python_3"))) )

  ;; eshell
  (with-eval-after-load 'eshell
    (require 'em-tramp)
    (setq eshell-prefer-lisp-functions t)
    (setq eshell-prefer-lisp-variables t)
    (setq password-cache t)
    (setq password-cache-expiry (* 60 10))
    ;; (eshell/alias "rm" "echo 'rm disabled. try trash-{put,rm} or *rm to force.'")
    )

  ;; Org
  (defvar personal//org-todo-file "~/ocloud/org/todo.org"
    "The path to the primary todo file.")

  (defvar personal//org-notes-file "~/ocloud/org/notes.org"
    "The path to the notes file.")

  (defvar personal//org-contacts-file "~/ocloud/org/contacts.org"
    "The path to the contacts file.")

  (setq org-capture-templates
        '(("t" "todo" entry (file+headline personal//org-todo-file "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n\n%a\n")))

  (with-eval-after-load 'org
   (add-to-list 'org-agenda-files personal//org-todo-file)
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (sql . t)
      (ipython . t)
      (emacs-lisp . t)
      ))
   )

  ;; Time tracking
  ;; (setq buffer-list-update-hook nil)
  (defvar personal--buffer-tracker-categories
    '((email . (lambda (buffer) (string-prefix-p "*mu4e-" (buffer-name buffer))))
      (other . (lambda (buffer) t)))
    "Categories for buffer tracking.")

  ;; (timeclock-change nil 'other)
  (defun personal//buffer-tracker-hook ()
    (let ((current-buffer (car (buffer-list))))
      (if current-buffer
          (let ((match (-first (lambda (category) (funcall (cdr category) current-buffer))
                               personal--buffer-tracker-categories)))
            (if project
                (if (and (boundp 'timeclock-last-event) (equal "i" (car timeclock-last-event)))
                    (timeclock-change nil project)
                  (timeclock-in nil project))))
          )))
      ; (message (buffer-name current-buffer))

  ;; (add-hook 'buffer-list-update-hook 'personal//buffer-tracker-hook)

  ;; deft
  (setq deft-directory "~/ocloud/org/notes")

  ;; magit
  (with-eval-after-load 'magit
    (setq magit-repository-directories `((,(expand-file-name "~/repos") . 1))))

  ;; Slack
  (with-eval-after-load 'slack
    (setq slack-prefer-current-team t)
    (spacemacs/set-leader-keys
      "aCr" 'slack-select-rooms
      "aCg" 'slack-group-select)
    (when (file-exists-p dotspacemacs-slack-el)
      (load-file dotspacemacs-slack-el)
      (mapcar (lambda (team) (apply 'slack-register-team team)) dotspacemacs-slack-teams)))

  (defun stump-notify (message)
    "Create a stump notification"
    (interactive "sNotification: ")
    (start-process "notifications-add" nil
                   "stumpish" "notifications-add" message))

  ;; Elfeed

  (with-eval-after-load 'elfeed
    (setq elfeed-feeds
          '("http://venturevalkyrie.com/rss")))
  ;; Alert
  (with-eval-after-load 'alert
    (alert-define-style 'stump :title "stumpwm notifications"
                        :notifier
                        (lambda (info)
                          (let ((title (plist-get info :title))
                                (mode (symbol-name (plist-get info :mode)))
                                (message (plist-get info :message)))
                            (stump-notify (concat title " [" mode "]: " message)))))
    (setq alert-default-style 'libnotify))

  ;; Python
  (with-eval-after-load 'python
    ;; (setq python-shell-interpreter "ipython"
    ;;       ; For IPython 5.x --
    ;;       python-shell-interpreter-args "--simple-prompt -i"
    ;;       )
    (setq python-shell-interpreter "ipython")

    (defun python-use-tabs ()
      (interactive)
      (setq indent-tabs-mode t
            tab-width 4
            py-indent-tabs-mode t))
    )


  ;; LFE
  (setq inferior-lfe-program-options '("-pa" "ebin"))

  ;; SQL
  (setq sql-connection-alist
        '(
          (postgres
           (sql-product 'postgres)
           (sql-user "postgres")
           (sql-server "localhost")
           (sql-database "postgres")
           (sql-port 5432))
          (mimic
           (sql-product 'postgres)
           (sql-user "mimic")
           (sql-server "localhost")
           (sql-database "mimic")
           (sql-port 5433))
          (biguns
           (sql-product 'postgres)
           (sql-user "ubuntu")
           (sql-server "localhost")
           (sql-database "deepdive_deepmed_ubuntu")
           (sql-port 5433))
          (htcs
           (sql-product 'postgres)
           (sql-user "htcs")
           (sql-server "localhost")
           (sql-database "htcs")
           (sql-port 5432))
          ))

  ;; tramp
  ;; (add-to-list 'tramp-default-proxies-alist
  ;;              '("ip-10-50-3-34.ec2.internal" nil "/ssh:ec2-user@34.198.67.93"))

  ;; Prodigy
  (setq personal--repos-dir (expand-file-name "~/repos"))

  (defvar personal--pyvenv-dir
    (expand-file-name "~/.virtualenvs")
    "Path to dir containing python venvs.")

  (defun personal--ensure-dir (path)
    (if (file-directory-p path)
        (file-name-as-directory path)
      (error "repo doesn't exist")))

  (defun personal--repo-path (repo)
    (let ((path (concat (file-name-as-directory personal--repos-dir) repo)))
      (personal--ensure-dir path)))

  (defun personal--pyvenv-path (venv)
    (let ((path (concat (file-name-as-directory personal--pyvenv-dir) venv)))
      (personal--ensure-dir path)))

  (defun personal--pyvenv-python (venv)
    (concat (personal--pyvenv-path venv) "bin/python"))

  ;; (prodigy-define-service
  ;;   :name "biguns:5432->localhost:5433"
  ;;   :command "ssh"
  ;;   :args '("-L" "5433:localhost:5432" "biguns" "-N")
  ;;   :tags '(:deepmed))
  ;; (prodigy-define-service
  ;;   :name "allscratch webpack"
  ;;   :command "yarn"
  ;;   :tags '(allscratch frontend)
  ;;   :args '("start")
  ;;   :cwd (concat spacemacs--repos-dir "/allscratch"))
  ;; (prodigy-define-service
  ;;   :name "allscratch dev backend"
  ;;   :tags '(allscratch backend)
  ;;   :cwd (concat spacemacs--repos-dir "/allscratch")
  ;;   :command "/home/jtmoulia/.virtualenvs/allscratch/bin/python"
  ;;   :args (list (concat spacemacs--repos-dir "/allscratch/bin/runflask"))
  ;;   :env '(("ALLSCRATCH_ENV" "dev")))
  ;; TODO
  (prodigy-define-service
    :name "healthtensor research tunnel"
    :tags '(healthtensor tunnel)
    :command "ssh"
    ;; :cwd
    :args (list "-F" (concat personal--repos-dir "/hippos/ssh.conf")
                "-L"))

  (prodigy-define-service
    :name "healthtensor local webserver"
    :tags '(healthtensor-local)
    :command (personal--pyvenv-python "cedars")
    :args '("-m" "flask" "run")
    :cwd (personal--repo-path "automed")
    :env '(("FLASK_DEBUG" "1")
           ("FLASK_APP" "automed/api.py")))

  (prodigy-define-service
    :name "healthtensor local webpack dev server"
    :tags '(healthtensor-local)
    :cwd (personal--repo-path "automed")
    :command  (concat (personal--repo-path "automed") "node_modules/.bin/webpack-dev-server"))
  )

(defun dotspacemacs/init ()
  "Init callback."
  (add-hook 'after-init-hook #'global-emojify-mode)
  (setq-default dotspacemacs-editing-style 'vim
                dotspacemacs-leader-key "SPC"
                dotspacemacs-emacs-leader-key "M-m"
                dotspacemacs-major-mode-leader-key ","
                dotspacemacs-command-key ":")

  (setq spacemacs-erlang-elixir-use-edts t))

(with-eval-after-load 'mu4e
  ;; Activate snippets for mu4e-view-mode
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (yas-minor-mode 1)
              (yas-reload-all 1)))

  ;; Configure Contexts
  (setq-default mu4e-contexts
   `(
     ,(make-mu4e-context
       :name "gmail"
       :enter-func
       (lambda ()
         (mu4e-message
          (concat "Switching to context: gmail")))
       :match-func
       (lambda (msg)
         (when msg
           (mu4e-message-contact-field-matches msg
                                               :to "jtmoulia@gmail.com")))
       :vars '((user-mail-address . "jtmoulia@gmail.com")
               (user-full-name . "Thomas Moulia")
               (mu4e-inbox-folder . "/gmail/INBOX")
               (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
               (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
               (mu4e-trash-folder . "/gmail/[Gmail].Trash")
               (mu4e-refile-folder . "/gmail/[Gmail].All Mail")
               (mu4e-spam-folder . "/gmail/[Gmail].spam")
               (smtpmail-smtp-user . "jtmoulia@gmail.com")
               (smtpmail-default-smtp-server . "smtp.gmail.com")
               (smtpmail-smtp-server . "smtp.gmail.com")
               (smtpmail-smtp-service . 465)))
     ,(make-mu4e-context
       :name "pocketknife"
       :enter-func
       (lambda ()
         (mu4e-message
          (concat "Switching to context: pocketknife")))
         :match-func
         (lambda (msg)
           (when msg
             (mu4e-message-contact-field-matches
              msg :to "jtmoulia@pocketknife.io")))
         :vars '((user-mail-address . "jtmoulia@pocketknife.io")
                 (user-full-name . "Thomas Moulia")
                 (mu4e-inbox-folder . "/pocketknife/INBOX")
                 (mu4e-sent-folder . "/pocketknife/INBOX.Sent Items")
                 (mu4e-drafts-folder . "/pocketknife/INBOX.Drafts")
                 (mu4e-trash-folder . "/pocketknife/INBOX.Trash")
                 (mu4e-refile-folder . "/pocketknife/INBOX.Archive")
                 (mu4e-spam-folder . "/pocketknife/INBOX.Junk Mail")
                 (smtpmail-smtp-user . "jtmoulia@pocketknife.io")
                 (smtpmail-default-smtp-server . "mail.messagingengine.com")
                 (smtpmail-smtp-server . "mail.messagingengine.com")
                 (smtpmail-stream-type . ssl)
                 (smtpmail-smtp-service . 465)))
     ,(make-mu4e-context
       :name "healthtensor"
       :enter-func
       (lambda ()
         (mu4e-message
          (concat "Switching to context: healthtensor")))
       :match-func
       (lambda (msg)
         (when msg
           (mu4e-message-contact-field-matches
            msg :to "thomas@healthtensor.com")))
       :vars '((user-mail-address . "thomas@healthtensor.com")
               (user-full-name . "Thomas Moulia")
               (mu4e-inbox-folder . "/healthtensor/INBOX")
               (mu4e-sent-folder . "/healthtensor/INBOX.Sent Items")
               (mu4e-drafts-folder . "/healthtensor/INBOX.Drafts")
               (mu4e-trash-folder . "/healthtensor/INBOX.Trash")
               (mu4e-refile-folder . "/healthtensor/INBOX.Archive")
               (mu4e-spam-folder . "/healthtensor/INBOX.Junk Mail")
               (smtpmail-smtp-user . "thomas@healthtensor.com")
               (smtpmail-default-smtp-server . "smtp.gmail.com")
               (smtpmail-smtp-server . "smtp.gmail.com")
               (smtpmail-smtp-service . 465)))
     ))

  ;; Helper Functions
  (defun dotspacemacs//mu4e-context (context-name)
    "Return the context in `mu4e-contexts' with name CONTEXT-NAME.

Raises an error if that context isn't present."
    (let* ((names (mapcar (lambda (context)
                            (cons (mu4e-context-name context) context))
                          mu4e-contexts))
           (context (cdr (assoc context-name names))))
      (if context
          context
        (error "no context with name: %s" context-name))))

  (defun dotspacemacs//mu4e-context-get-var (context var)
    "For CONTEXT return VAR. Helper function for access."
    (cdr (assoc var (mu4e-context-vars context))))

  (defun dotspacemacs//mu4e-context-var (context-name var)
    "Return the value of VAR for the context with name CONTEXT-NAME, searching
`mu4e-contexts'."
    (dotspacemacs//mu4e-context-get-var
     (dotspacemacs//mu4e-context context-name)
     var))

  (defun dotspacemacs//mu4e-contexts-var (var)
    "Return a list of the value for VAR across `mu4e-contexts'. If VAR is
undefined for a context, it will be filtered out."
    (delq nil
          (mapcar (lambda (context)
                    (dotspacemacs//mu4e-context-get-var context var))
                  mu4e-contexts)))

  (defun dotspacemacs//mu4e-add-maildir-prefix (maildir)
    "Add maildir: prefix to MAILDIR for mu queries."
    (concat "maildir:\"" maildir "\""))

  (defun dotspacemacs//flat-cat (&rest list)
    "Flatten and concatenate LIST."
    (apply 'concat (-flatten list)))

  (defun dotspacemacs//flat-cat-pose (sep &rest list)
      "Unabashed helper function to interpose SEP padded with
spaces into LIST. Return the padded result."
      (dotspacemacs//flat-cat
       (-interpose (concat " " sep " ") list)))

  (defun* dotspacemacs//mu4e-query
      (var &key (prefix "") (sep "AND"))
    (apply 'dotspacemacs//flat-cat-pose sep
           (-map (lambda (folder) (concat prefix folder))
                 (dotspacemacs//mu4e-contexts-var var))))

  ;; Configure Vars
  (require 'mu4e-contrib)
  ;; (require 'org-mu4e)
  (setq-default
   mu4e-mu-binary         "/usr/bin/mu"
   mu4e-maildir           "~/maildirs"            ;; top-level Maildir
   mu4e-confirm-quit      nil
   mu4e-get-mail-command  "offlineimap"
   mu4e-headers-skip-duplicates t
   mu4e-update-interval   600
   mu4e-compose-signature (apply 'concat (-interpose
                                          "  \n"
                                          '("Thomas Moulia"
                                            "Co-Founder & CTO | HealthTensor"
                                            "www.healthtensor.com"
                                            "jtmoulia.pocketknife.io")))
   mu4e-compose-dont-reply-to-self t
   mu4e-compose-complete-only-personal t
   mu4e-hide-index-messages nil
   mu4e-html2text-command 'mu4e-shr2text
   mu4e-user-mail-address-list (dotspacemacs//mu4e-contexts-var
                                'user-mail-address)
   ;; User info
   message-auto-save-directory (concat (file-name-as-directory mu4e-maildir)
                                       "drafts")
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-stream-type 'ssl
   smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
   ;; smtpmail-queue-mail t
   smtpmail-queue-dir  (expand-file-name "~/maildirs/queue/cur"))

  ;; drafts are saved as *message*-___
  (add-to-list 'auto-mode-alist '("\\*message\\*-+" . message-mode))

  ;; mu4e bookmarks -- this is the magic
  (let* ((maildir "maildir:")
         (not-maildir (concat "NOT " maildir))
         (not-spam (dotspacemacs//mu4e-query 'mu4e-spam-folder
                                            :prefix not-maildir))
         (not-trash (dotspacemacs//mu4e-query 'mu4e-trash-folder
                                              :prefix not-maildir))
         (not-refile (dotspacemacs//mu4e-query 'mu4e-refile-folder
                                               :prefix not-maildir)))
    (setq mu4e-bookmarks
         `((,(dotspacemacs//flat-cat-pose "AND"
              "flag:unread" "NOT flag:trashed" not-spam not-trash not-refile)
            "Unread messages" ?u)
           (,(dotspacemacs//flat-cat-pose "AND" "date:today..now" not-spam)
            "Today's messages" ?t)
           (,(dotspacemacs//flat-cat-pose "AND" "date:7d..now" not-spam)
            "Last 7 days" ?w)
           (,(apply 'dotspacemacs//flat-cat-pose "OR"
              (mapcar 'dotspacemacs//mu4e-add-maildir-prefix
                      (dotspacemacs//mu4e-contexts-var 'mu4e-inbox-folder)))
            "Messages in inboxes", ?i)
           (,(dotspacemacs//flat-cat-pose "AND" "mime:image/*" not-spam)
            "Messages with images" ?p)
           (,(dotspacemacs//flat-cat-pose "AND"
              "flag:unread" "NOT flag:trashed" not-spam)
            "Unread spam" ?s)))

    (setq mu4e-maildir-shortcuts
          '(("/INBOX" . ?i))))

  (evil-define-key 'evilified mu4e-view-mode-map
    "y" 'evil-yank
    "f" 'evil-find-char
    "t" 'evil-find-char-to)
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (zeal-at-point counsel-dash helm-dash password-generator org-category-capture ob-restclient ivy-purpose window-purpose imenu-list impatient-mode grandshell-theme fuzzy evil-lion emoji-cheat-sheet-plus editorconfig counsel swiper ivy company-restclient know-your-http-well company-emoji company-ansible winum symon string-inflection org-brain evil-org sayid flycheck-credo elixir-mode terraform-mode hcl-mode jabber fsm org-page git mustache synonyms csv-mode slime emojify circe oauth2 websocket pcre2el ht markdown-mode macrostep parent-mode projectile request haml-mode pos-tip flycheck flx magit magit-popup git-commit with-editor smartparens iedit anzu evil goto-chg undo-tree org powerline diminish web-completion-data dash-functional company hydra highlight spinner pkg-info epl bind-map bind-key yasnippet packed anaconda-mode pythonic f dash s alert log4e gntp helm avy helm-core async auto-complete popup package-build nginx-mode jinja2-mode ansible-doc ansible web-beautify livid-mode skewer-mode json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern tern coffee-mode clj-refactor inflections edn clojure-snippets multiple-cursors paredit peg cider-eval-sexp-fu cider seq queue clojure-mode elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet elfeed helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag define-word ace-jump-helm-line yapfify yaml-mode ws-butler window-numbering which-key wgrep web-mode volatile-highlights vi-tilde-fringe uuidgen use-package typo toc-org tagedit sql-indent spacemacs-theme spaceline smex smeargle slime-company slim-mode slack scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restclient restart-emacs rcirc-notify rcirc-color rbenv rake rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort pug-mode prodigy popwin pip-requirements persp-mode pbcopy paradox osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-ipython ob-http ob-elixir neotree mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest markdown-toc magit-gitflow lorem-ipsum live-py-mode linum-relative link-hint less-css-mode launchctl ivy-hydra info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flycheck-pos-tip flycheck-mix flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu erlang emmet-mode elisp-slime-nav dumb-jump deft cython-mode counsel-projectile company-web company-statistics company-anaconda common-lisp-snippets column-enforce-mode color-theme-sanityinc-tomorrow clean-aindent-mode chruby bundler auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
