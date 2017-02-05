(defvar dotspacemacs-mu-root
  (expand-file-name "~/repos/mu")
  "mu's source path")
(defvar dotspacemacs-slack-el
  (expand-file-name (concat dotspacemacs-directory "/slack.el"))
  "the path to the slack elisp file")

(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-chromium))

(setq dotspacemacs-additional-packages '(ob-ipython))

(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-configuration-layers
                '(
                  ansible
                  auto-completion
                  clojure
                  common-lisp
                  deft
                  elfeed
                  emacs-lisp
                  git
                  html
                  javascript
                  markdown
                  nginx
                  org
                  prodigy
                  python
                  rcirc
                  restclient
                  slack
                  spacemacs-layouts
                  sql
                  syntax-checking
                  typography
                  yaml
                  (mu4e :variables
                        mu4e-installation-path
                        (concat dotspacemacs-mu-root "/mu4e"))
                  ; themes-megapack
                  ;; private layers
                  ; org-page
                  ; artist
                  ; lfe
                  )))

;; Web mode config
(setq web-mode-markup-indent-offset 2)

;; javascript config
(setq js2-basic-offset 2)

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
                                          :size 22
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.1))

(defun dotspacemacs/user-config ()
  "Post spacemacs init config."
  ;; Org
  (with-eval-after-load 'org
   (defvar personal//org-todo-file "~/Dropbox/org/todo.org"
     "The path to the primary todo file.")

   (defvar personal//org-notes-file "~/Dropbox/org/notes.org"
     "The path to the notes file.")

   (defvar personal//org-contacts-file "~/Dropbox/org/contacts.org"
     "The path to the contacts file.")

   (add-to-list 'org-agenda-files personal//org-todo-file)

   (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (sql . t)
      (ipython . t)
      ))
   )

  (setq deft-directory "~/Dropbox/org/notes")

  ;; Slack
  (with-eval-after-load 'slack
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
    (setq python-shell-interpreter "ipython"
          ; For IPython 5.x --
          python-shell-interpreter-args "--simple-prompt -i"
          )

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
           (sql-port 5433))))

  ;; tramp
  (add-to-list 'tramp-default-proxies-alist
               '("ip-10-50-3-34.ec2.internal" nil "/ssh:ec2-user@34.198.67.93"))

  ;; Prodigy
  (setq spacemacs--lib-dir (expand-file-name "~/lib"))
  (prodigy-define-service
    :name "biguns:5432->localhost:5433"
    :command "ssh"
    :args '("-L" "5433:localhost:5432" "biguns" "-N")
    :tags '(:deepmed)) 
  (prodigy-define-service
    :name "allscratch webpack"
    :command "yarn"
    :tags '(allscratch frontend)
    :args '("start")
    :cwd (concat spacemacs--lib-dir "/allscratch"))
  (prodigy-define-service
    :name "allscratch dev backend"
    :tags '(allscratch backend)
    :cwd (concat spacemacs--lib-dir "/allscratch")
    :command "/home/jtmoulia/.virtualenvs/allscratch/bin/python"
    :args (list (concat spacemacs--lib-dir "/allscratch/bin/runflask"))
    :env '(("ALLSCRATCH_ENV" "dev")))

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
  (require 'org-mu4e)
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
          '(("/INBOX" . ?i)))
    )
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (nginx-mode jinja2-mode ansible-doc ansible web-beautify livid-mode skewer-mode json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern tern coffee-mode clj-refactor inflections edn clojure-snippets multiple-cursors paredit peg cider-eval-sexp-fu cider seq queue clojure-mode elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet elfeed helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag define-word ace-jump-helm-line yapfify yaml-mode ws-butler window-numbering which-key wgrep web-mode volatile-highlights vi-tilde-fringe uuidgen use-package typo toc-org tagedit sql-indent spacemacs-theme spaceline smex smeargle slime-company slim-mode slack scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restclient restart-emacs rcirc-notify rcirc-color rbenv rake rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort pug-mode prodigy popwin pip-requirements persp-mode pbcopy paradox osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-ipython ob-http ob-elixir neotree mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest markdown-toc magit-gitflow lorem-ipsum live-py-mode linum-relative link-hint less-css-mode launchctl ivy-hydra info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flycheck-pos-tip flycheck-mix flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu erlang emmet-mode elisp-slime-nav dumb-jump deft cython-mode counsel-projectile company-web company-statistics company-anaconda common-lisp-snippets column-enforce-mode color-theme-sanityinc-tomorrow clean-aindent-mode chruby bundler auto-yasnippet auto-highlight-symbol auto-compile alchemist aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
