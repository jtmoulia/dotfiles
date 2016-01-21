(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-configuration-layers '(org
                                                    git
                                                    python
                                                    javascript
                                                    common-lisp
                                                    html
                                                    erlang
                                                    elixir
                                                    haskell
                                                    markdown
                                                    rcirc
                                                    auto-completion
                                                    syntax-checking
                                                    racket
                                                    mu4e
                                                    ; themes-megapack
                                                    ;; private layers
                                                    org-page
                                                    artist
                                                    pdf-tools
                                                    lfe
                                                    )))

;; Web mode config
(setq web-mode-markup-indent-offset 2)

;; javascript config
(setq js2-basic-offset 2)

;; erlang config
;; Use man pages installed by homebrew when on OS X
(if (string-equal system-type "darwin")
    (setq erlang-root-dir "/usr/local/opt/erlang/lib/erlang"))

(setq-default dotspacemacs-themes '(sanityinc-tomorrow-eighties
                                    grandshell
                                    leuven))


(setq rcirc-server-alist
      '(("irc.freenode.net" :port 6697 :encryption tls
	 :channels ("#switchboard-dev" "#elixir-lang" "#emacs"))))


(setq-default dotspacemacs-default-font '("Source Code Pro"
                                          :size 12
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.1))

(setq paradox-github-token "7502ee1e220307b9cb9e369a2d6f209038dad6bf")

(defun dotspacemacs/user-config ()
  "Post spacemacs init config."
  ;; Org
  (defvar personal//org-todo-file "~/Dropbox/org/todo.org"
    "The path to the primary todo file.")

  (defvar personal//org-notes-file "~/Dropbox/org/notes.org"
    "The path to the notes file.")

  (defvar personal//org-contacts-file "~/Dropbox/org/contacts.org"
    "The path to the contacts file.")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  ;; Racket
  (setq racket-racket-program "/Applications/Racket v6.2.1/bin/racket")

  ;; LFE
  (setq inferior-lfe-program-options '("-pa" "ebin"))
  )

(defun dotspacemacs/init ()
  "Init callback."
  (setq-default dotspacemacs-editing-style 'vim
                dotspacemacs-leader-key "SPC"
                dotspacemacs-emacs-leader-key "M-m"
                dotspacemacs-major-mode-leader-key ","
                dotspacemacs-command-key ":")

  (setq spacemacs-erlang-elixir-use-edts t))

(with-eval-after-load 'mu4e
  (setq-default
   mu4e-mu-binary         "/usr/local/bin/mu"
   mu4e-maildir           "~/maildirs"            ;; top-level Maildir
   mu4e-confirm-quit      nil
   mu4e-get-mail-command  "offlineimap"
   mu4e-headers-skip-duplicates t
   mu4e-update-interval   600
   mu4e-compose-signature "Thomas Moulia  \njtmoulia.pocketknife.io  \nSkype: jtmoulia  \n@jtmoulia  "
   mu4e-compose-dont-reply-to-self t
   mu4e-compose-complete-only-personal t
   mu4e-hide-index-messages t
   mu4e-sent-folder "/pocketknife/INBOX.Sent Items"
   mu4e-html2text-command "html2text --body-width=78"
   mu4e-user-mail-address-list '("jtmoulia@pocketknife.io"
                                 "jtmoulia@gmail.com"
                                 "thomas@spatch.co")

   ;; User info
   user-full-name "Thomas Moulia"
   user-mail-address "jtmoulia@pocketknife.io"

   message-auto-save-directory (concat (file-name-as-directory mu4e-maildir)
                                       "drafts")
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-stream-type 'ssl
   smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")

   ;; smtpmail-queue-mail t
   smtpmail-queue-dir  "~/maildirs/queue/cur"

   tmail//mu4e-accounts-config
   '(("pocketknife"
      (user-mail-address  "jtmoulia@pocketknife.io")
      (mu4e-inbox-folder  "/pocketknife/INBOX")
      (mu4e-sent-folder   "/pocketknife/INBOX.Sent Items")
      (mu4e-drafts-folder "/pocketknife/INBOX.Drafts")
      (mu4e-trash-folder  "/pocketknife/INBOX.Trash")
      (mu4e-refile-folder "/pocketknife/INBOX.Archive")
      (smtpmail-smtp-user "jtmoulia@pocketknife.io")
      (smtpmail-default-smtp-server "mail.messagingengine.com")
      (smtpmail-smtp-server "mail.messagingengine.com")
      (smtpmail-stream-type ssl)
      (smtpmail-smtp-service 465))
     ("gmail"
      (user-mail-address  "jtmoulia@gmail.com")
      (mu4e-inbox-folder  "/gmail/INBOX")
      (mu4e-sent-folder   "/gmail/[Gmail].Sent Mail")
      (mu4e-drafts-folder "/gmail/[Gmail].Drafts")
      (mu4e-trash-folder  "/gmail/[Gmail].Trash")
      (mu4e-refile-folder "/gmail/[Gmail].Archive")
      (smtpmail-smtp-user "jtmoulia@gmail.com")
      (smtpmail-default-smtp-server "smtp.gmail.com")
      (smtpmail-smtp-server "smtp.gmail.com")
      (smtpmail-smtp-service 465))
     ("spatch"
      (user-mail-address  "thomas@spatch.co")
      (mu4e-inbox-folder  "/spatch/INBOX")
      (mu4e-sent-folder   "/spatch/[Gmail].Sent Mail")
      (mu4e-drafts-folder "/spatch/[Gmail].Drafts")
      (mu4e-trash-folder  "/spatch/[Gmail].Trash")
      (mu4e-refile-folder "/spatch/[Gmail].Archive")
      (smtpmail-smtp-user "thomas@spatch.co")
      (smtpmail-default-smtp-server "smtp.gmail.com")
      (smtpmail-smtp-server "smtp.gmail.com")
      (smtpmail-smtp-service 465))
     ("docdir_admin"
      (user-mail-address  "admin@thedocdir.com")
      (mu4e-inbox-folder  "/docdir_admin/Inbox")
      (mu4e-sent-folder   "/docdir_admin/Sent")
      (mu4e-drafts-folder "/docdir_admin/Drafts")
      (mu4e-trash-folder  "/docdir_admin/Trash")
      (mu4e-refile-folder "/docdir_admin/Archive")
      (smtpmail-smtp-user "admin@thedocdir.com")
      (smtpmail-default-smtp-server "box.thedocdir.com")
      (smtpmail-smtp-server "box.thedocdir.com")
      (smtpmail-stream-type starttls)
      (smtpmail-smtp-service 587))
     ("docdir"
      (user-mail-address  "jtmoulia@thedocdir.com")
      (mu4e-inbox-folder  "/docdir/Inbox")
      (mu4e-sent-folder   "/docdir/Sent")
      (mu4e-drafts-folder "/docdir/Drafts")
      (mu4e-trash-folder  "/docdir/Trash")
      (mu4e-refile-folder "/docdir/Archive")
      (smtpmail-smtp-user "jtmoulia@thedocdir.com")
      (smtpmail-default-smtp-server "box.thedocdir.com")
      (smtpmail-smtp-server "box.thedocdir.com")
      (smtpmail-stream-type starttls)
      (smtpmail-smtp-service 587))
     ))

      ;; drafts are saved as *message*-___
      (add-to-list 'auto-mode-alist '("\\*message\\*-+" . message-mode))

      (defun tmail//mu4e-msg-root (msg)
        "Return the root directory of the MSG's maildir."
        (let ((maildir (mu4e-message-field msg :maildir)))
          (string-match "/\\(.*?\\)/" maildir)
          (match-string 1 maildir)))

      (defun tmail//mu4e-account-property (account property &optional accounts)
        "For ACCOUNT, return a PROPERTY using ACCOUNTS."
        (let ((accounts (if accounts accounts tmail//mu4e-accounts-config)))
          (cadr (assoc property (assoc account accounts)))))

      (defun tmail//mu4e-properties (property &optional accounts)
        "Return PROPERTY for ACCOUNTS."
        (let ((accounts (if accounts accounts tmail//mu4e-accounts-config)))
          (mapcar (lambda (account) (cadr (assoc property (cdr account)))) accounts)))


      ;; list of default spam folders
      (setq tmail//spams
            '("maildir:/gmail/[Gmail].spam" "maildir:/spatch/[Gmail].spam"))

      (defun tmail//mu4e-join-spam (query &optional spams separator)
        "Modify the mu4e QUERY to include SPAMS folders using SEPARATOR."
        (let ((spams (if spams spams tmail//spams))
              (separator (if separator separator " AND NOT ")))
          (concat query
                  (apply 'concat (mapcar (lambda (spam)
                                           (concat separator spam))
                                         spams)))))

      (defun tmail//interpose-concat (sep list)
        "Interpose SEP into LIST and concatenate."
        (apply 'concat (-interpose sep list)))

      (defun tmail//add-maildir-prefix (maildir)
        "Add maildir: prefix to MAILDIR for mu queries."
        (concat "maildir:" maildir))

      ;; mu4e bookmarks -- this is the magic
      (setq mu4e-bookmarks
            `((,(tmail//mu4e-join-spam "flag:unread AND NOT flag:trashed")
               "Unread messages" ?u)
              (,(tmail//mu4e-join-spam "date:today..now")
               "Today's messages" ?t)
              (,(tmail//mu4e-join-spam "date:7d..now")
               "Last 7 days" ?w)
              (,(tmail//interpose-concat
                 " OR "
                 (mapcar 'tmail//add-maildir-prefix
                         (tmail//mu4e-properties 'mu4e-inbox-folder)))
               "Messages in inboxes", ?i)
              (,(tmail//mu4e-join-spam "mime:image/*")
               "Messages with images" ?p)
              (,(tmail//mu4e-join-spam
                 "flag:unread AND NOT flag:trashed"
                 tmail//spams
                 " AND ")
               "Unread spam" ?s)
              ))

      (defun tmail//mu4e-set-account ()
        "Set the account for composing a message."
        (let* ((account
                (if (and (not current-prefix-arg) mu4e-compose-parent-message)
                    (tmail//mu4e-msg-root mu4e-compose-parent-message)
                  (completing-read (format "Compose with account: (%s) "
                                           (mapconcat #'(lambda (var) (car var))
                                                      tmail//mu4e-accounts-config"/"))
                                   (mapcar #'(lambda (var) (car var))
                                           tmail//mu4e-accounts-config)
                                   nil t nil nil (caar tmail//mu4e-accounts-config))))
               (account-vars (cdr (assoc account tmail//mu4e-accounts-config))))
          (if account-vars
              (mapc #'(lambda (var)
                        (set (car var) (cadr var)))
                    account-vars)
            (error "No email account found"))))

      (setq mu4e-refile-folder
            (lambda (msg)
              (tmail//mu4e-account-property (tmail//mu4e-msg-root msg)
                                            'mu4e-refile-folder))
            mu4e-trash-folder
            (lambda (msg)
              (tmail//mu4e-account-property (tmail//mu4e-msg-root msg)
                                            'mu4e-trash-folder)))

      (add-hook 'mu4e-compose-pre-hook 'tmail//mu4e-set-account))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(paradox-automatically-star nil)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values (quote ((org-confirm-babel-evalute)))))
