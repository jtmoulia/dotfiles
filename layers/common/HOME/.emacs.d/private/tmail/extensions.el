;;; extensions.el --- tmail Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar tmail-pre-extensions
  '()
  "List of all extensions to load before the packages.")

(defvar tmail-post-extensions
  '(mu4e)
  "List of all extensions to load after the packages.")

(defun tmail/init-mu4e ()
  "mu4e configuration."
  (add-to-list 'load-path
               "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e/")
  (require 'mu4e)
  (use-package mu4e
    :defer t
    :config
    (progn
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
       mu4e-org-contacts-file (expand-file-name "~/Dropbox/contacts.org")

       ;; User info
       user-full-name "Thomas Moulia"
       user-mail-address "jtmoulia@pocketknife.io"

       message-auto-save-directory (concat (file-name-as-directory mu4e-maildir)
                                           "drafts")
       send-mail-function 'smtpmail-send-it
       message-send-mail-function 'smtpmail-send-it
       smtpmail-stream-type 'ssl
       smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")

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
          (smtpmail-smtp-service 465))))

      (add-to-list 'mu4e-headers-actions
                   '("org-contact-add" . mu4e-action-add-org-contact) t)
      (add-to-list 'mu4e-view-actions
                   '("org-contact-add" . mu4e-action-add-org-contact) t)

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

      (add-hook 'mu4e-compose-pre-hook 'tmail//mu4e-set-account)

      ;; Set up keybindings
      (evilify mu4e-main-mode mu4e-main-mode-map)
      (evilify mu4e-headers-mode mu4e-headers-mode-map
        "mj" 'mu4e~headers-jump-to-maildir)

      )))
