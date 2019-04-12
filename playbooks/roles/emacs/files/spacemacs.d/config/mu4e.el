;; Activate snippets for mu4e-view-mode
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (yas-reload-all 1)))

;; Configure Contexts
(setq-default
 mu4e-contexts
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
 mu4e-update-interval   nil
 mu4e-index-lazy-check  t
 mu4e-use-fancy-chars   nil
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
        `((,(dotspacemacs//mu4e-context-var "gmail" 'mu4e-inbox-folder) . ?g)
          (,(dotspacemacs//mu4e-context-var "healthtensor" 'mu4e-inbox-folder) . ?h))))

(evil-define-key 'evilified mu4e-view-mode-map
  "y" 'evil-yank
  "f" 'evil-find-char
  "t" 'evil-find-char-to)
