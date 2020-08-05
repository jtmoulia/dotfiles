(defvar my-mu4e--personal-gmail-all-mail
  "/gmail/[Gmail].All Mail"
  "The endless email directory for personal gmail.")

(defvar my-mu4e--healthtensor-gmail-all-mail
  "/healthtensor/[Gmail].All Mail"
  "The endless email directory for HealthTensor's gmail.")

(defvar my-mu4e--mailing-lists-alist
  `(((,my-mu4e--personal-gmail-all-mail . "/gmail/[Gmail].Trash")
     . ("mu-discuss@googlegroups.com"
        "jtmoulia@alum.mit.edu"))
    ((,my-mu4e--healthtensor-gmail-all-mail . "/healthtensor/[Gmail].Trash")
     . ("scalar@healthtensor.com"))
    )
  "List of mailing list addresses and folders where their messages are saved")

(setq my-mu4e--mailing-lists-alist
  `(((,my-mu4e--personal-gmail-all-mail . "/gmail/[Gmail].Trash")
     . ("mu-discuss@googlegroups.com"
        "jtmoulia@alum.mit.edu"))
    ;; ((,my-mu4e--healthtensor-gmail-all-mail . "/healthtensor/[Gmail].Trash")
    ;;  . ("mu-discuss@googlegroups.com"))
    ))

(defvar my-mu4e--headers-hide-all-mail
  nil
  "Whether to show `[Gmail].All Mail' in mu4e headers view")

(defun* my-mu4e//get-refile-for-mailing-list
    (msg &optional (mailing-list-alist my-mu4e--mailing-lists-alist))
  "Return the account associated with the provided mailing-list"
  (if mailing-list-alist
      (let ((next-mailing-list (car mailing-list-alist)))
        (if (seq-filter (lambda (mailing-list)
                          (mu4e-message-contact-field-matches msg :to mailing-list))
                        (cdr next-mailing-list))
            (car next-mailing-list)
          (my-mu4e//get-refile-for-mailing-list msg (cdr mailing-list-alist))))))

(defun my-mu4e//refile-folder-function (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (subject (mu4e-message-field msg :subject))
         (mailing-list (my-mu4e//get-refile-for-mailing-list msg)))
    (cond
     (mailing-list (car mailing-list))
     ((string-match "^/gmail" maildir)
      my-mu4e--personal-gmail-all-mail)
     ((string-match "^/healthtensor" maildir)
      my-mu4e--healthtensor-gmail-all-mail)
     ;; this is this function . . .
     (t mu4e-refile-folder)
     )))

(defun my-mu4e//trash-folder-function (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (subject (mu4e-message-field msg :subject))
         (mailing-list (my-mu4e//get-refile-for-mailing-list msg)))
    (cond
     (mailing-list (cdr mailing-list))
     ((string-match "^/gmail" maildir) "/gmail/[Gmail].Trash")
     ((string-match "^/healthtensor" maildir) "/healthtensor/[Gmail].Trash")
     ;; this is this function . . .
     (t mu4e-trash-folder)
     )))

;; `mu4e-trash-folder' is defined here because it's not working in `:vars' :/
;; Luckily, it's the same folder across all contexts.
(setq-default mu4e-trash-folder #'my-mu4e//trash-folder-function)

;; Configure Contexts
(setq-default
 mu4e-contexts
 `(
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
             (mu4e-sent-folder . "/healthtensor/[Gmail].Sent Mail")
             (mu4e-drafts-folder . "/healthtensor/[Gmail].Drafts")
             (mu4e-trash-folder . "/healthtensor/[Gmail].Trash")
             ;; (mu4e-trash-folder . my-mu4e//trash-folder-function)
             (mu4e-refile-folder . my-mu4e//refile-folder-function)
             (mu4e-spam-folder . "/healthtensor/[Gmail].Spam")
             (smtpmail-smtp-user . "thomas@healthtensor.com")
             (smtpmail-default-smtp-server . "smtp.gmail.com")
             (smtpmail-smtp-server . "smtp.gmail.com")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587)))
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
             ;; (mu4e-trash-folder . my-mu4e//trash-folder-function)
             (mu4e-refile-folder . my-mu4e//refile-folder-function)
             (mu4e-spam-folder . "/gmail/[Gmail].Spam")
             (smtpmail-smtp-user . "jtmoulia@gmail.com")
             (smtpmail-default-smtp-server . "smtp.gmail.com")
             (smtpmail-smtp-server . "smtp.gmail.com")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587)))
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
             ;; (mu4e-trash-folder . my-mu4e//trash-folder-function)
             (mu4e-refile-folder . my-mu4e//refile-folder-function)
             (mu4e-spam-folder . "/pocketknife/Junk Mail")
             (smtpmail-smtp-user . "jtmoulia@pocketknife.io")
             (smtpmail-default-smtp-server . "mail.messagingengine.com")
             (smtpmail-smtp-server . "mail.messagingengine.com")
             (smtpmail-stream-type . ssl)
             (smtpmail-smtp-service . 465)))
   ))


;; Configure Vars
(require 'mu4e-contrib)

;; (require 'org-mu4e)
(setq-default
 mu4e-mu-binary         "/usr/bin/mu"
 ;; top-level maildir, email fetcher should be configured to save here
 mu4e-root-maildir     "~/.mail"
 mu4e-confirm-quit      nil
 mu4e-get-mail-command  "offlineimap -q"
 mu4e-headers-skip-duplicates t
 mu4e-headers-include-related nil
 mu4e-update-interval   600
 mu4e-index-lazy-check  nil
 mu4e-use-fancy-chars   nil
 mu4e-compose-signature (apply 'concat (-interpose
                                        "  \n"
                                        '("Thomas Moulia"
                                          "Co-Founder & CTO | HealthTensor"
                                          "www.healthtensor.com"
                                          "jtmoulia.pocketknife.io")))
;; TODO: How can this HTML signature be part of the org export
;;  mu4e-compose-signature (apply 'concat (-interpose
;;                                         "\n"
;;                                         '(
;; "<div style=\"font-family:roboto, arial-bold, sans-serif;font-weight:bold;color:#43474c;font-size:100%;line-height:20px;margin:none\">Thomas Moulia | CTO"
;; "</div>"
;; "<a href=\"mailto:Thomas@healthtensor.com\" style=\"font-family:roboto, arial;color:#757d84;font-size:87.5%;text-decoration:none !important\">Thomas@healthtensor.com</a>"
;; "<hr style=\"border-color:#b6babe;margin-top:8px;margin-bottom:4px\" noshade>"
;; "</hr>"
;; "<table>"
;; "  <td>"
;; "  <a href=\"https://www.healthtensor.com/\"><img src=\"https://healthtensor-media.s3-us-west-1.amazonaws.com/HTlogo_vertical_blue.png\" height=\"36px\"></a>"
;; " </td>"
;; " <td style=\"padding-left:8px;line-height:2px;font-size:87.5%;font-family:roboto, arial, sans-serif;color:#757d84\">"
;; "    <p>4133 Redwood Avenue</p>"
;; "    <p>Los Angeles, CA 90066</p>"
;; "  </td>"
;; "</table>"
;; )))
 mu4e-compose-dont-reply-to-self t
 mu4e-compose-complete-only-personal t
 mu4e-hide-index-messages t
 mu4e-html2text-command 'mu4e-shr2text
 ;; User info
 message-auto-save-directory (concat (file-name-as-directory mu4e-root-maildir)
                                     "drafts")
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'ssl
 smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
 ;; smtpmail-queue-mail t
 smtpmail-queue-dir  (expand-file-name "~/.mail/queue/cur"))

;; drafts are saved as *message*-___
(add-to-list 'auto-mode-alist '("\\*message\\*-+" . message-mode))


;; Helper functions for composing bookmarks from contexts
(defun my-mu4e//mu4e-context (context-name)
  "Return the context in `mu4e-contexts' with name CONTEXT-NAME.

Raises an error if that context isn't present."
  (let* ((names (mapcar (lambda (context)
                          (cons (mu4e-context-name context) context))
                        mu4e-contexts))
         (context (cdr (assoc context-name names))))
    (if context
        context
      (error "no context with name: %s" context-name))))

(defun my-mu4e//mu4e-context-get-var (context var)
  "For CONTEXT return VAR. Helper function for access."
  (cdr (assoc var (mu4e-context-vars context))))

(defun my-mu4e//mu4e-context-var (context-name var)
  "Return the value of VAR for the context with name CONTEXT-NAME, searching
`mu4e-contexts'."
  (my-mu4e//mu4e-context-get-var
   (my-mu4e//mu4e-context context-name)
   var))

(defun my-mu4e//mu4e-contexts-var (var)
  "Return a list of the value for VAR across `mu4e-contexts'. If VAR is
undefined for a context, it will be filtered out."
  (delq nil
        (mapcar (lambda (context)
                  (my-mu4e//mu4e-context-get-var context var))
                mu4e-contexts)))

(defun my-mu4e//mu4e-add-maildir-prefix (maildir)
  "Add maildir: prefix to MAILDIR for mu queries."
  (concat "maildir:\"" maildir "\""))

(defun my-mu4e//flat-cat (&rest list)
  "Flatten and concatenate LIST."
  (apply 'concat (-flatten list)))

(defun my-mu4e//flat-cat-pose (sep &rest list)
  "Unabashed helper function to interpose SEP padded with
spaces into LIST. Return the padded result."
  (my-mu4e//flat-cat
   (-interpose (concat " " sep " ") list)))

(defun* my-mu4e//wrap-terms (terms &key (prefix "") (sep "AND"))
  (apply 'my-mu4e//flat-cat-pose sep
         (-map (lambda (term) (concat "(" prefix "\"" term "\"" ")")) terms)))

(defun* my-mu4e//mu4e-query
    (var &key (prefix "") (sep "AND"))
  (my-mu4e//wrap-terms (my-mu4e//mu4e-contexts-var var) :prefix prefix :sep sep))

(defun my-mu4e//bm-or (&rest list)
  (apply 'my-mu4e//flat-cat-pose "OR" list))

(defun my-mu4e//bm-and (&rest list)
  (apply 'my-mu4e//flat-cat-pose "AND" list))

(defun my-mu4e//bm-not (item)
  (concat "NOT " item))

(defun my-mu4e//bm-wrap (item)
  (concat "(" item ")"))

(defun my-mu4e//not-spam ()
  (my-mu4e//mu4e-query 'mu4e-spam-folder
                       :prefix "NOT maildir:"))

(defun my-mu4e//not-trash ()
  (my-mu4e//wrap-terms
   '("/gmail/[Gmail].Trash" "/healthtensor/[Gmail].Trash" "/pocketknife/INBOX.Trash")
   :prefix "NOT maildir:"))

(defun my-mu4e//inboxes ()
  (my-mu4e//bm-wrap
   (apply 'my-mu4e//bm-or
          (mapcar 'my-mu4e//mu4e-add-maildir-prefix
                  (my-mu4e//mu4e-contexts-var 'mu4e-inbox-folder)))))

(defun my-mu4e//sent-folders ()
  (my-mu4e//bm-wrap
   (apply 'my-mu4e//bm-or
          (mapcar 'my-mu4e//mu4e-add-maildir-prefix
                  (my-mu4e//mu4e-contexts-var 'mu4e-sent-folder)))))

;; mu4e bookmarks -- this is the magic
(setq mu4e-bookmarks
      `((,(my-mu4e//bm-and
           "flag:unread" "NOT flag:trashed" (my-mu4e//not-spam) (my-mu4e//not-trash))
         "Unread messages" ?u)
        (,(my-mu4e//bm-and
           "date:7d..now" "flag:unread" "NOT flag:trashed" (my-mu4e//not-spam) (my-mu4e//not-trash))
         "Unread messages from the last week" ?U)
        (,(my-mu4e//inboxes)
         "All inboxes", ?i)
        (,(my-mu4e//bm-and "date:7d..now" (my-mu4e//bm-or (my-mu4e//inboxes)))
         "All inbox messages from the last week", ?I)
        (,(my-mu4e//bm-and "date:today..now" (my-mu4e//not-spam))
         "Today's messages" ?t)
        (,(my-mu4e//bm-and "date:7d..now" (my-mu4e//not-spam) (my-mu4e//not-trash))
         "Last 7 days no trash or spam" ?w)
        ("date:7d..now"
         "Last 7 days" ?W)
        (,(my-mu4e//bm-and "mime:image/*" (my-mu4e//not-spam))
         "Messages with images" ?p)
        (,(my-mu4e//sent-folders)
         "Sent mail" ?s)
        (,(my-mu4e//bm-and "date:7d..now" (my-mu4e//sent-folders))
         "Sent mail from the last week" ?S)
        (,(my-mu4e//bm-and "flag:unread" "NOT flag:trashed" (my-mu4e//not-spam))
         "Unread spam" ?z))
      )

(setq mu4e-maildir-shortcuts
      `((,(my-mu4e//mu4e-context-var "gmail" 'mu4e-inbox-folder) . ?g)
        (,(my-mu4e//mu4e-context-var "healthtensor" 'mu4e-inbox-folder) . ?h)))

;; Configure mu4e-alert
(setq mu4e-alert-interesting-mail-query (my-mu4e//bm-and (my-mu4e//inboxes) "flag:unread")
      mu4e-alert-style 'libnotify
      mu4e-alert-email-notification-types '(subjects))
(mu4e-alert-enable-notifications)

;; See single folder config: https://groups.google.com/forum/#!topic/mu-discuss/BpGtwVHMd2E
(add-hook 'mu4e-mark-execute-pre-hook
          (lambda (mark msg)
            (cond
                  ((equal mark 'refile) (mu4e-action-retag-message msg "-\\Inbox"))
                  ((equal mark 'trash) (mu4e-action-retag-message msg "-\\Inbox,-\\Starred"))
                  ((equal mark 'flag) (mu4e-action-retag-message msg "-\\Inbox,\\Starred"))
                  ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;; monkey-patch override of original definition to not override https URLs
;; TODO: check if the mu project would accept this upstream
;; (defun org~mu4e-mime-replace-images (str current-file)
;;   "Replace images in html files with cid links."
;;   (let (html-images)
;;     (cons
;;      (replace-regexp-in-string ;; replace images in html
;;       "src=\"\\(?!https\\)\\([^\"]+\\)\""
;;       (lambda (text)
;;         (format
;;          "src=\"cid:%s\""
;;          (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
;;                           (match-string 1 text)))
;;                 (path (expand-file-name
;;                        url (file-name-directory current-file)))
;;                 (ext (file-name-extension path))
;;                 (id (replace-regexp-in-string "[\/\\\\]" "_" path)))
;;            (add-to-list 'html-images
;;                         (org~mu4e-mime-file
;;                          (concat "image/" ext) path id))
;;            id)))
;;       str)
;;      html-images)))

;; (defun org~mu4e-mime-replace-images (str current-file)
;;   "Replace images in html files with cid links."
;;   nil)
;;
;; GMail has duplicate messages between All Mail and other directories.
;; This function allows the
(defun my-mu4e-headers-toggle-all-mail (&optional dont-refresh)
  "Toggle whether to hide all mail and re-render"
  (interactive)
  (setq my-mu4e--headers-hide-all-mail (not my-mu4e--headers-hide-all-mail))
  (unless dont-refresh
    (mu4e-headers-rerun-search)))

(defun my-mu4e-headers-hide-predicate (msg)
  (if my-mu4e--headers-hide-all-mail
   (string-equal "/healthtensor/[Gmail].All Mail" (mu4e-message-field msg :maildir))))

(setq mu4e-headers-hide-predicate #'my-mu4e-headers-hide-predicate)


;; Keybindings
(map! :map mu4e-headers-mode-map :nv "z D" #'my-mu4e-headers-toggle-all-mail)

;;; mu4e.el ends here
