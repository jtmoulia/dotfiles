;; Dirty Hack: https://github.com/syl20bnr/spacemacs/issues/4413
(defvar spacemacs-mode-line-new-version-lighterp t)

(defvar dotspacemacs-mu-root (expand-file-name "~/repos/mu")
  "mu's source path")

(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-configuration-layers
                '(org
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
                  (mu4e :variables
                        mu4e-installation-path
                        (concat dotspacemacs-mu-root "/mu4e"))
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
                 (mu4e-inbox-folder . "/pocketknife/INBOX")
                 (mu4e-sent-folder . "/pocketknife/INBOX.Sent Items")
                 (mu4e-drafts-folder . "/pocketknife/INBOX.Drafts")
                 (mu4e-trash-folder . "/pocketknife/INBOX.Trash")
                 (mu4e-refile-folder . "/pocketknife/INBOX.Archive")
                 (smtpmail-smtp-user . "jtmoulia@pocketknife.io")
                 (smtpmail-default-smtp-server . "mail.messagingengine.com")
                 (smtpmail-smtp-server . "mail.messagingengine.com")
                 (smtpmail-stream-type . ssl)
                 (smtpmail-smtp-service . 465)))))

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
    (concat "maildir:" maildir))

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
  (setq-default
   mu4e-mu-binary         (concat dotspacemacs-mu-root "/mu/mu")
   mu4e-maildir           "~/maildirs"            ;; top-level Maildir
   mu4e-confirm-quit      nil
   mu4e-get-mail-command  "offlineimap"
   mu4e-headers-skip-duplicates t
   mu4e-update-interval   600
   mu4e-compose-signature (apply 'concat (-interpose
                                          "  \n"
                                          '("Thomas Moulia"
                                            "jtmoulia.pocketknife.io"
                                            "Skype: jtmoulia"
                                            "@jtmoulia")))
   mu4e-compose-dont-reply-to-self t
   mu4e-compose-complete-only-personal t
   mu4e-hide-index-messages t
   mu4e-html2text-command "html2text --body-width=78"
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
   smtpmail-queue-dir  "~/maildirs/queue/cur")

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
            "Unread spam" ?s))))
  )

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
