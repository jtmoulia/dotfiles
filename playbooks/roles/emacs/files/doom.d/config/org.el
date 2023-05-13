;;; config/elfeed.el -*- lexical-binding: t; -*-

(require 'f)

;; Org
(setq-default org-directory (f-join my-sync-dir "org"))
(setq-default +org-capture-todo-file "agenda/ht.org")
(setq-default org-agenda-files (list (f-join org-directory "agenda")))

(defvar my//org-capture-my-todo-file "agenda/mine.org"
  "File to capture my TODO items to.")

(defvar my//org-capture-regard-todo-file "agenda/ht.org"
  "File to capture regard TODO items to.")

(defvar my//org-capture-bookmark-file (f-join org-directory "bookmarks.org")
  "File to capture bookmarks to.")

(defvar my//org-log-file "~/src/hoglog/content-org/journal.org"
  "File to capture journal to.")

;; Don't warn about deadlines if an item is scheduled
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

(defun my-org-copy-link ()
  "Insert the org link under the cursor into the kill ring."
  (interactive)
  (let ((object (org-element-context)))
    (when (eq (car object) 'link)
      (kill-new (org-element-property :raw-link object)))))

(defun my-org-eww-link ()
  "Open the org link under the cursor in eww."
  (interactive)
  (let ((object (org-element-context)))
    (when (eq (car object) 'link)
      (eww (org-element-property :raw-link object)))))

(setq-default
 org-capture-templates
 `(("t" "capture todo item")
   ("r" "regard capture")
   ("b" "bookmarks")
   ("l" "log")
   ("tm" "capture my todo item" entry
    (file+headline
     ,(expand-file-name my//org-capture-my-todo-file org-directory)
     "Inbox")
    "* TODO %?\n%i\n%a" :prepend t)
   ("rt" "capture regard todo item" entry
    (file+headline
     ,(expand-file-name my//org-capture-regard-todo-file org-directory)
     "Inbox")
    "* TODO %?\n%i\n%a" :prepend t)
   ("rj" "capture regard jira todo item" entry
    (file+headline
     ,(expand-file-name my//org-capture-regard-todo-file org-directory)
     "Inbox")
    "* TODO JIRA Issue %^{Issue}: %?\n
  - [[https://healthtensor.atlassian.net/browse/%\\1][JIRA Link]]
  - [[orgit:~/repos/automed/][Automed Repo]]\n%i\n\n")
   ("bb" "capture bookmark" entry
    (file+headline my//org-capture-bookmark-file "Inbox")
    "* %?\n:PROPERTIES:\n:CREATED: %U\n:URL: %a\n:END:\n\n" :prepend t)
   ("ll" "capture log" entry
    (file+headline my//org-log-file "Log")
    "* %(format-time-string \"%B %-dth, '%y\"): %?
SCHEDULED: %T
:PROPERTIES:\n:EXPORT_FILE_NAME: %(format-time-string \"%Y-%m-%d\")\n:END:\n\n"
    :prepend t)))

(after! 'elfeed
  (add-to-list
   'org-capture-templates
   `("bf" "capture [el]feed" entry
     (file+headline ,(car rmh-elfeed-org-files) "root")
     "* %?\n\n")))

;; Company mode auto-completion is painfully slow -- disabled.
(defun my/disable-company-mode ()
  (company-mode -1))
(add-hook 'org-mode-hook #'my/disable-company-mode)

;; By default enable auto fill mode in org mode buffers
(add-hook 'org-mode-hook 'auto-fill-mode)

;; By default enable flyspell mode in org mode buffers
(add-hook 'org-mode-hook 'flyspell-mode)

;; Configuration for the org super agenda
;; https://github.com/alphapapa/org-super-agenda
(after! org-super-agenda
  ;; Disable the org super agenda header map because we use `org-evil-agenda'
  (setq org-super-agenda-header-map nil)
  ;; If defined, start origami mode for agenda folding
  (add-hook 'org-agenda-mode-hook
            (lambda () (when (fboundp 'origami-mode) (origami-mode t)))))

;; org-pomodoro config
;; without tweaking, the default sound is... penetrating. off till then.
(setq org-pomodoro-play-sounds nil)

;; unmap the existing agenda view dispatch for fold bindings
(map! :map evil-org-agenda-mode-map :after (org-super-agenda origami)
      :m "z" nil)

;; map origami folds
(map! :map evil-org-agenda-mode-map :after (org-super-agenda origami)
      :prefix "z"
      :nmi :desc "Org agenda view dispatch" "v" #'org-agenda-view-mode-dispatch
      :nmi :desc "Close agenda section fold" "c" #'origami-close-node
      :nmi :desc "Open agenda section fold" "o" #'origami-open-node
      :nmi :desc "Toggle agenda section fold" "a" #'origami-toggle-node
      :nmi :desc "Toggle all agenda folds" "T" #'origami-toggle-all-nodes)

;; unmap the existing agenda view dispatch for fold bindings
(map! :map evil-org-mode-map :localleader
      :m :desc "Kill the org link to the ring" "l y" #'my-org-copy-link
      :m :desc "Kill the org link to the ring" "l e" #'my-org-eww-link)

;; archive an agenda item using =, A=
(map! :map evil-org-agenda-mode-map :after (org-super-agenda)
      :localleader
      :nmi :desc "Org agenda archive" "A" #'org-agenda-archive)

;; enable org-super-agenda global minor mode if defined
(when (fboundp 'org-super-agenda-mode)
  (org-super-agenda-mode t))

(setq org-agenda-custom-commands
     '(("A" "Absolutely Awesome Agenda"
        ((agenda "" ((org-agenda-span 'week)
                     (org-super-agenda-groups
                      '((:name "Regard In Progress"
                         :and (:tag "regard" :todo ("TODO" "STRT"))
                         :order 1)

                        (:name "Regard Waiting"
                         :and (:tag "regard" :todo ("WAIT"))
                         :order 2)

                        (:name "Regard Completed"
                         :and (:tag "regard" :todo ("DONE" "KILL"))
                         :order 3)

                        (:discard (:anything t))))))
         (alltodo "" ((org-agenda-overriding-header "All Tasks")
                      (org-super-agenda-groups
                       '((:name "Important"
                                :tag "Important"
                                :priority "A"
                                :order 6)
                         (:name "Due Today"
                                :deadline today
                                :order 2)
                         (:name "Due Soon"
                                :deadline future
                                :order 3)
                         (:name "Overdue"
                                :deadline past
                                :order 1)
                         (:name "Done"
                                :and (:tag "regard" :todo ("DONE" "KILL"))
                                :order 9)
                         (:name "Regard Etc"
                                :tag "regard"
                                :order 10)
                         (:discard (:anything t))))))))
                          

       ("M" "my agenda"
          ((agenda "" ((org-agenda-span 'week)
                       (org-super-agenda-groups
                        '((:discard (:tag "regard"))
                          (:name "Time Grid"
                           :time-grid t  ; Items that appear on the time grid
                           :order 0)  ; Items that have this TODO keyword
                          (:name "Mine In Progress"
                           :and (:tag "mine" :not (:todo ("DONE" "WAIT")))
                           :order 1)  ; Items that have this TODO keyword
                          (:name "Mine Completed"
                           :and (:tag "mine" :todo ("DONE" "WAIT"))
                           :order 2)))))))))
                         

(setq org-hugo-date-format "%Y-%m-%dT%T")

;;; org.el ends here
