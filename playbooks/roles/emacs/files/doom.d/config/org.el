;; Org
(setq-default org-directory "~/ocloud/org")
(setq-default +org-capture-todo-file "agenda/ht.org")
(setq-default org-agenda-files '("~/ocloud/org/agenda/"))
;; (setq org-agenda-dim-blocked-tasks 'invisible)

(defvar personal//org-capture-my-todo-file "agenda/mine.org"
  "File to capture my TODO items to.")
(defvar personal//org-capture-healthtensor-todo-file "agenda/ht.org"
  "File to capture healthtensor TODO items to.")
(defvar personal//org-capture-bookmark-file "bookmarks.org"
  "File to capture bookmarks to.")

(setq-default
 org-capture-templates
 `(("t" "capture todo item")
   ("tm" "capture my todo item" entry
    (file+headline
     ,(expand-file-name personal//org-capture-my-todo-file org-directory)
     "Inbox")
    "* TODO %?\n%i\n%a" :prepend t)
   ("th" "capture healthtensor todo item" entry
    (file+headline
     ,(expand-file-name personal//org-capture-healthtensor-todo-file org-directory)
     "Inbox")
    "* TODO %?\n%i\n%a" :prepend t)
   ("b" "capture bookmark" entry
    (file+headline personal//org-capture-bookmark-file "Inbox")
    "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)))

;; (org-add-agenda-custom-command
;;  '("c" "Agenda and Unscheduled"
;;    ((agenda "")
;;     (todo ""
;;           ((org-agenda-overriding-header "\nUnscheduled TODO")
;;            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))))

(defun personal/org-agenda-filter-tags-to-ht ()
  "Interactive function to filter the agenda views flags down to HT."
  (interactive)
  (org-agenda-filter-apply '("+HT") 'tag t))

(defun personal/org-open-ht ()
  (interactive)
  (find-file "~/ocloud/org/agenda/ht.org"))

(defun personal/org-open-mine ()
  (interactive)
  (find-file "~/ocloud/org/agenda/mine.org"))

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      "h" #'personal/org-agenda-filter-tags-to-ht)

(map! :leader "o o h" #'personal/org-open-ht)
(map! :leader "o o m" #'personal/org-open-mine)

;; By default enable auto fill mode in org mode buffers
(add-hook 'org-mode-hook 'auto-fill-mode)

;; By default enable flyspell mode in org mode buffers
(add-hook 'org-mode-hook 'flyspell-mode)
