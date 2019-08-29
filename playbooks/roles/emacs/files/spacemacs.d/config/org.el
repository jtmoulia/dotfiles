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

;; rebind the evil visual mode motion commands
(setq evil-org-movement-bindings
      '((up . "K") (down . "J") (left . "h") (right . "l")))

(add-to-list 'org-agenda-files personal//org-todo-file)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (sql . t)
    (ipython . t)
    (emacs-lisp . t)
    (shell . t)
    ))
