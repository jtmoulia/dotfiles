;; Org
(setq-default org-directory "~/ocloud/org")
(setq-default +org-capture-todo-file "agenda/healthtensor.org")
(setq-default org-agenda-files '("~/ocloud/org/agenda/"))

(defvar my-org//capture-my-todo-file "agenda/mine.org"
  "File to capture my TODO items to.")
(defvar my-org//capture-healthtensor-todo-file "agenda/healthtensor.org"
  "File to capture healthtensor TODO items to.")
(defvar my-org//capture-bookmark-file "bookmarks.org"
  "File to capture bookmarks to.")
(expand-file-name my-org//capture-healthtensor-todo-file org-directory)

(setq-default
 org-capture-templates
 `(("t" "capture todo item")
   ("tm" "capture my todo item" entry
    (file+headline
     ,(expand-file-name my-org//capture-my-todo-file org-directory)
     "Inbox")
    "* TODO %?\n%i\n%a" :prepend t)
   ("th" "capture healthtensor todo item" entry
    (file+headline
     ,(expand-file-name my-org//capture-healthtensor-todo-file org-directory)
     "Inbox")
    "* TODO %?\n%i\n%a" :prepend t)
   ("b" "capture bookmark" entry
    (file+headline my-org//capture-bookmark-file "Inbox")
    "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)))

;; By default enable auto fill mode in org mode buffers
(add-hook 'org-mode-hook 'auto-fill-mode)

;; By default enable flyspell mode in org mode buffers
(add-hook 'org-mode-hook 'flyspell-mode)
