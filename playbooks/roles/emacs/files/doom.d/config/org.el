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

;; Don't warn about deadlines if an item is scheduled
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

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

;; enable org-super-agenda global minor mode if defined
(when (fboundp 'org-super-agenda-mode)
  (org-super-agenda-mode t))


;; (setq org-super-agenda-groups
;;       '(;; Each group has an implicit boolean OR operator between its selectors.
;;         (:name "HT"  ; Optionally specify section name
;;          ;; :time-grid t  ; Items that appear on the time grid
;;          :tag "ht"
;;          :todo "TODAY"
;;          :order 1)  ; Items that have this TODO keyword
;;         (:name "MINE"  ; Optionally specify section name
;;          :tag "mine"
;;          :todo "TODAY"
;;          :order 2)  ; Items that have this TODO keyword
;;         ;; After the last group, the agenda will display items that didn't
;;         ;; match any of these groups, with the default order position of 99

 (setq org-agenda-custom-commands
      '(("A" "Absolutely Awesome Agenda"
         ((agenda "" ((org-agenda-span 'week)
                      (org-super-agenda-groups
                       '((:name "Time Grid"
                          :time-grid t  ; Items that appear on the time grid
                          :order 0)  ; Items that have this TODO keyword
                        (:name "HealthTensor In Progress"
                          :and (:tag "ht" :not (:todo ("DONE")))
                          :order 1)  ; Items that have this TODO keyword

                         (:name "HealthTensor Completed"
                          :and (:tag "ht" :todo ("DONE"))
                          :order 2)
                         (:name "Mine"
                          :tag "mine"
                          :order 3)
                         ))))
                       ;; '((:name "Today"
                       ;;          :time-grid t
                       ;;          :date today
                       ;;          :todo "TODAY"
                       ;;          :scheduled today
                       ;;          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "All Tasks")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "ht"
                                 :tag "HT"
                                 :order 10)
                          (:name "mine"
                                 :tag "mine"
                                 :order 12)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          ; (:discard (:tag ("Chore" "Routine" "Daily")))
                          ))))))))


;;; org.el ends here
