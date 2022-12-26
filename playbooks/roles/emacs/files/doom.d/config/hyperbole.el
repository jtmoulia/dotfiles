;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/hyperbole.el -*- lexical-binding: t; -*-

(require 'thingatpt)

;; Match against strings like RD-1234
(defconst my//regard-jira-regex "RD\\-[0-9]+")

(defib regard-jira ()
    "Make a JIRA Regard identifier open in the browser."
    (let ((jira-id (when (thing-at-point-looking-at my//regard-jira-regex)
                     (buffer-substring (match-beginning 0) (match-end 0)))))
      (when jira-id
        (hact 'browse-url (concat "https://jira.com/" jira-id)))))

;;; hyperbole.el ends here
