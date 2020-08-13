;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/text.el -*- lexical-binding: t; -*-
;; Helpers for working with text.

(defun +join-paragraphs ()
  "Join paragraphs in selection such that each takes a single line"
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (if (string= "\n" (string (char-after (point))))
          ;; skip the end of paragraph separator
          (forward-char)
        ;; replace the paragraph newline
        (replace-match " " nil t)))))

;;; text.el ends here
