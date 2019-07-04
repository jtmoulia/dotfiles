;; lsp.el
(defun personal//toggle-boolean-variable (var-name)
  (if (symbol-value var-name)
      (set-variable var-name nil)
    (set-variable var-name t)))

(defun personal//create-toggle (var-name)
  "Return a lambda that will toggle boolean VAR-NAME when called."
  (lambda () (personal//toggle-boolean-variable var-name)))
