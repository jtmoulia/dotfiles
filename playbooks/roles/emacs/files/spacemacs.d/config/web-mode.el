;; web-mode

(defvar personal--web-indent
  2
  "Indentation across js, css, html")

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

(setq web-mode-markup-indent-offset personal--web-indent
      web-mode-css-indent-offset personal--web-indent
      web-mode-code-indent-offset personal--web-indent
      web-mode-attr-indent-offset personal--web-indent)

