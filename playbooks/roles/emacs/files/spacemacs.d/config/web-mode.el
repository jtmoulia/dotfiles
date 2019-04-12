;; TODO: this should be moved to JS specific configuration
(defvar personal--web-indent
  2
  "Indentation across js, css, html")

;; js/web-mode
(setq-default
 ;; general js + json
 js-indent-level personal--web-indent
 ;; js2-mode
 js2-basic-offset personal--web-indent
 ;; web-mode
 css-indent-offset personal--web-indent
 )

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

(setq web-mode-markup-indent-offset personal--web-indent
      web-mode-css-indent-offset personal--web-indent
      web-mode-code-indent-offset personal--web-indent
      web-mode-attr-indent-offset personal--web-indent)

