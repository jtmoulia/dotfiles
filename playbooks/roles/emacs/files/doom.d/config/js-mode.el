;; js-mode

(defvar personal--js-indent
  2
  "Indentation across JS and CSS for js-mode.")

(setq-default
 ;; general js + json
 js-indent-level personal--js-indent
 ;; js2-mode
 js2-basic-offset personal--js-indent
 ;; web-mode
 css-indent-offset personal--js-indent
 )
