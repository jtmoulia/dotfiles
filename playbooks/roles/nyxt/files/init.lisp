;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;            My Nyxt Config           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Setup slynk for interactive coding with sly from Emacs.
;; Requires slynk to be installed in the path, to link in doom's slynk run:
;; ln -s ~/.emacs.d/.local/straight/build-29.0.50/sly/slynk ~/.local/share/nyxt/extensions/slynk
;;
;; Exposes `start-slynk' for kicking off a sly repl server
(load-after-system :slynk "~/.config/nyxt/my-slynk.lisp")
(ignore-errors (asdf:load-system :slynk))


;; Requires demeter to be installed in the path. Load errors are ignored.
;;
;; Startup by calling `demeter'
(ignore-errors (asdf:load-system :demeter))
