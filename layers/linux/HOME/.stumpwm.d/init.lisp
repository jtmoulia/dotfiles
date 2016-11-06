;; init.lisp -- stumpwm init file
;; author: Thomas Moulia

;; Load swank.
;; NB: This requires multithreaded SBCL vs CLisp
;; *prefix-key* ; swank will kick this off
(load "/home/jtmoulia/.emacs.d/elpa/slime-20161102.711/swank-loader.lisp")
(swank-loader:init)
(defcommand swank () ()
    (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen)
	       "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
(swank)

(load-module "battery-portable")

;; Variables
(set-focus-color "red")
(set-font "-*-terminus-*-*-*-*-24-*-*-*-*-*-iso10646-*")
(setf *screen-mode-line-format*
      (list "[^B%n^b] %W " ; groups/windows
            "^>" ; right align
            " %B ^7* %d")
      *mode-line-position* :bottom
      *mode-line-timeout* 1)

;; Keybindings
(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")

(define-key *root-map* (kbd "v") "hsplit")
(define-key *root-map* (kbd "s") "vsplit")
(define-key *root-map* (kbd "d") "remove-split")
(define-key *root-map* (kbd "m") "only")
(define-key *root-map* (kbd "w") "other-window")
(define-key *root-map* (kbd "b") "windowlist")
(define-key *root-map* (kbd "K") "delete-window")
(define-key *root-map* (kbd "M") "mode-line")

(define-key *root-map* (kbd "C") "run-shell-command chromium --force-device-scale-factor=1.8")
(define-key *root-map* (kbd "S") "run-shell-command spotify --force-device-scale-factor=1.8")

(define-key *top-map* (kbd "XF86MonBrightnessDown") "run-shell-command light -U 10")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "run-shell-command light -A 10")
