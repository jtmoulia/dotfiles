;; init.lisp -- stumpwm init file
;; author: Thomas Moulia

;; Load swank.
;; NB: This requires multithreaded SBCL vs CLisp
;; *prefix-key* ; swank will kick this off
;; (load "/home/jtmoulia/.emacs.d/elpa/slime-20161102.711/swank-loader.lisp")
;; (defcommand swank () ()
;;     (swank:create-server :port 4005
;;                        :style swank:*communication-style*
;;                        :dont-close t)
;;   (swank-loader:init)
;;   (echo-string (current-screen)
;; 	       "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))
;; (swank)

(load-module "battery-portable")
(load-module "wifi")
(load-module "notifications")

;; Variables
(set-focus-color "red")
; (set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
(set-font "-*-terminus-*-*-*-*-24-*-*-*-*-*-iso10646-*")
(setf *screen-mode-line-format*
      (list "[^B%n^b] %u " ; groups/windows
            "^> " ; right align
            "%N %I %B ^7* %d")
      *mode-line-position* :bottom
      *mode-line-timeout* 5)


;; Keybindings
(define-key *root-map* (kbd "N") 'notifications:*notifications-map*)
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
(define-key *root-map* (kbd "c")
  "run-shell-command maim --select ~/scratch/grab-$(date +%FT%T).png")


(defvar *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a")
      "run-shell-command dmenu_run -fn 'Terminus-16'")
    (define-key m (kbd "e")
      "run-shell-command emacs")
    (define-key m (kbd "x")
      "run-shell-command xterm")
    (define-key m (kbd "c")
      "run-shell-command chromium --force-device-scale-factor=1.8")
    (define-key m (kbd "s")
      "run-shell-command spotify --force-device-scale-factor=1.8")
    (define-key m (kbd "v")
      "run-shell-command pavucontrol")
    (define-key m (kbd "k")
      "run-shell-command enpass")
    m))

(define-key *root-map* (kbd "a") '*app-map*)


(define-key *top-map* (kbd "XF86MonBrightnessDown") "run-shell-command light -U 10")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "run-shell-command light -A 10")

(define-key *top-map* (kbd "XF86AudioMute") "run-shell-command pactl set-sink-mute 1 toggle")
(define-key *top-map* (kbd "XF86AudioLowerVolume")
  "run-shell-command pactl set-sink-mute 1 false; pactl set-sink-volume 1 -5%")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")
  "run-shell-command pactl set-sink-mute 1 false; pactl set-sink-volume 1 +5%")
