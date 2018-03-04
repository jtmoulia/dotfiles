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
(set-font "-*-terminus-*-*-*-*-28-*-*-*-*-*-iso10646-*")
(setf *screen-mode-line-format*
      (list "[^B%n^b] %u " ; groups/windows
            "^> " ; right align
            "%N "
            "[Network: %I] "
            "[Power: %B] ^7* %d")
      *mode-line-position* :bottom
      *mode-line-timeout* 5)


;; Keybindings
(define-key *root-map* (kbd "N") 'notifications:*notifications-map*)
(define-key *root-map* (kbd "M") "mode-line")
(define-key *root-map* (kbd "c")
  "run-shell-command maim --select ~/scratch/grab-$(date +%FT%T).png")


(defvar *app-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a")
      "run-shell-command dmenu_run -i -fn 'Terminus-18'")
    (define-key m (kbd "e")
      "run-shell-command emacsclient -c")
    (define-key m (kbd "E")
      "run-shell-command emacs")
    (define-key m (kbd "x")
      "run-shell-command termite --exec=tmux")
    (define-key m (kbd "X")
      "run-shell-command termite")
    (define-key m (kbd "c")
      "run-shell-command chromium --force-device-scale-factor=2.0")
    (define-key m (kbd "f")
      "run-shell-command firefox-developer")
    (define-key m (kbd "s")
      "run-shell-command spotify --force-device-scale-factor=2.0")
    (define-key m (kbd "S")
      "run-shell-command slack --force-device-scale-factor=2.0")
    (define-key m (kbd "v")
      "run-shell-command pavucontrol")
    (define-key m (kbd "k")
      "run-shell-command enpass")
    (define-key m (kbd "p")
      "run-shell-command passmenu -i -fn 'Terminus-18'")
    m))

(define-key *root-map* (kbd "a") '*app-map*)

(defvar *window-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "b") "windowlist")
    ; (define-key m )
    m))

(define-key *root-map* (kbd "b") '*window-map*)

(defvar *frame-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "w") "other-window")
    (define-key m (kbd "h") "move-focus left")
    (define-key m (kbd "l") "move-focus right")
    (define-key m (kbd "j") "move-focus down")
    (define-key m (kbd "k") "move-focus up")
    (define-key m (kbd "v") "hsplit")
    (define-key m (kbd "s") "vsplit")
    (define-key m (kbd "d") "remove-split")
    (define-key m (kbd "m") "only")
    (define-key m (kbd "K") "delete-window")
    m))

(define-key *root-map* (kbd "w") '*frame-map*)

(define-key *top-map* (kbd "XF86MonBrightnessDown") "run-shell-command light -U 10")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "run-shell-command light -A 10")

(define-key *top-map* (kbd "XF86AudioMute") "run-shell-command pactl set-sink-mute 0 toggle")
(define-key *top-map* (kbd "XF86AudioLowerVolume")
  "run-shell-command pactl set-sink-mute 0 false; pactl set-sink-volume 0 -5%")
(define-key *top-map* (kbd "XF86AudioRaiseVolume")
  "run-shell-command pactl set-sink-mute 0 false; pactl set-sink-volume 0 +5%")


;; Renumber windows by class (https://github.com/stumpwm/stumpwm/wiki/TipsAndTricks)
(defparameter *window-class-renumber*
  '(
    ("Emacs" . 0)
    ("Termite" . 1)
    ("Firefox Developer Edition" . 2)
    ("Enpass-Desktop" . 3)
    ("Spotify" . 4)
    )
  "Alist of window classes to be renumbered, and their target numbers.")

(defun renumber-window-by-class (win)
  "Renumber window if its class matches *window-class-renumber*."

  (let* ((class (window-class win))
         (target-number (cdr (assoc class *window-class-renumber*
                                    :test #'string=))))

    (when target-number
      (let ((other-win (find-if #'(lambda (win)
                                    (= (window-number win) target-number))
                                (group-windows (window-group win)))))
        (if other-win
            (when (string-not-equal class (window-class other-win))
              ;; other window, different class; switch numbers
              (setf (window-number other-win) (window-number win))
              (setf (window-number win) target-number))
            ;; if there's already a window of this class, do nothing.
            ;; just keep the new number for this window.

            ;; else: no other window; target number is free.
            (setf (window-number win) target-number))
        ))))

(add-hook *new-window-hook* 'renumber-window-by-class)
