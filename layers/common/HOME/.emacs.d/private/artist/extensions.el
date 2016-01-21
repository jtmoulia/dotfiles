;;; extensions.el --- artist Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq artist-post-extensions '(artist))

(defun artist/init-artist ()
  "artist configuration."
  (use-package artist
    :defer t
    :commands artist-mode
    :init
    (progn
      (defvar artist--keymap ))
    :config
    (progn
      ;; (evil-leader/set-key-for-mode 'artist-mode "mfdhhhhhh")
      (local-set-key "h" 'artist-backward-char
                     "j" 'artist-next-line
                     "k" 'artist-previous-line
                     "l" 'artist-forward-char)
      ;; (evilify artist-mode artist-mode-map
      ;;          "h" 'artist-backward-char
      ;;          "j" 'artist-next-line
      ;;          "k" 'artist-previous-line
      ;;          "l" 'artist-forward-char
      ;;          "")
      )))
