(use-modules (guix packages)
             (guix git-download)
             (guix build-system meson)
             (guix licenses)
             (gnu packages base)
             (gnu packages search)
             (gnu packages glib)
             (gnu packages guile)
             (gnu packages mail)
             (gnu packages pkg-config)
             (gnu packages shells)
             (gnu packages commencement)
             (gnu packages emacs))

(let ((commit "18bf2d995d242ed2e44f2379e357d7012d0e0b16")
      (revision "1"))
  (package
   (name "mu")
   (version (git-version "1.8.9" revision commit))
   (source (origin
            ;; (method url-fetch)
            ;; (uri (string-append "https://github.com/djcb/mu/releases/"
            ;;                     "download/" version "/"
            ;;                     "mu-" version ".tar.xz"))
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jtmoulia/mu")
                  (commit commit)))
            (sha256
             (base32
              "1jgl9nyhs5lwczaz30nr6zndpgdvipjpf6za75razv98l8kiiaax"))))
   (build-system meson-build-system)
   (native-search-paths
    '((search-path-specification
      (variable "GUILE_LOAD_COMPILED_PATH")
      (files '("guile")))
    (search-path-specification
      (variable "GUILE_LOAD_PATH")
      (files '("guile")))
    (search-path-specification
      (variable "GUILE_EXTENSIONS_PATH")
      (files '("guile")))
    ))
   (search-paths
    '((search-path-specification
      (variable "GUILE_LOAD_COMPILED_PATH")
      (files '("guile")))
    (search-path-specification
      (variable "GUILE_LOAD_PATH")
      (files '("guile")))
    (search-path-specification
      (variable "GUILE_EXTENSIONS_PATH")
      (files '("guile")))
    ))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("glib" ,glib "bin")             ; for gtester
      ("emacs" ,emacs-next)
      ("tzdata" ,tzdata-for-tests)
      ("gcc-toolchain" ,gcc-toolchain-9) ; for charconv
      ; ("guile" ,guile-3.0)
      ))   ; for mu/test/test-mu-query.c
   (inputs
    `(("xapian" ,xapian)
      ("guile" ,guile-3.0)
      ("glib" ,glib)
      ("gmime" ,gmime)))
   (arguments
     `(
       ;; #:modules ((guix build gnu-build-system)
       ;;            ((guix build emacs-build-system) #:prefix emacs:)
       ;;            (guix build utils))
       ;; #:imported-modules (,@%gnu-build-system-modules
       ;;                     (guix build emacs-build-system)
       ;;                     (guix build emacs-utils))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-tests
                    (lambda _
                      (substitute* (find-files "mu/tests" "\\.cc?$")
                        (("/bin/sh") (which "sh")))
                      (substitute* (find-files "guile/tests" "\\.cc?$")
                        (("/bin/sh") (which "sh")))
                      (substitute* '("lib/tests/bench-indexer.cc"
                                     "lib/utils/mu-test-utils.cc")
                        (("/bin/rm") (which "rm")))
                      #t))
                  (add-before 'check 'testr
                     (lambda _
                       (setenv "GUILE_EXTENSIONS_PATH" "./guile")
                       (setenv "GUILE_LOAD_PATH" "./guile")
                       (setenv "GUILE_LOAD_COMPILED_PATH" "./guile")
                       #t))

                  ;; (add-after 'unpack 'patch-notmuch-lib.el
                  ;;   (lambda _
                  ;;     (substitute* "emacs/notmuch-lib.el"
                  ;;       (("/bin/sh") (which "sh")))))
                  ;; (replace 'configure
                  ;;   (lambda* (#:key outputs #:allow-other-keys)
                  ;;     (setenv "CC" "gcc")
                  ;;     (setenv "CONFIG_SHELL" (which "sh"))

                  ;;     (let* ((out (assoc-ref outputs "out"))
                  ;;            (elisp (emacs:elpa-directory out)))
                  ;;       (invoke "./configure"
                  ;;               (string-append "--prefix=" out)
                  ;;               (string-append "--emacslispdir=" elisp)
                  ;;               (string-append "--emacsetcdir=" elisp)))))
                  ;; (add-after 'install 'make-autoloads
                  ;;   (assoc-ref emacs:%standard-phases 'make-autoloads)))
                ))
     )
   (home-page "http://www.djcbsoftware.nl/code/mu")
   (synopsis "Quickly find emails")
   (description
    "Mu is a tool for dealing with e-mail messages stored in the
Maildir-format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
   (license gpl3+))
  )
