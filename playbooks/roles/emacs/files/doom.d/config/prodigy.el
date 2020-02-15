;; Prodigy
(setq personal--repos-dir (expand-file-name "~/repos"))

(defvar personal--pyvenv-dir
  (expand-file-name "~/.virtualenvs")
  "path to dir containing python venvs.")

(defun personal--ensure-dir (path)
  (if (file-directory-p path)
      (file-name-as-directory path)
    (error "repo doesn't exist")))

(defun personal--repo-path (repo)
  (let ((path (concat (file-name-as-directory personal--repos-dir) repo)))
    (personal--ensure-dir path)))

(defun personal--pyvenv-path (venv)
  (let ((path (concat (file-name-as-directory personal--pyvenv-dir) venv)))
    (personal--ensure-dir path)))

(defun personal--pyvenv-python (venv)
  (concat (personal--pyvenv-path venv) "bin/python"))

;; (prodigy-define-service
;;   :name "biguns:5432->localhost:5433"
;;   :command "ssh"
;;   :args '("-L" "5433:localhost:5432" "biguns" "-N")
;;   :tags '(:deepmed))
;; (prodigy-define-service
;;   :name "allscratch webpack"
;;   :command "yarn"
;;   :tags '(allscratch frontend)
;;   :args '("start")
;;   :cwd (concat spacemacs--repos-dir "/allscratch"))
;; (prodigy-define-service
;;   :name "allscratch dev backend"
;;   :tags '(allscratch backend)
;;   :cwd (concat spacemacs--repos-dir "/allscratch")
;;   :command "/home/jtmoulia/.virtualenvs/allscratch/bin/python"
;;   :args (list (concat spacemacs--repos-dir "/allscratch/bin/runflask"))
;;   :env '(("ALLSCRATCH_ENV" "dev")))
