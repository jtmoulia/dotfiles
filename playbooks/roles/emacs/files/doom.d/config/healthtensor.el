;;; ../.dotfiles/playbooks/roles/emacs/files/doom.d/config/healthtensor.el -*- lexical-binding: t; -*-

(defvar org-babel-default-header-args:sql '() "Org babel SQL headers")

(defun ht/add-to-org-babel-sql-headers (key value)
  (setq org-babel-default-header-args:sql
        (cons `(,key . ,value) (assq-delete-all key org-babel-default-header-args:sql))))

(defun ht/set-db-headers (password)
  "Set the DB headers, read PASSWORD."
  (interactive "Pn")
  (let ((header-args `((:engine . "postgresql")
                       (:dbport . 5430)
                       (:dbhost . "localhost")
                       (:dbuser . "automed")
                       (:database . "automed")
                       (:dbpassword . ,password))))
    (cl-loop for (key . value) in header-args
          do (ht/add-to-org-babel-sql-headers key value))))

(defun ht/add-to-sql-connection-alist ()
  "Add to `sql-connection-alist' localhost 5430 automed connection."
  (setq sql-connection-alist
        `(
          ("automed-postgres-5434"
           (sql-product 'postgres)
           (sql-server "localhost")
           (sql-port 5434)
           (sql-user "automed")
           (sql-database "automed")
           ))))

;;; healthtensor.el ends here
