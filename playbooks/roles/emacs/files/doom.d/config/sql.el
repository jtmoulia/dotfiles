;;; ~/.dotfiles/playbooks/roles/emacs/files/doom.d/config/sql.el -*- lexical-binding: t; -*-

(setq sql-connection-alist
      '(
        ("postgres-5431"
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-port 5431)
         (sql-user "automed")
         (sql-password "automed")
         (sql-database "automed")
         )))

;;; sql.el ends here
