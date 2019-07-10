(defvar personal-ansible--vault-password-files
  `(
    (,(expand-file-name "~/repos/automed/hippos/roles/automed/files/Prod.yml")
     . ,(expand-file-name "~/repos/automed/hippos/Prod-password"))
    (,(expand-file-name "~/repos/automed/hippos/roles/automed/files/test.yml")
     . ,(expand-file-name "~/repos/automed/hippos/test-password"))
    )
  "Personal ansible vault password files.")

(defun personal-ansible//set-ansible-vault-password-file ()
  "Set the ansible vault password file using the buffer file
to look up the password file from `personal-ansible--vault-password-files'"
  (if-let ((password-file (alist-get buffer-file-name personal-ansible--vault-password-files)))
      (set-variable 'ansible-vault-password-file) password-file))

(add-hook 'yaml-mode-hook 'personal-ansible//set-ansible-vault-password-file)
