;; TODO: this requires the ansible mode to be loaded, which can cause issues
(defvar personal-ansible--vault-password-files
  `(
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/Prod.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/Prod-password"))
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/test.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/Test-password"))
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/AutomedTest.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/AutomedTest-password"))
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/CaseReview.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/CaseReview-password"))
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/CernerTest.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/CernerTest-password"))
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/PHHTest.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/PHHTest-password"))
    (,(intern (expand-file-name "~/repos/automed/hippos/roles/automed/files/EpicTest.yml"))
     . ,(expand-file-name "~/repos/automed/hippos/EpicTest-password"))
    )
  "alist of personal ansible vault password files where the keys are symbols")

(defun personal-ansible//set-ansible-vault-password-file ()
  "Set the ansible vault password file using the buffer file
to look up the password file from `personal-ansible--vault-password-files'"
  (if-let ((password-file (alist-get (intern buffer-file-name) personal-ansible--vault-password-files)))
      (set-variable 'ansible-vault-password-file password-file)))

(add-hook 'yaml-mode-hook 'personal-ansible//set-ansible-vault-password-file)
