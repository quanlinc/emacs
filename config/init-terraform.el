;;; init-terraform.el --- Terraform configurations
;;; Commentary:
;;; Code:

;;; Terraform

(when (maybe-require-package 'terraform-mode)
  (with-eval-after-load 'terraform-mode
    (when (maybe-require-package 'reformatter)
      (reformatter-define terraform-format
        :program "terraform" :args '("fmt", "-")))))

(provide 'init-terraform)
;;; init-terraform.el ends here
