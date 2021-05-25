;;; custom-flycheck.el --- syntax checking
;;;
;;; Commentary:
;;;
;;; syntax checking
;;;
;;; Code:

(use-package flycheck
  :straight t
  :defer t
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    )
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  (setq flycheck-tmp-prefix ".tmp")

  (setq-default flycheck-check-syntax-automatically '(save))
  ;; (setq-default flycheck-disabled-checkers )
  (setq flycheck-mode-line-prefix "✔")

  (which-key-add-key-based-replacements "C-c !" "flycheck"))

;;; custom-flycheck.el ends here
