;;; custom-pass.el --- pass configuration
;;;
;;; Commentary:
;;;
;;; Code:

(use-package pass
  :straight t
  :defer t
  :config
  (setq pass-username-fallback-on-file t))

;;; custom-pass.el ends here
