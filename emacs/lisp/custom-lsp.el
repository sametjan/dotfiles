;;; custom-lsp.el
;;;
;;; Commentary
;;;
;;;
;;;
;;; Code:

(use-package lsp-mode
  :defer t
  :straight t
  :init
  (defvar lsp-tool "lsp")
  :hook ((js-mode . lsp)
	 (typescript-mode . lsp)
	 )
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  (lsp-enable-which-key-integration)
  )

(use-package lsp-ui
  :defer t
  :straight t
  :custom
  (lsp-ui-sideline-enable nil)
  )

;;; custom-lsp.el ends here
