;;; custom-markdown.el --- markdown configuration
;;;
;;; Commentary:
;;;
;;; markdown preview
;;;
;;; Code:

(use-package markdown-mode
  :straight t
  :defer t
  :bind("C-c m" . markdown-live-preview-mode)
  :mode(("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  ;; :general
  ;; (tyrant-def markdown-mode-map
  ;;             "mp" 'markdown-live-preview-mode)
  )

;;; custom-markdown.el ends here
