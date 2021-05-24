;;; custom-js.el --- js configuration
;;;
;;; Commentary:
;;;
;;; snippets, keybindings
;;;
;;; Code:

(use-package js
  :defer t
  :mode "\\*.js'"
  :config
  (define-key js-mode-map (kbd "M-,") nil)
  (define-key js-mode-map (kbd "C-c C-f") 'js-find-symbol)
  ;; (company-mode)
  ;; :general
  ;; (tyrant-def js-mode-map
  ;;             "mf" 'js-find-symbol)
  )

;;; custom-js.el ends here
