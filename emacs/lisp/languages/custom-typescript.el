;;; custom-typescript.el --- ts configuration
;;;
;;; Commentary:
;;;
;;; This is really just the web mode config but since
;;; we use it for tsx files primarily here it is
;;;
;;; Code:

(use-package typescript-mode
  :defer t
  :straight t)

(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.[jt]sx\\'" . web-mode)
         ("\\.html\\'" . web-mode)))

;;; custom-typescript.el ends here
