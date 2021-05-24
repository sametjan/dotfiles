;;; custom-autocomplete.el --- autocomplete configuration
;;;
;;; Commentary:
;;;
;;; autocompletion with company mode
;;;
;;; Code:

(use-package company
  :defer t
  :straight t
  :after eldoc
  :init
  (progn
    (global-company-mode)
    (setq company-tooltip-limit 20)	; bigger popup window
    (setq company-idle-delay nil)	; don't autocomplete on typing, backtab instead
    ;; (setq company-idle-delay .3)     ; decrease delay before autocompletion popup shows
    (setq company-echo-delay 0)		; remove annoying blinking
    (setq company-begin-commands '(self-insert-command)) ;start autocompletion after typing, if we want to ignore our special tab key we bind below
    ))

(use-package company-box
  :straight t
  :defer t
  :hook
  (company-mode . company-box-mode))

(global-set-key [backtab] 'company-complete-common) ;; backtab triggers autocomplete

(use-package yasnippet
  :defer t
  :straight t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/js-mode")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (yas-global-mode 1)
  )

;;; custom-autocomplete.el ends here
