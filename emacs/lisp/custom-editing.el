;;; custom-editing.el --- configuration pertaining to editing text
;;;
;;; Commentary:
;;;
;;; code folding, syntax based packages, multiple cursors, etc
;;;
;;; Code:

;; zap to char using avy
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)

;; run comment-line instead of comment dwim
;; http://ergoemacs.org/misc/emacs_comment-line_vs_comment-dwim.html
;; I switched this out because comment-dwim breaks lines instead of commenting each line
;; in a region like comment-line does
;; also get rid of annoying web mode binding
(add-hook 'web-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-;"))))
(global-set-key (kbd "M-;") 'comment-line)

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (add-hook 'org-mode-hook (lambda () (setq-local smartparens-global-mode nil)))

  (show-smartparens-global-mode +1)
  (add-hook 'org-mode-hook (lambda () (setq-local show-smartparens-global-mode nil)))

  ;; (which-key-add-key-based-replacements "C-c s" "smartparens")
  (global-set-key (kbd "C-c s e") 'sp-show-enclosing-pair)
  (global-set-key (kbd "C-c s u") 'sp-up-sexp)
  (global-set-key (kbd "C-c s d") 'sp-down-sexp)
  (global-set-key (kbd "C-c s i") 'sp-change-enclosing)
  (global-set-key (kbd "C-c s c") 'sp-rewrap-sexp)
  )

(defun set-sp-face ()
  "Customize matching sp face."
  (add-hook 'smartparens-mode-hook
	    (lambda ()
	      (set-face-attribute 'sp-show-pair-match-face nil
				  :foreground "green"
				  :background nil
				  :weight 'normal
				  :underline nil
				  ))))
(apply-if-gui 'set-sp-face)

;; from smartparens documentation
(sp-local-pair 'prog-mode "{" nil :post-handlers '((my-create-newline-enter-sexp "RET")))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package origami
  :straight t
  :defer t
  :hook
  ((prog-mode) . origami-mode))

;; text-selection
(use-package expand-region
  :straight t
  :bind("C-;" . er/expand-region))

;; Utilities
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'undo)
;; (global-set-key (kbd "Cx C-e") 'eval-buffer)

(global-set-key (kbd "C-x C-l") 'mark-entire-line)
(global-set-key (kbd "M-<RET>") 'return-newline-below) ;; return new line below

(global-set-key [(control up)] 'move-lines-up)
(global-set-key [(control down)] 'move-lines-down)

;; Multiple Cursors
(use-package multiple-cursors
  :straight t
  :bind
  ("M-." . mc/mark-next-like-this)
  ("M-," . mc/mark-previous-like-this)
  ("C-c M-." . mc/mark-all-like-this)
  ("C-c C-e" . mc/edit-lines)
  )

;; keystroke completion
(use-package which-key
  :straight t
  :config
  (which-key-mode))

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

;; rename file and buffer
(global-unset-key (kbd "C-x C-r"))
(global-set-key (kbd "C-x C-r") 'rename-file-and-buffer)

;;; custom-editing.el ends here
