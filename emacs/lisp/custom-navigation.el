;;; custom-navigation.el --- nav config
;;;
;;; Commentary:
;;;
;;; moving around files, buffers, perspectives, and windows in Emacs
;;;
;;; Code:

;; code-navigation
(use-package avy
  :straight t
  :bind (("M-g" . avy-goto-char)	             ;; Go to char
	 ("M-l" . avy-goto-lin)))      	             ;; go to line

(use-package ivy
  :straight t
  :defer 0.1
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-x B" . ivy-switch-buffer-other-window)
	 (:map ivy-minibuffer-map
	       ("C-c C-r" . ivy-resume)
	       ("C-c C-o" . ivy-occur)               ;; open list in buffer
	       ("<M-return>" . ivy-immediate-don))   ;; ignore suggestion and return current entry
	 )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package swiper
  :after ivy
  :straight t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

;; Beautify Ivy
(use-package ivy-rich
  :straight t
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev
	ivy-rich-switch-buffer-align-virtual-buffer t
	ivy-virtual-abbreviate 'full)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-M-l" . counsel-imenu)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)	;; don't start searches with ^
  )

;; project navigation
(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Development"))
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

;; Perspective navigation
(use-package perspective
  :straight t
  :bind (
	 ;; ("C-x b" . persp-switch-to-buffer*)
	 ("C-x k" . persp-kill-buffer*)
	 ("C-M-<left>" . persp-prev)
	 ("C-M-<right>" . persp-next)
	 ("C-M-<return>" . persp-switch)
	 ("C-M-<delete>" . persp-state-save)
	 ("C-M-<backspace>" . persp-state-load)
	 )
  :init
  (setq persp-initial-frame-name "main")
  (setq persp-sort 'created)
  (persp-mode)
  :config
  (setq persp-state-default-file "~/.emacs.d/save-perspective")
  (add-hook 'kill-emacs-hook #'persp-state-save)
  )
