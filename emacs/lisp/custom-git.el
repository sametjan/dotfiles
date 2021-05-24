;;; custom-git.el --- git/magit config
;;;
;;; Commentary:
;;;
;;; Version control stuff
;;;
;;; Code:

(use-package magit
  :defer t
  :straight t
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout)
  ("C-x l" . magit-log-branches))

(use-package github-review
  :defer t
  :straight t
  :after magit
  :bind (:map magit-mode-map
	      ("C-c r" . github-review-forge-pr-at-point)))

(use-package forge
  :defer t
  :straight t
  :after magit)

(use-package git-link
  :defer t
  :straight t
  :bind
  ("C-c t" . git-link))

;;; custom-git.el ends here
