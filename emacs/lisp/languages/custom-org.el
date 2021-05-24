;;; custom-org.el --- org mode config
;;;
;;; Commentary:
;;;
;;; org mode
;;;
;;; Code:

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

;; start github review with given link at point
(defun github-start-review-at-link ()
  "Copies the URL from an org link at the point and starts github review"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
	(progn
	  (github-review-start plain-url)))))

(use-package org
  :straight t
  :defer t
  :config
  (setq org-support-shift-select t)
  :bind (:map org-mode-map
	      ("<M-return>" . org-return-newline-below)
	      ("C-`" . org-open-at-point-plaintext)
	      ("<C-delete" . org-remove-link)
	      ("<C-escape>" . org-mark-ring-goto)
	      ;; ("<return>" . bcm/org-return)
	      ("C-c r" . github-start-review-at-link)
	      ))

(use-package org-drill
  :defer t
  :straight t)

;; pretty bullets
(use-package org-bullets
  :defer t
  :straight t
  :hook (org-mode . org-bullets-mode))

(setq org-return-follow-links t)
(setq org-agenda-files '("~/Documents/OrgFiles"))

;; overwrite web mode binding for C-c C-l
(add-hook 'web-mode-hook
	  (lambda ()
	    (local-unset-key (kbd "C-c C-l"))))
(global-set-key (kbd "C-c C-l") 'org-store-link)

(use-package ox-reveal
  :defer t
  :straight t)

(setq org-reveal-root
      ;; "https://cdnjs.com/libraries/reveal.js/3.6.0"
      "reveal.js"
      )
(setq org-reveal-mathjax t)

(use-package htmlize
  :defer t
  :straight t)

;;; custom-rog.el ends her
