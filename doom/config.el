;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Steven Ametjan"
      user-mail-address "steve@hittingthebottle.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 12 :weight 'regular)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono NL" :size 13 :weight 'light))

;; Display Powerline symbols correctly in vTerm
(setq doom-emoji-fallback-font-families nil)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/OrgFiles/")

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Mail setup
(setq sendmail-program "/opt/homebrew/bin/msmtp"
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function #'message-send-mail-with-sendmail)
(set-email-account! "HTB"
                    '((mu4e-sent-folder       . "/htb/Sent")
                      (mu4e-drafts-folder     . "/htb/Drafts")
                      (mu4e-trash-folder      . "/htb/Trash")
                      (mu4e-refile-folder     . "/htb/Archive")
                      (smtpmail-smtp-user     . "steve@hittingthebottle.com")
                      (user-mail-address      . "steve@hittingthebottle.com")
                      (mu4e-compose-signature . "--\nSteven Ametjan"))
                    t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! mu4e-views
  :after mu4e
  :init
  (setq mu4e-views-completion-method 'vertico
              mu4e-views-default-view-method 'html
              mu4e-views-next-previous-message-behavior 'stick-to-current-window
              mu4e-views-auto-view-selected-message t)
  (map! :localleader
        :map mu4e-headers-mode-map
        :desc "Select viewing method" "v" #'mu4e-views-mu4e-select-view-msg-method
        :desc "Toggle auto view selected messages" "f" #'mu4e-views-toggle-auto-view-selected-message
        :desc "View as nonblocked html" "i" #'mu4e-views-mu4e-view-as-nonblocked-html)
  (map! :map mu4e-headers-mode-map
        :desc "Scroll email view down" "M-n" #'mu4e-views-cursor-msg-view-window-down
        :desc "Scroll email view up" "M-p" #'mu4e-views-cursor-msg-view-window-up))

(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. if FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (with-current-buffer (current-buffer)
      (save-excursion
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-iso-week-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))
    )
  )

(map! :map org-mode-map
      :localleader
      (:prefix ("r" . "refile")
       "d" #'org-refile-to-datetree))

;; Disable LSP Formatting
(setq +format-with-lsp nil)

(use-package! lsp-tailwindcss)
(use-package! prisma-mode)
(setq-hook! 'web-mode +format-with prettier)
