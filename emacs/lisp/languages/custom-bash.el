;;; custom-bash.el --- bash config
;;;
;;; Commentary:
;;;
;;; bash config
;;;
;;; Code:

;; on shell script save, make the file executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; custom-bash.el ends here
