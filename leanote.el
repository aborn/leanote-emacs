;;; Code:

;; for leanote-mode
(require 'cl-lib)
(require 'json)
(require 'request)

;;;;  Variables
(defvar leanote-user nil)
(defvar leanote-password nil)
(defvar leanote-token nil)
(defvar leanote-api-login "/auth/login")

(defcustom leanote-api-root
  "https://leanote.com/api"
  "api root"
  :group 'leanote-mode
  :type 'string)

(defcustom leanote-request-timeout 10
  "Timeout control for http request, in seconds."
  :group 'leanote-mode
  :type 'number)

(defgroup leanote-mode nil
  "leanote mini group"
  :prefix "leanote-"
  :group 'external)

(define-minor-mode leanote-mode
  "leanote mini-mode"
  :init-value nil
  :lighter " leanote"
  :keymap '(([C-c m] . leanote-init))
  :group 'leanote-mode)

(defun leanote-init ()
  "init it"
  (interactive)
  (message "leanote start."))

(defun leanote-login (&optional user password)
  "login in leanote"
  (interactive)
  (when (null user)
    (setq user (read-string "User: " nil nil leanote-user)))
  (when (null password)
    (setq password (read-passwd "Password: " nil leanote-password)))
  )

(provide 'leanote)
