;;; Code:

;; for leanote-mode
(require 'cl-lib)
(require 'json)
(require 'request)
(require 'aborn-log)   ;; only for test

;;;;  Variables
(defvar leanote-user nil)
(defvar leanote-user-password nil)
(defvar leanote-user-email nil)
(defvar leanote-user-id nil)
(defvar leanote-token nil)
(defvar leanote-api-login "/auth/login")
(defvar leanote-api-getnotebooks "/notebook/getNotebooks")

(defcustom leanote-api-root
  "https://leanote.com/api"
  "api root"
  :group 'leanote-mode
  :type 'string)

(defcustom leanote-request-timeout 10
  "Timeout control for http request, in seconds."
  :group 'leanote-mode
  :type 'number)

(defcustom leanote-local-path
  "~/leanote/note"
  :group 'leanote-mode
  :type 'string)

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

(defun leanote-parser ()
  (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))

(defun leanote-get-note-books ()
  "get note books"
  (interactive)
  (request (concat leanote-api-root leanote-api-getnotebooks)
           :params '(("token" . leanote-token))
           :sync t
           :parser 'leanote-parser
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (if (not (arrayp data))
                           (progn
                             (message "get-note-book failed, cause: %s"
                                      (assoc-default 'Msg data)))  ;; NOTLOGIN
                         (progn
                           (message "finished. notebook number=%d" (length data)))))))
  )

(defun leanote-login (&optional user password)
  "login in leanote"
  (interactive)
  (when (null user)
    (setq user (read-string "Email: " nil nil leanote-user-email)))
  (when (null password)
    (setq password (read-passwd "Password: " nil leanote-user-password)))
  (message "email:%s, password%s" user password)
  (request (concat leanote-api-root leanote-api-login)
           :params '(("email" . user)
                     ("pwd" . password))
           :sync t
           :parser 'leanote-parser
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (if (equal :json-false (assoc-default 'Ok data))
                           (message "%s" (assoc-default 'Msg data))
                         (progn
                           (setq leanote-token (assoc-default 'Token data))
                           (setq leanote-user (assoc-default 'Username data))
                           (setq leanote-user-email (assoc-default 'Email data))
                           (setq leanote-user-id (assoc-default 'UserId data))
                           (message "login success!")))))
           )
  )

(provide 'leanote)
