;;; leanote.el --- Minor mode for leanote in markdown file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5") (request "0.2") (let-alist "1.0.3"))
;; Keywords: leanote, note, markdown
;; Homepage: https://github.com/aborn/leanote-mode
;; URL: https://github.com/aborn/leanote-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emacs use leanote

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'let-alist)

;;;;  Variables

;; for debug
(defvar leanote-debug-data nil)

;; user info
(defvar leanote-user nil)
(defvar leanote-user-password nil)
(defvar leanote-user-email nil)
(defvar leanote-user-id nil)
(defvar leanote-token nil)

;; local cache 
(defvar leanote-current-note-book nil)

;; api
(defvar leanote-api-login "/auth/login")
(defvar leanote-api-getnotebooks "/notebook/getNotebooks")
(defvar leanote-api-getnotecontent "/note/getNoteContent")
(defvar leanote-api-getnoteandcontent "/note/getNoteAndContent")
(defvar leanote-api-getnotes "/note/getNotes")

(defcustom leanote-api-root "https://leanote.com/api"
  "api root"
  :group 'leanote
  :type 'string)

(defcustom leanote-request-timeout 10
  "Timeout control for http request, in seconds."
  :group 'leanote
  :type 'number)

(defcustom leanote-local-root-path "~/leanote/note"
  "local leanote path"
  :group 'leanote
  :type 'string)

(defgroup leanote nil
  "leanote mini group"
  :prefix "leanote-"
  :group 'external)

(define-minor-mode leanote
  "leanot mini mode"
  :init-value nil
  :lighter " leanote"
  :keymap '(([C-c m] . leanote-init))
  :group 'leanote)

(defun leanote-init ()
  "init it"
  (interactive)
  (unless leanote-token
    (message "please login first.")
    (leanote-login))
  (leanote-get-note-books)
  (leanote-mkdir-notebooks-directory-structure leanote-current-note-book)
  (cl-loop for elt in (append leanote-current-note-book nil)
           collect
           (let* ((title (assoc-default 'Title elt))
                  (notebookid (assoc-default 'NotebookId elt))
                  (notes (leanote-get-notes notebookid)))
             (message "title:%s, nootbookid:%s, has %d notes." title notebookid (length notes))
             (leanote-create-notes-files title notes)
             )
           )
  (message "leanote start."))

(defun leanote-create-notes-files (notebookname notes)
  "create note files"
  (let* ((notebookroot (expand-file-name notebookname leanote-local-root-path)))
    (cl-loop for note in (append notes nil)
             collect
             (let* ((noteid (assoc-default 'NoteId note))
                    (title (assoc-default 'Title note))
                    (is-markdown-content (assoc-default 'IsMarkdown note))
                    (notecontent-obj (leanote-get-note-content noteid))
                    (notecontent (assoc-default 'Content notecontent-obj)))
               (message "ismarkdown:%s, title:%s, content:%s" is-markdown-content title notecontent)
               (when (eq t is-markdown-content)
                 (message "ok, is markdown!")
                 )
               )
             )
    )
  )

(defun leanote-parser ()
  "parser"
  (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))

(defun leanote-get-note-content (noteid)
  "get note content, return type.Note"
  (interactive)
  (leanote-common-api-action leanote-api-getnotecontent "noteId" noteid))

(defun leanote-get-notes (notebookid)
  "get notebook notes list"
  (interactive)
  (leanote-common-api-action leanote-api-getnotes "notebookId" notebookid))

(defun leanote-get-note-and-content (noteid)
  "get note and content, return  type.Note"
  (interactive)
  (leanote-common-api-action leanote-api-getnoteandcontent "noteId" noteid))

(defun leanote-common-api-action (api &optional param-key &optional param-value)
  "common api only one parameter"
  (let ((result nil))
    (request (concat leanote-api-root api)
             :params `(("token" . ,leanote-token) (,param-key . ,param-value))
             :sync t
             :parser 'leanote-parser
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq leanote-debug-data data)  ;; TODO
                         (if (arrayp data)
                             (setq result data)
                           (progn (unless (eq (assoc-default 'Ok leanote-debug-data) :json-false)
                                    (setq result data))
                                  )))))
    result))

(defun leanote-get-note-books ()
  "get note books"
  (interactive)
  (let ((note-books (leanote-common-api-action leanote-api-getnotebooks)))
    (when note-books
      (setq leanote-current-note-book note-books)
      (message "finished. total:%d" (length note-books))
      note-books)))

(defun leanote-mkdir-notebooks-directory-structure (note-books-data)
  "make note-books hierarchy"
  (unless (file-exists-p leanote-local-root-path)
    (message "make root dir %s" leanote-local-root-path)
    (make-directory leanote-local-root-path t))
  (cl-loop for elt in (append note-books-data nil)
           collect
           (let* ((title (assoc-default 'Title elt))
                  (has-parent (not (string= "" (assoc-default 'ParentNotebookId elt))))
                  (current-note-book (expand-file-name title leanote-local-root-path)))
             (message "title=%s" title)
             (when (and (not has-parent) (not (file-exists-p current-note-book)))
               (make-directory current-note-book)
               ))
           ))

(defun leanote-login (&optional user password)
  "login in leanote"
  (interactive)
  (when (null user)
    (setq user (read-string "Email: " nil nil leanote-user-email)))
  (when (null password)
    (setq password (read-passwd "Password: " nil leanote-user-password)))
  (request (concat leanote-api-root leanote-api-login)
           :params `(("email" . ,user)
                     ("pwd" . ,password))
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
                           (setq leanote-user-password password) ;; update password
                           (message "login success!")))))))

(provide 'leanote)
