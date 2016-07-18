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
(require 'pcache)    ;; for persistent

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
(defvar leanote-current-all-note-books nil)
(defvar leanote-current-note-book nil)
;; notebook-id -> notes-list(without content) map
(defvar leanote--cache-notebookid-notes (make-hash-table :test 'equal))
;; notebook-id -> notebook-info map
(defvar leanote--cache-notebookid-info (make-hash-table :test 'equal))
;; note-id -> notebook-info map
(defvar leanote--cache-noteid-info (make-hash-table :test 'equal))
;; local-path -> notebook-id
(defvar leanote--cache-notebook-path-id (make-hash-table :test 'equal))

;; pcache persistent repo name
(defconst leanote-persistent-repo "*leanote*")
(defconst leanote-log-buffer-name "*Leanote-Log*")

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
  "leanote mini mode"
  :init-value nil
  :lighter " leanote "
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c u") 'leanote-push-current-file-to-remote)
            (define-key map (kbd "C-c D") 'leanote-delete-current-note)
            map)
  :group 'leanote
  (leanote-init)
  (leanote-log "leanote minor mode initial!"))

(defun leanote-init ()
  "do some init work when leanote minor-mode turn on"
  (let ((repo (pcache-repository leanote-persistent-repo)))
    (when (= 0 (hash-table-count leanote--cache-noteid-info))
     (setq leanote--cache-noteid-info
          (leanote-persistent-get 'leanote--cache-noteid-info)))
    (when (= 0 (hash-table-count leanote--cache-notebook-path-id))
      (setq leanote--cache-notebook-path-id
            (leanote-persistent-get 'leanote--cache-notebook-path-id)))
    (when (= 0 (hash-table-count leanote--cache-notebookid-info))
      (setq leanote--cache-notebookid-info
            (leanote-persistent-get 'leanote--cache-notebookid-info)))
    (when (= 0 (hash-table-count leanote--cache-notebookid-notes))
     (setq leanote--cache-notebookid-notes
          (leanote-persistent-get 'leanote--cache-notebookid-notes))))
  (add-hook 'after-save-hook 'leanote-after-save-action)
  (leanote-log "finished leanote-init."))

(defun leanote-after-save-action ()
  "do some action after save markdown file"
  (let* ((full-file-name (buffer-file-name))
         (note-info nil)
         (is-modified nil)
         (noteid nil)
         (note-info-remote nil)
         (note-book-id nil))
    (when (string-suffix-p ".md" full-file-name)
      (setq note-book-id (gethash
                          (substring default-directory 0 (- (length default-directory) 1))
                          leanote--cache-notebook-path-id))
      (unless note-book-id
        (leanote-log "no releated notebook"))
      (when note-book-id
        (setq note-info-remote (leanote-get-note-info-base-note-full-name full-file-name))
        (setq note-info (gethash (assoc-default 'NoteId note-info-remote)
                                 leanote--cache-noteid-info))
        (setq is-modified (assoc-default 'IsModified note-info))
        (setq noteid (assoc-default 'NoteId note-info))
        (when (and note-info (not is-modified))
          (add-to-list 'note-info '(IsModified . t))
          (puthash noteid note-info leanote--cache-noteid-info)
          (leanote-persistent-put 'leanote--cache-noteid-info leanote--cache-noteid-info)
          (leanote-log (format "change file status when save. %s" full-file-name))
          ))
      )))

(defun leanote-persistent-put (key has-table)
  "put "
  (let ((repo (pcache-repository leanote-persistent-repo)))
    (pcache-put repo key has-table)))

(defun leanote-persistent-get (key)
  "get "
  (let ((repo (pcache-repository leanote-persistent-repo))
        (result nil))
    (setq result (pcache-get repo key))
    (unless (hash-table-p result)
      (setq result (make-hash-table :test 'equal)))
    result))

(defun leanote-sync ()
  "init it"
  (interactive)
  (leanote-log (format "--------start to sync leanote data:%s-------" (leanote--get-current-time-stamp)))
  (unless leanote-token
    (leanote-login))
  (unless leanote-token     ;; make sure login success.
    (leanote-log "login failed!")
    (error "login failed!"))
  (leanote-ajax-get-note-books)
  (unless (> (hash-table-count leanote--cache-noteid-info) 0)
    (setq leanote--cache-noteid-info   ;; restore from disk
          (leanote-persistent-get 'leanote--cache-noteid-info))
    (leanote-log "restore leanote--cache-noteid-info from disk."))
  ;; keep all notebook node info and store to hash table first
  (cl-loop for elt in (append leanote-current-all-note-books nil)
           collect
           (let* ((notebookid (assoc-default 'NotebookId elt)))
             (message "notebookid:%s  title:%s" notebookid (assoc-default 'Title elt))
             (puthash notebookid elt leanote--cache-notebookid-info)))
  (leanote-mkdir-notebooks-directory-structure leanote-current-all-note-books)
  (cl-loop for elt in (append leanote-current-all-note-books nil)
           collect
           (let* ((title (assoc-default 'Title elt))
                  (notebookid (assoc-default 'NotebookId elt))
                  (notes (leanote-ajax-get-notes notebookid)))
             (puthash notebookid notes leanote--cache-notebookid-notes)
             (leanote-log (format "notebook-name:%s, nootbook-id:%s, has %d notes."
                                  title notebookid (length notes)))
             (leanote-create-notes-files title notes notebookid)))
  (let ((local-cache (leanote-persistent-get 'leanote--cache-noteid-info)))
    (when (equal 0 (hash-table-count local-cache))
      (leanote-persistent-put 'leanote--cache-noteid-info leanote--cache-noteid-info)
      (leanote-log "notice: init leanote-persistent-put for leanote--cache-noteid-info")))
  (leanote-persistent-put 'leanote--cache-notebook-path-id leanote--cache-notebook-path-id)
  (leanote-persistent-put 'leanote--cache-notebookid-info leanote--cache-notebookid-info)
  (leanote-persistent-put 'leanote--cache-notebookid-notes leanote--cache-notebookid-notes)
  (leanote-log (format "--------finished sync leanote data:%s-------" (leanote--get-current-time-stamp)))
  )

(defun leanote--get-current-time-stamp ()
  "get current time stamp"
  (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

(defun leanote-create-notes-files (notebookname notes notebookid)
  "create&update all notes content in notebookname"
  (let* ((notebookroot (expand-file-name
                        (leanote-get-notebook-parent-path notebookid)
                        leanote-local-root-path)))
    (puthash notebookroot notebookid leanote--cache-notebook-path-id)
    (leanote-log (format "notebookroot=%s" notebookroot))
    (cl-loop for note in (append notes nil)
             collect
             (let* ((noteid (assoc-default 'NoteId note))
                    (title (assoc-default 'Title note))
                    (is-markdown-content (assoc-default 'IsMarkdown note))
                    (notecontent-obj (leanote-ajax-get-note-content noteid))
                    (notecontent (assoc-default 'Content notecontent-obj))
                    (note-local-cache (gethash noteid leanote--cache-noteid-info)))
               (when (eq t is-markdown-content)
                 (save-current-buffer
                   (let* ((filename (concat title ".md"))
                          (file-full-name (expand-file-name filename notebookroot)))
                     (if (file-exists-p file-full-name)
                         (progn
                           (leanote-log "file %s exists in local." file-full-name)
                           (let* ((is-modified (assoc-default 'IsModified note-local-cache)))
                             (if is-modified
                                 (leanote-log "local file %s has modified, sync error for this file."
                                              file-full-name)
                               (progn
                                 (find-file file-full-name)
                                 (setq buffer-read-only nil)
                                 (erase-buffer)
                                 (insert notecontent)
                                 (save-buffer)
                                 (puthash noteid note leanote--cache-noteid-info)
                                 (leanote-log "ok, file %s updated!" file-full-name)
                                 ))))
                       (progn
                         (leanote-log (format "file %s not exists in local." file-full-name))
                         (find-file file-full-name)
                         (insert notecontent)
                         (save-buffer)
                         (puthash noteid note leanote--cache-noteid-info)
                         (leanote-log (format "ok, file %s finished!" file-full-name))
                         )))))))))

(defun leanote-get-note-info-base-note-full-name (full-file-name)
  "get note info base note full name"
  (unless (string-suffix-p ".md" full-file-name)
    (error "file %s is not markdown file." full-file-name))
  (let* ((note-info nil)   ;; fefault return
         (notebook-id (gethash
                       (substring default-directory 0 (- (length default-directory) 1))
                       leanote--cache-notebook-path-id))
         (note-title (string-remove-suffix ".md"
                                           (string-remove-prefix
                                            default-directory
                                            full-file-name)))
         (notebook-notes nil))
    (unless notebook-id
      (error "sorry, cannot find any notes for notebook-id %s. %s"
             notebook-id
             "make sure this file is leanote file and you have login."))
    (setq notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
    (cl-loop for elt in (append notebook-notes nil)
             collect
             (when (equal note-title (assoc-default 'Title elt))
               (setq note-info elt)))   ;; keep remote value
    (unless note-info
      (setq note-info '())
      (add-to-list 'note-info `(NotebookId . ,notebook-id))
      (add-to-list 'note-info `(Title . ,note-title))
      (add-to-list 'note-info '(Usn . 0)))
    note-info))

(defun leanote-delete-current-note ()
  "delete current note"
  (interactive)
  (let* ((result-data nil)
         (note-info (leanote-get-note-info-base-note-full-name
                     (buffer-file-name)))
         (note-id (assoc-default 'NoteId note-info))
         (note-title (assoc-default 'Title note-info))
         (usn (assoc-default 'Usn note-info)))
    (unless note-id
      (error "cannot found current note for id %s" note-id))
    (when (yes-or-no-p (format "Do you really want to delete %s?" note-title))
      (setq result-data (leanote-ajax-delete-note note-id usn))
      ;; (setq leanote-debug-data result-data)   ;; TODO
      (if (and (listp result-data)
               (equal :json-false (assoc-default 'Ok result-data)))
          (error "delete note error, msg:%s." (assoc-default 'Msg result-data))
        (progn
          (unless result-data
            (error "error in delete note. reason: server error!"))
          (leanote-log "delete remote note %s success." note-title)
          (remhash note-id leanote--cache-noteid-info)
          (let ((name (buffer-file-name)))
            (delete name recentf-list)       ;; TODO is needed ?
            (kill-buffer)
            (leanote-log "local file %s was deleted." name)
            ))
        ))))

(defun leanote-ajax-delete-note (note-id usn)
  "delete note"
  (let* ((result nil)
         (usn-str (number-to-string (+ 1 usn))))
    (request (concat leanote-api-root "/note/deleteTrash")
             :params `(("token" . ,leanote-token)
                       ("noteId" . ,note-id)
                       ("usn" . ,usn-str))
             :sync t
             :type "POST"
             :parser 'leanote-parser
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq result data)))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (leanote-log "Got error: %S" error-thrown)
                                   (error "Got error: %S" error-thrown)))
             )
    result))

(defun leanote-push-current-file-to-remote ()
  "push current content or add new note to remote server."
  (interactive)
  (let* ((note-info (leanote-get-note-info-base-note-full-name
                     (buffer-file-name)))
         (result-data nil)
         (notebook-id (gethash
                       (substring default-directory 0 (- (length default-directory) 1))
                       leanote--cache-notebook-path-id))
         (notebook-info (gethash notebook-id leanote--cache-notebookid-info))
         (notebook-title (assoc-default 'Title notebook-info))
         (note-id (assoc-default 'NoteId note-info)))
    (if note-id
        (progn     ;; modify exists note
          (setq note-info (gethash note-id leanote--cache-noteid-info))
          (unless note-info
            (error "cannot find current note info for id %s in local cache." note-id))
          (setq result-data (leanote-ajax-update-note note-info (buffer-string)))
          ;; (setq leanote-debug-data result-data)
          (if (and (listp result-data)
                   (equal :json-false (assoc-default 'Ok result-data)))
              (error "push to remote error, msg:%s." (assoc-default 'Msg result-data))
            (progn
              (unless result-data
                (error "error in push(update note) to server. reason: server error!"))
              (leanote-log "push(update note) to remote success.")
              (message "push(update note) to remote success.")
              (puthash note-id result-data leanote--cache-noteid-info))
            ))
      (progn       ;; add new note
        (unless notebook-id
          (error "cannot find any notebook for this file."))
        (when (yes-or-no-p (format "The note was not found in notebook `%s'. Do you want to add it?"
                                   notebook-title))
          (add-to-list 'note-info '(NoteId . "0"))
          ;;(setq leanote-debug-data (gethash notebook-id leanote--cache-notebookid-notes))
          (setq result-data (leanote-ajax-update-note note-info (buffer-string) "/note/addNote"))
          (if (and (listp result-data)
                   (equal :json-false (assoc-default 'Ok result-data)))
              (error "push(add new note) to remote error, msg:%s." (assoc-default 'Msg result-data))
            (progn
              (unless result-data
                (error "error in push(add new note) to server. reason: server error!"))
              (leanote-log "push(add new note) to remote success.")
              (message "push(add new note) to remote success.")
              (let* ((notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
                     (notebook-notes-new (vconcat notebook-notes (vector result-data))))
                (setq note-id (assoc-default 'NoteId result-data))
                (unless note-id
                  (error "error in local data operate!"))
                (puthash notebook-id notebook-notes-new leanote--cache-notebookid-notes)
                (puthash note-id result-data leanote--cache-noteid-info)))
            )))
      )
    )
  )

(defun leanote-ajax-update-note (note-info note-content &optional api)
  "update note"
  (when (null api)
    (setq api "/note/updateNote"))
  (leanote-log "leanote-ajax-update-note api=%s" api)
  (let* ((result nil)
         (usn (assoc-default 'Usn note-info))
         (new-usn (+ 1 usn))
         (new-usn-str (number-to-string usn))
         (note-id (assoc-default 'NoteId note-info))
         (notebook-id (assoc-default 'NotebookId note-info))
         (note-title (assoc-default 'Title note-info)))
    (request (concat leanote-api-root api)
             :params `(("token" . ,leanote-token)
                       ("NoteId" . ,note-id)
                       ("Usn" . ,new-usn-str)
                       ("NotebookId" . ,notebook-id)
                       ("Title" . ,note-title)
                       ("IsMarkdown" . "true")
                       ("Abstract" . ,note-content)
                       ("Content" . ,note-content))
             :sync t
             :type "POST"
             :parser 'leanote-parser
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq result data)))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (leanote-log "Got error: %S" error-thrown)
                                   (error "Got error: %S" error-thrown)))
             )
    result))

(defun leanote-parser ()
  "parser"
  (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))

(defun leanote-ajax-get-note-books ()
  "get note books"
  (interactive)
  (let ((note-books (leanote-common-api-action leanote-api-getnotebooks)))
    (when note-books
      (setq leanote-current-all-note-books note-books)
      (leanote-log (format "Got %d notebooks." (length note-books)))
      note-books)))

(defun leanote-ajax-get-note-content (noteid)
  "get note content, return type.Note"
  (interactive)
  (leanote-common-api-action leanote-api-getnotecontent "noteId" noteid))

(defun leanote-ajax-get-notes (notebookid)
  "get all notes-info in notebook"
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

;; (leanote-get-notebook-parent-path "5789af43c3b1f40b51000009")
;; "其他笔记/其他语言学习"
(defun leanote-get-notebook-parent-path (parentid)
  "get notebook parent path"
  (if (not parentid)
      ""
    (progn (let* ((cparent (gethash parentid leanote--cache-notebookid-info))
                  (cparent-title (assoc-default 'Title cparent))
                  (cparent-parent-id (assoc-default 'ParentNotebookId cparent))
                  (cparent-no-parent (string= "" cparent-parent-id)))
             (if cparent-no-parent
                 cparent-title
               (concat (leanote-get-notebook-parent-path cparent-parent-id) "/" cparent-title))
             ))))

(defun leanote-mkdir-notebooks-directory-structure (&optional all-notebooks)
  "make note-books hierarchy"
  (interactive)
  (unless (file-exists-p leanote-local-root-path)
    (leanote-log "make root dir %s" leanote-local-root-path)
    (make-directory leanote-local-root-path t))
  (when (null all-notebooks)
    (leanote-log "all-notebooks not provided.")
    (setq all-notebooks leanote-current-all-note-books))
  (cl-loop for elt in (append all-notebooks nil)
           collect
           (let* ((title (assoc-default 'Title elt))
                  (notebook-id (assoc-default 'NotebookId elt))
                  (parent-id (assoc-default 'ParentNotebookId elt))
                  (has-parent (not (string= "" parent-id)))
                  (current-notebook-path (expand-file-name title leanote-local-root-path)))
             (leanote-log (format "title=%s" title))
             (when has-parent
               (leanote-log (format "title=%s has parent" title))
               (setq current-notebook-path (expand-file-name
                                            (leanote-get-notebook-parent-path notebook-id)
                                            leanote-local-root-path)))
             (unless (file-exists-p current-notebook-path)
               (leanote-log (format "notebook:%s, path:%s" title current-notebook-path))
               (make-directory current-notebook-path t)
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
                           (leanote-log "%s" (assoc-default 'Msg data))
                         (progn
                           (setq leanote-token (assoc-default 'Token data))
                           (setq leanote-user (assoc-default 'Username data))
                           (setq leanote-user-email (assoc-default 'Email data))
                           (setq leanote-user-id (assoc-default 'UserId data))
                           (setq leanote-user-password password) ;; update password
                           (leanote-log "login success!")))))))

(defun leanote-log (&rest args)
  "log message to `leanote-log-buffer-name'"
  (let* ((buf (get-buffer-create leanote-log-buffer-name))
         (local-current-time (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time))))
    (with-current-buffer buf
      (insert (concat local-current-time (string-join args " ")))
      (message (concat local-current-time (string-join args " ")))
      (insert "\n")
      )))

(provide 'leanote)
;;; leanote.el ends here
