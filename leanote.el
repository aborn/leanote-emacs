;;; leanote.el --- A minor mode writing markdown leanote  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Aborn Jiang

;; Author: Aborn Jiang <aborn.jiang@gmail.com>
;; Version: 0.2.5
;; Package-Requires: ((cl-lib "0.5") (request "0.2") (let-alist "1.0.3") (pcache "0.4.0") (s "1.10.0") (swiper "0.8.0") (helm "1.9.9"))
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

;; The leanote package is a minor mode for writing note in markdown file.
;; Before use this package, please spend a few minutes to learn leanote, which
;; is an open-source platform https://github.com/leanote/leanote. The leanote
;; office provide note server, android & ios apps.
;;
;; The emacs leanote package provides follwoing features:
;; * M-x leanote-login ------ login to server.
;; * M-x leanote-sync  ------ sync all notes from server to local.
;; * M-x leanote-push  ------ push current note to remote server (include create new).
;; * M-x leanote-pull  ------ pull(update) current note from server.
;; * M-x leanote-find find -- all notes in current account(default use swiper).
;; * M-x leanote-helm-find -- find all notes in current account(helm).
;; * M-x leanote-delete ----- delete current note
;;
;; Here is hot-keys
;; C-c u --- update/create note to remote server.
;; C-c r --- rename note
;; C-c f --- leanote-find

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'let-alist)
(require 'pcache)    ;; for persistent
(require 's)
(require 'ivy)
(require 'helm)
(require 'subr-x)

;;;;  Variables

;; for debug
(defvar leanote-debug-data nil)

;; user info
(defvar leanote-user nil)
(defvar leanote-user-password nil)
(defvar leanote-user-email nil)
(defvar leanote-user-id nil)
(defvar leanote-token nil)

(defvar leanote-current-all-note-books nil)
(defvar leanote-current-note-book nil)

;; timer task & locker
(defvar leanote-idle-timer nil)
(defvar leanote-task-locker nil)

;;; local cache 
;; notebook-id -> notes-list(without content) map
(defvar leanote--cache-notebookid-notes (make-hash-table :test 'equal))
;; notebook-id -> notebook-info map
(defvar leanote--cache-notebookid-info (make-hash-table :test 'equal))
;; note-id -> notebook-info map
(defvar leanote--cache-noteid-info (make-hash-table :test 'equal))
;; local-path -> notebook-id map
(defvar leanote--cache-notebook-path-id (make-hash-table :test 'equal))
;; record is need update
(defvar leanote--cache-note-update-status (make-hash-table :test 'equal))
;; pcache persistent repo name
(defconst leanote-persistent-repo "*leanote*")

;; log buffer name
(defconst leanote-log-buffer-name "*Leanote-Log*")

;; leanote group
(defgroup leanote nil
  "leanote mini group"
  :prefix "leanote-"
  :group 'external)

;; api
(defcustom leanote-api-login "/auth/login"
  "login api"
  :group 'leanote
  :type 'string)

(defcustom leanote-api-getnotebooks "/notebook/getNotebooks"
  "get note books api"
  :group 'leanote
  :type 'string)

(defcustom leanote-api-getnotecontent "/note/getNoteContent"
  "get conte content api"
  :group 'leanote
  :type 'string)

(defcustom leanote-api-getnoteandcontent "/note/getNoteAndContent"
  "get note and content api"
  :group 'leanote
  :type 'string)

(defcustom leanote-api-getnotes "/note/getNotes"
  "get notes api"
  :group 'leanote
  :type 'string)

(defcustom leanote-api-root "https://leanote.com/api"
  "api root"
  :group 'leanote
  :type 'string)

(defcustom leanote-request-timeout 10
  "Timeout control for http request, in seconds."
  :group 'leanote
  :type 'number)

(defcustom leanote-idle-interval 1
  "idle timer to execute check update"
  :group 'leanote
  :type 'number)

(defcustom leanote-check-interval (* 60 3)
  "note status check interval, default 3 minutes"
  :group 'leanote
  :type 'number)

(defcustom leanote-local-root-path "~/leanote/note"
  "local leanote path"
  :group 'leanote
  :type 'string)

(defcustom leanote-log-level "info"
  "which level log should be printed,
   can be `info', `warning', `error'"
  :group 'leanote
  :type 'string)

(defcustom leanote-mode nil
  "toggle leanote-mode."
  :require 'leanote
  :type 'boolean
  :group 'leanote)

;; minor mode
(define-minor-mode leanote
  "leanote mini mode"
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c u") 'leanote-push)
            (define-key map (kbd "C-c r") 'leanote-rename)
            (define-key map (kbd "C-c f") 'leanote-find)
            (define-key map (kbd "C-c o") 'leanote-pull)
            map)
  :group 'leanote
  (leanote-init)
  (leanote-log "leanote minor mode initial!"))

;;;###autoload
(defun leanote-init ()
  "do some init work when leanote minor-mode turn on"
  (unless leanote-mode
    (setq leanote-mode t))
  (unless (assq 'leanote-mode minor-mode-alist)
    (add-to-list 'minor-mode-alist '(leanote-mode " Ⓛ") t))
  (spaceline-define-segment leanote-status
    "show the leanote status"
    (when leanote-mode
      (powerline-raw
       (s-trim (leanote-status))))
    :when active)
  ;; (unless (assq '(leanote-status :when active) spaceline-left)  ;; spaceline-left only in 1.x version
  ;;   (add-to-list 'spaceline-left '(leanote-status :when active) t))
  (spaceline-spacemacs-theme 'leanote-status) ;; install leanote-status to spaceline--mode-lines
  (spaceline-compile)
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
          (leanote-persistent-get 'leanote--cache-notebookid-notes)))
  (leanote-check-note-update)
  (add-hook 'after-save-hook 'leanote-after-save-action))

(defun leanote-after-save-action ()
  "do some action when markdown file are saved."
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
          (cl-pushnew '(IsModified . t) note-info)
          (puthash noteid note-info leanote--cache-noteid-info)
          (leanote-persistent-put 'leanote--cache-noteid-info leanote--cache-noteid-info)
          (force-mode-line-update)
          (leanote-log (format "change file status when save. %s" full-file-name))
          ))
      )))

(defun leanote-persistent-put (key value)
  "save key value to persistent cache."
  (let ((repo (pcache-repository leanote-persistent-repo)))
    (pcache-put repo key value)))

(defun leanote-persistent-get (key)
  "get value from persistent cache."
  (let ((repo (pcache-repository leanote-persistent-repo))
        (result nil))
    (setq result (pcache-get repo key))
    (unless (hash-table-p result)
      (setq result (make-hash-table :test 'equal)))
    result))

;;;###autoload
(defun leanote-sync ()
  "sync notebooks and notes from remote server."
  (interactive)
  (leanote-log (format "--------start to sync leanote data:%s-------"
                       (leanote--get-current-time-stamp)))
  (leanote-make-sure-login)
  (leanote-ajax-get-note-books)
  (unless (> (hash-table-count leanote--cache-noteid-info) 0)
    (setq leanote--cache-noteid-info   ;; restore noteid info from persistent cache.
          (leanote-persistent-get 'leanote--cache-noteid-info))
    (leanote-log "restore leanote--cache-noteid-info."))
  ;; keep all notebook node info and store to hash table first
  (cl-loop for elt in (append leanote-current-all-note-books nil)
           collect
           (let* ((notebookid (assoc-default 'NotebookId elt)))
             (leanote-log (format "notebookid:%s  title:%s" notebookid (assoc-default 'Title elt)))
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
  (leanote-log (format "--------finished sync leanote data:%s-------" (leanote--get-current-time-stamp))))

(defun leanote--get-current-time-stamp ()
  "get current time stamp"
  (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

(defun leanote-create-notes-files (notebookname notes notebookid)
  "create/update all notes in `notebookname'"
  (let* ((notebookroot (expand-file-name
                        (leanote-get-notebook-parent-path notebookid)
                        leanote-local-root-path)))
    (puthash notebookroot notebookid leanote--cache-notebook-path-id)
    (leanote-log (format "notebookroot=%s, notebookname=%s" notebookroot notebookname))
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
                           (leanote-log (format "file %s exists in local." file-full-name))
                           (let* ((is-modified (assoc-default 'IsModified note-local-cache)))
                             (if is-modified
                                 (leanote-log "error" (format "local file %s has modified, sync error for this file."
                                                              file-full-name))
                               (progn
                                 (find-file file-full-name)
                                 (setq buffer-read-only nil)
                                 (erase-buffer)
                                 (insert notecontent)  ;; if notecontent is nil ?
                                 (save-buffer)
                                 (puthash noteid note leanote--cache-noteid-info)
                                 (leanote-log (format "ok, local file %s updated!" file-full-name))
                                 ))))
                       (progn
                         (leanote-log (format "file %s not exists in local." file-full-name))
                         (find-file file-full-name)
                         (insert notecontent)
                         (save-buffer)
                         (puthash noteid note leanote--cache-noteid-info)
                         (leanote-log (format "ok, local file %s created!" file-full-name))
                         )))))))))

(defun leanote-get-note-info-base-note-full-name (ffn)
  "get note info base note full file name `ffn'"
  (unless (string-suffix-p ".md" ffn)
    (error (format "file %s is not markdown file." ffn)))
  (let* ((note-info nil)   ;; fefault return
         (notebook-id (gethash
                       (substring default-directory 0 (- (length default-directory) 1))
                       leanote--cache-notebook-path-id))
         (note-title (file-name-base ffn))
         (notebook-notes nil))
    (unless notebook-id
      (error (format "sorry, cannot find any notes for notebook-id %s. %s"
                     notebook-id
                     "make sure this file is leanote file and you have login.")))
    (setq notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
    (cl-loop for elt in (append notebook-notes nil)
             collect
             (when (equal note-title (assoc-default 'Title elt))
               (setq note-info elt)))   ;; keep remote value
    (unless note-info
      (setq note-info '())
      (cl-pushnew `(NotebookId . ,notebook-id) note-info)
      (cl-pushnew `(Title . ,note-title) note-info)
      (cl-pushnew '(Usn . 0) note-info))
    note-info))

;;;###autoload
(defun leanote-delete ()
  "delete current note."
  (interactive)
  (leanote-make-sure-login)
  (let* ((result-data nil)
         (note-info (leanote-get-note-info-base-note-full-name
                     (buffer-file-name)))
         (note-id (assoc-default 'NoteId note-info))
         (notebook-id (assoc-default 'NotebookId note-info))
         (note-title (assoc-default 'Title note-info))
         (usn (assoc-default 'Usn note-info)))
    (unless note-id
      (error "cannot found current note for id %s" note-id))
    (when (yes-or-no-p (format "Do you really want to delete %s?" note-title))
      (setq result-data (leanote-ajax-delete-note note-id usn))
      (if (and (listp result-data)
               (equal :json-false (assoc-default 'Ok result-data)))
          (error "delete note error, msg:%s." (assoc-default 'Msg result-data))
        (progn
          (unless result-data
            (leanote-log "error" "error in delete note. reason: server error!")
            (error "error in delete note. reason: server error!"))
          (leanote-log (format "delete remote note %s success." note-title))
          (remhash note-id leanote--cache-noteid-info)   ;; remove from local cache
          (let ((name (buffer-file-name))
                (notebook-notes-new (leanote-delete-local-notebook-note notebook-id note-id)))
            (when (listp recentf-list)      ;; remove it from recentf-list
              (delete name recentf-list))
            ;; (kill-buffer)
            (leanote-delete-file-and-buffer)
            (puthash notebook-id notebook-notes-new leanote--cache-notebookid-notes)
            (leanote-log (format "local file %s was deleted." name))
            ))
        ))))

(defun leanote-delete-file-and-buffer ()
  "delete current buffer and its file"
  (let ((filename (buffer-file-name)))
    (when filename
      (delete-file filename)
      (kill-buffer)
      (when (listp recentf-list)
        (delete filename recentf-list)))))

(defun leanote-delete-local-notebook-note (notebook-id noteid)
  "delete local cache notebook note"
  (let* ((notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
         (index (leanote-get-note-index notebook-notes noteid))
         (result notebook-notes))
    (when (>= index 0)
      (aset notebook-notes index nil)) ;; a new array
    (setq result (delete nil notebook-notes))
    result))

(defun leanote-ajax-delete-note (note-id usn)
  "delete note"
  (leanote-log (format "note-id=%s, usn=%d will be delete." note-id usn))
  (let* ((result nil)
         (usn-str (number-to-string (+ 0 usn))))
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
                                   (leanote-log "error" "Got error in leanote-ajax-delete-note")
                                   (error "Got error: %S" error-thrown)))
             )
    result))

(defun leanote-notebook-has-note-p (notebook-notes note-name)
  "notebook has note with name `note-name'"
  (let* ((result nil))
    (when (string-suffix-p ".md" note-name)
      (setq note-name (substring note-name 0
                                 (- (length note-name) 3))))
    (cl-loop for elt in (append notebook-notes nil)
             collect
             (let ((title (assoc-default 'Title elt)))
               (when (equal title note-name)
                 (setq result t))))
    result))

(defun leanote-get-note-index (notebook-notes note-id)
  "get the note index in `notebook-notes'"
  (let* ((index -1)
         (count 0))
    (cl-loop for elt in (append notebook-notes nil)
             collect
             (let ((note-id-in-notebook (assoc-default 'NoteId elt)))
               (when (equal note-id-in-notebook note-id)
                 (setq index count)
                 (leanote-log (format "matched: noteid=%s, index=%d" note-id index)))
               (setq count (+ count 1))))
    index))

(defun leanote-notebook-replace (notebook-notes new-note note-id)
  "replace `note-name' with `new-note'"
  (let* ((index (leanote-get-note-index notebook-notes note-id)))
    (when (and (>= index 0)
               (arrayp notebook-notes))
      (aset notebook-notes index new-note))))

(defun leanote-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (leanote-log "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (progn
            (leanote-log "A buffer named '%s' already exists! Use another name!" new-name)
            (rename-file name new-name 1)
            (rename-buffer (concat new-name "#" name))
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;###autoload
(defun leanote-rename ()
  "rename current note"
  (interactive)
  (leanote-make-sure-login)
  (let* ((buf-name (buffer-file-name))
         (note-info (leanote-get-note-info-base-note-full-name buf-name))
         (result-data nil)
         (notebook-id (gethash
                       (substring default-directory 0 (- (length default-directory) 1))
                       leanote--cache-notebook-path-id))
         (notebook-info (gethash notebook-id leanote--cache-notebookid-info))
         (notebook-title (assoc-default 'Title notebook-info))
         (notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
         (note-id (assoc-default 'NoteId note-info))
         (note-title (assoc-default 'Title note-info))
         (new-name nil))
    (when note-id
      (setq note-info (gethash note-id leanote--cache-noteid-info)) ;; force update
      (setq new-name (read-string "Input new name:" nil nil note-title))
      (when (string-suffix-p ".md" new-name)
        (setq new-name (substring new-name 0 (- (length new-name) 3))))
      (when (equal note-title new-name)
        (error "Rename error, not changed!"))
      (when (leanote-notebook-has-note-p notebook-notes new-name)
        (error (format "Rename error: the notebook %s already exists note %s" notebook-title new-name)))
      (when (yes-or-no-p (format "Change file name %s.md to %s.md?"
                                 note-title new-name))
        (leanote-log "rename note %s with new name %s" note-title new-name)
        (cl-pushnew `(Title . ,new-name) note-info)
        (setq result-data (leanote-ajax-update-note note-info nil))
        (if (and (listp result-data)
                 (equal :json-false (assoc-default 'Ok result-data)))
            (error "rename note error, msg:%s." (assoc-default 'Msg result-data))
          (progn
            (unless result-data
              (error "error in rename. reason: server error!"))
            (leanote-notebook-replace notebook-notes result-data note-id)
            (puthash notebook-id notebook-notes leanote--cache-notebookid-notes)
            (puthash note-id result-data leanote--cache-noteid-info)
            (leanote-rename-file-and-buffer (concat new-name ".md"))
            (when (listp recentf-list)      ;; remove it from recentf-list
              (delete buf-name recentf-list))
            (message "rename note success.")
            (leanote-log "rename note success.")))))
    ))

(defun leanote-get-current-note-id ()
  "get current buffer leanote note-id"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (let* ((is-markdown-file (string-suffix-p ".md" file-name))
             (note-title (file-name-base file-name)))
        (when is-markdown-file
          (let* ((notebook-id (gethash
                               (substring default-directory
                                          0 (- (length default-directory) 1))
                               leanote--cache-notebook-path-id))
                 (notebook-notes nil)
                 (note-id nil))
            (when notebook-id
              (setq notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
              (cl-loop for elt in (append notebook-notes nil)
                       collect
                       (when (equal note-title (assoc-default 'Title elt))
                         (setq note-id (assoc-default 'NoteId elt))))
              note-id)))))))

(defun leanote-extra-abstract (content)
  "get abstract from leanote `content'"
  (let* ((s-indx (string-match "#" content))
         (s-end (string-match "#" content (+ 1 s-indx)))
         (result nil))
    (cond
     ((and s-indx (not s-end))
      (setq result (substring content (+ 1 s-indx))))
     ((and s-indx s-end (> s-end s-indx))
      (let* ((pure-txt (s-trim (substring content (+ 1 s-indx) s-end)))
             (lf-indx (string-match "\n" pure-txt))
             (title (if lf-indx
                        (substring pure-txt 0 lf-indx)
                      pure-txt
                      ))
             (abstruct (if lf-indx
                           (substring pure-txt (+ 1 lf-indx))
                         pure-txt
                         )))
        (if (> (length abstruct) 0)
            (setq result abstruct)
          (set result title)))))
    (s-trim result)))

;;;###autoload
(defun leanote-pull ()
  "force update current note"  
  (interactive)
  (let* ((noteid (leanote-get-current-note-id))
         (noteinfo (gethash noteid leanote--cache-noteid-info))
         (is-modified nil)
         (query-msg "Do you want to replace local with remote content?"))
    (when (and noteid noteinfo)
      (leanote-make-sure-login)
      (setq is-modified (assoc-default 'IsModified noteinfo))
      (when is-modified
        (setq query-msg (concat "Local file is modified!" query-msg)))
      (when (yes-or-no-p query-msg)
        (let* ((notecontent-obj (leanote-get-note-and-content noteid))
               (notecontent (assoc-default 'Content notecontent-obj)))
          (when notecontent
            (erase-buffer)
            (insert notecontent)
            (save-buffer)  ;; save buffer must before
            (puthash noteid notecontent-obj leanote--cache-noteid-info)
            (puthash noteid `(,noteid :false ,(current-time))
                     leanote--cache-note-update-status)
            )))
      )))

(defun leanote-async-current-note-status (note-id callback)
  "async get note status"
  (interactive)
  (let* ((token leanote-token))
    (async-start
     `(lambda ()
        (set 'note-id ,note-id)
        ,(async-inject-variables "\\`note-id\\'")
        ,(async-inject-variables "\\`leanote-token\\'")
        ;;(require 'package)
        (package-initialize)
        (add-to-list 'load-path "~/github/leanote-mode")
        (require 'leanote)
        (let* (result)
          (setq result (leanote-get-note-and-content note-id))
          result))
     callback)))

(defun leanote-check-note-update-task ()
  "current note is need update. "
  (interactive)
  (let* ((note-id (leanote-get-current-note-id))
         (note-and-content nil)
         (remote-usn nil)
         (local-usn nil)
         (result nil)
         (status :false))
    ;; (leanote-log "execute leanote-current-note-need-update-status ...")
    (when (and note-id leanote-token)
      (let* ((cache-status (gethash note-id leanote--cache-note-update-status))
             (note-info (gethash note-id leanote--cache-noteid-info))
             (is-need-force-update t))
        (when cache-status
          (setq is-need-force-update (leanote-status-is-timeout cache-status))
          (unless is-need-force-update
            (setq result cache-status)
            (leanote-log (format "status not need update, last update: %s %s"
                                 (format-time-string "%Y-%m-%d %H:%M:%S"
                                                     (car (last cache-status)))
                                 note-id))))
        (when (or (not leanote-task-locker)
                  (leanote-status-is-timeout leanote-task-locker 30))
          (when (and note-info is-need-force-update)
            (setq leanote-task-locker `(,note-id :false ,(current-time)))
            (message "check note status for note:%s" note-id)
            (leanote-log (format "check note status for note:%s" note-id))
            (leanote-async-current-note-status
             note-id
             (lambda (asyncresult)
               (setq note-and-content asyncresult)
               (setq remote-usn (assoc-default 'Usn note-and-content))
               (setq local-usn (assoc-default 'Usn (gethash note-id leanote--cache-noteid-info)))
               (when (and remote-usn local-usn)
                 (when (> remote-usn local-usn)
                   (setq status t))
                 (if (eq t status)
                     (leanote-log (format "note need update, local-usn=%d, remote-usn=%d %s"
                                          local-usn remote-usn note-id))
                   (leanote-log (format "note not need update, local-usn=%d, remote-usn=%d %s"
                                        local-usn remote-usn note-id)))
                 (setq result `(,note-id ,status ,(current-time)))
                 (puthash note-id result leanote--cache-note-update-status)
                 (force-mode-line-update)
                 (leanote-log (format "finished check note status for note:%s" note-id)))
               )))
          )))
    result
    ))

(defun leanote-status-is-timeout (status &optional timeout)
  "check status is timeout"
  (when (null timeout)
    (setq timeout leanote-check-interval))
  (let ((result t)
        (last-time (car (last status)))
        (diff nil))
    (when (and status last-time)
      (setq diff (time-to-seconds (time-subtract (current-time) last-time)))
      (setq result (> diff timeout)))
    result))

(defun leanote-check-note-update ()
  "check current note is need update"
  ;; first force check, after execute task.
  (leanote-check-note-update-task)
  (unless leanote-idle-timer
    (leanote-log "leanote-idle-timer execute....")
    (setq leanote-idle-timer
          (run-with-idle-timer leanote-idle-interval t
                               'leanote-check-note-update-task))))

(defun leanote--login-status ()
  (if leanote-token
      "⦾"
    "✭"))

(defun leanote-status ()
  "current leanote status"
  (let* ((note-id (leanote-get-current-note-id))
         (result "")
         (note-info nil))
    (when note-id
      (setq note-info (gethash note-id leanote--cache-noteid-info))
      (when note-info
        (let ((is-modified (assoc-default 'IsModified note-info))
              (is-need-update (eq t (car (cdr (gethash note-id leanote--cache-note-update-status))))))
          (if is-modified
              (if is-need-update
                  (setq result (concat "leanote*⇡" (leanote--login-status)))
                (setq result (concat "leanote*" (leanote--login-status))))
            (if is-need-update
                (setq result (concat "leanote⇡" (leanote--login-status)))
              (setq result (concat "leanote" (leanote--login-status))))))))
    result))

;;;###autoload
(defun leanote-push ()
  "push current content or add new note to remote server."
  (interactive)
  (leanote-make-sure-login)
  (let* ((note-info (leanote-get-note-info-base-note-full-name
                     (buffer-file-name)))
         (result-data nil)
         (notebook-id (gethash
                       (substring default-directory 0 (- (length default-directory) 1))
                       leanote--cache-notebook-path-id))
         (notebook-info (gethash notebook-id leanote--cache-notebookid-info))
         (notebook-title (assoc-default 'Title notebook-info))
         (notebook-notes (gethash notebook-id leanote--cache-notebookid-notes))
         (note-id (assoc-default 'NoteId note-info))
         (note-title (assoc-default 'Title note-info)))
    (save-buffer)  ;; save it before update.
    (if note-id
        (progn     ;; modify exists note.
          (setq note-info (gethash note-id leanote--cache-noteid-info)) ;; force update
          (unless note-info
            (error "cannot find current note info for id %s in local cache." note-id))
          (setq result-data (leanote-ajax-update-note note-info (buffer-string)))
          (if (and (listp result-data)
                   (equal :json-false (assoc-default 'Ok result-data)))
              (error "push to remote error, msg:%s." (assoc-default 'Msg result-data))
            (progn
              (unless result-data
                (error "error in push(update note) to server. reason: server error!"))
              (leanote-log (format "file %s update to remote success." note-title))
              (message (format "file %s update to remote success." note-title))
              (leanote-notebook-replace notebook-notes result-data note-id)
              (puthash note-id result-data leanote--cache-noteid-info))
            ))
      (progn       ;; add new note
        (unless notebook-id
          (error "cannot find any notebook for this file."))
        (when (yes-or-no-p (format "The note was not found in notebook `%s'. Do you want to add it?"
                                   notebook-title))
          (cl-pushnew '(NoteId . "0") note-info)
          ;;(setq leanote-debug-data (gethash notebook-id leanote--cache-notebookid-notes))
          (setq result-data (leanote-ajax-update-note note-info (buffer-string) "/note/addNote"))
          (if (and (listp result-data)
                   (equal :json-false (assoc-default 'Ok result-data)))
              (error "add new note to remote error, msg:%s." (assoc-default 'Msg result-data))
            (progn
              (unless result-data
                (error "add new note to server error. reason: server error!"))
              (leanote-log (format "add new file %s to remote success." note-title))
              (message (format "add new file %s to remote success." note-title))
              (let* ((notebook-notes-new (vconcat notebook-notes (vector result-data))))
                (setq note-id (assoc-default 'NoteId result-data))
                (unless note-id
                  (error "error in local data operate!"))
                (puthash notebook-id notebook-notes-new leanote--cache-notebookid-notes)
                (puthash note-id result-data leanote--cache-noteid-info)))
            )))
      )))

(defun leanote-ajax-update-note (note-info &optional note-content api)
  "update note content and abstract."
  (when (null api)
    (setq api "/note/updateNote"))
  (leanote-log (format "leanote-ajax-update-note api=%s" api))
  (let* ((result nil)
         (usn (assoc-default 'Usn note-info))
         (new-usn (+ 1 usn))
         (new-usn-str (number-to-string usn))
         (note-id (assoc-default 'NoteId note-info))
         (notebook-id (assoc-default 'NotebookId note-info))
         (note-title (assoc-default 'Title note-info))
         (request-params nil)
         (note-abstract nil))
    (setq request-params `(("token" . ,leanote-token)
                           ("NoteId" . ,note-id)
                           ("Usn" . ,new-usn-str)
                           ("NotebookId" . ,notebook-id)
                           ("Title" . ,note-title)))
    (if note-content
        (progn
          (leanote-log "update content")
          (setq note-abstract (leanote-extra-abstract note-content))
          (cl-pushnew '("IsMarkdown" . "true") request-params)
          (cl-pushnew `("Abstract" . ,note-abstract) request-params)
          (cl-pushnew `("Content" . ,note-content) request-params))
      (leanote-log "only update info."))
    (request (concat leanote-api-root api)
             :params request-params
             :sync t
             :type "POST"
             :parser 'leanote-parser
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq result data)))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (message "Got error: %S" error-thrown)
                                   (leanote-log "error" "Got error")
                                   (error "Got error: %S" error-thrown)))
             )
    result))

(defun leanote-parser ()
  "parser"
  (json-read-from-string (decode-coding-string (buffer-string) 'utf-8)))

(defun leanote-ajax-get-note-books ()
  "get note books"
  (interactive)
  (leanote-log (format "leanote-ajax-get-note-books api: %s" leanote-api-getnotebooks))
  (let ((note-books (leanote-common-api-action leanote-api-getnotebooks)))
    (if note-books
        (progn (setq leanote-current-all-note-books note-books)
               (leanote-log (format "Got %d notebooks." (length note-books)))
               note-books)
      (progn
        (message "No notebooks got!")
        (leanote-log "warning" "No notebooks got!")
        (error "No notebooks got!")))
    ))

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
  (unless api
    (error "leanote-common-api-action parameter api is %s!" api))
  (leanote-log (format "do ajax, api=%s" api))
  (let ((result nil))
    (request (concat leanote-api-root api)
             :params `(("token" . ,leanote-token) (,param-key . ,param-value))
             :sync t
             :parser 'leanote-parser
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (if (arrayp data)
                             (progn
                               (setq result data))
                           (progn
                             (unless (eq (assoc-default 'Ok leanote-debug-data) :json-false)
                               (setq result data))
                             ))))
             :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                   (message "Got error: %S" error-thrown)
                                   (leanote-log "error" "Got error")
                                   (error "Got error: %S" error-thrown)))
             )
    result))

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
    (leanote-log (format "make root dir %s" leanote-local-root-path))
    (make-directory leanote-local-root-path t))
  (when (null all-notebooks)
    (leanote-log "warning" "all-notebooks not provided.")
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

(defun leanote-make-sure-login (&optional force)
  "make sure login first"
  (when (null force)
    (if leanote-token
        (setq force nil)
      (setq force t)))
  (when force
    (leanote-login))
  (unless leanote-token
    (leanote-log "error" "login failed!")
    (error "login failed!")))

;;;###autoload
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
                           (leanote-log "error" (format "%s" (assoc-default 'Msg data)))
                         (progn
                           (setq leanote-token (assoc-default 'Token data))
                           (setq leanote-user (assoc-default 'Username data))
                           (setq leanote-user-email (assoc-default 'Email data))
                           (setq leanote-user-id (assoc-default 'UserId data))
                           (setq leanote-user-password password)
                           (message "login success!")
                           (leanote-log "login success!")))))))

;;; find & search

(defun leanote-get-all-notes-from-cache ()
  "get all note-info from local cache"
  (let* ((result '()))
    (maphash (lambda (key value)
               (let* ((notebookid (assoc-default 'NotebookId value))
                      (notetitle (assoc-default 'Title value))
                      (noteid (assoc-default 'NoteId value))
                      (notebookpath (leanote-get-notebook-path-from-cache notebookid))
                      (fullpath nil))
                 (when (and notebookid
                            notebookpath)
                   (setq fullpath (expand-file-name (concat notetitle ".md") notebookpath))
                   (when (file-exists-p fullpath)
                     (add-to-list 'result
                                  (cons fullpath
                                        (list notebookpath notetitle notebookid noteid)))))))
             leanote--cache-noteid-info)
    result))

(defun leanote-get-notebook-path-from-cache (notebook-id)
  "obtain `notebook-id' notebook path based on local cache"
  (let (result)
    (maphash (lambda (key value)
               (when (string= value notebook-id)
                 (setq result key)))
             leanote--cache-notebook-path-id)
    result))

(defun leanote--open-note (x &optional helm)
  (let* ((file-name (car x)))
    (when helm
      (setq file-name (expand-file-name
                       (concat (car (cdr x)) ".md")
                       file-name)))
    (if (file-exists-p file-name)
        (find-file file-name)
      (message "note %s doesn't exists." file-name))))

;;;###autoload
(defun leanote-find ()
  "find note by title with ivy-mode"
  (interactive)
  (let (collection)
    (setq collection (leanote-get-all-notes-from-cache))
    (ivy-read "search note by title: "
              collection
              :action 'leanote--open-note
              )))

;;;###autoload
(defun leanote-helm-find ()
  "helm find note"
  (interactive)
  (let (collection)
    (setq collection (leanote-get-all-notes-from-cache))
    (helm :sources (helm-build-sync-source "test"
                     :candidates collection
                     :fuzzy-match t
                     :action (lambda (x)
                               (leanote--open-note x t)))
          :buffer "*helm test*"
          )))

;;; log

(defun leanote-log2msg (level &rest args)
  "only warning or error message to *Message* buffer."
  (when (or (equal "warning" level) (equal "error" level))
    (let* ((local-current-time (format-time-string "[leanote][%Y-%m-%d %H:%M:%S] " (current-time))))
      (message (concat local-current-time (string-join args " ")))
      )))

(defun leanote-log2buf (level &rest args)
  "log message in buffer `leanote-log-buffer-name'"
  (let* ((buf (get-buffer-create leanote-log-buffer-name))
         (local-current-time (format-time-string "[%Y-%m-%d %H:%M:%S] " (current-time))))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "[%s] " level))
      (insert (concat local-current-time (string-join args " ")))
      (insert "\n"))))

(defun leanote-log4j (level msg)
  "log4j: log message with corresponding level."
  (leanote-log2msg level msg)
  (cond ((equal "info" leanote-log-level)
         (progn
           (leanote-log2buf level msg)))
        ((equal "warning" leanote-log-level)
         (progn
           (when (or (equal "warning" level) (equal "error" level))
             (leanote-log2buf level msg))
           ))
        ((equal "error" leanote-log-level)
         (progn
           (when (or (equal "error" level))
             (leanote-log2buf level msg))
           ))))

(defun leanote-log (&rest args)
  "log message!"
  (let* ((size (length args))
         (level (if (= 1 size)
                    "info"
                  (car args)))
         (content (if (= 1 size)
                      args
                    (cdr args))))
    (leanote-log4j level (string-join content " "))))

(provide 'leanote)
;;; leanote.el ends here
