;;; tongbu.el --- Share text/file between your computer and phone  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/tongbu.el
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2"))
;; Keywords: tools
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides A web server for sharing text/files between two divices.

;;; Code:

(require 'web-server)

(defgroup tongbu nil
  "A web server for sharing text/files."
  :group 'tools)

(defcustom tongbu-port 8888
  "Port number used by the tongbu web server."
  :type 'integer)

(defcustom tongbu-host "0.0.0.0"
  "Host name used by the tongbu web server."
  :type 'string)

(defcustom tongbu-css "
textarea {
    font-size: 1rem;
    line-height: 1.5;
    padding: 10px;
    width: 100%;
}
"
  "The stylesheet."
  :type 'string)

(defcustom tongbu-js ""
  "The script.

This must be either empty or a script element, e.g.,

  (setq tongbu-js \"<script> alert('Hello World!'); </script>\")

The script element will be added at the end of the HTML."
  :type 'string)

;; IDEA lint (generated) HTML in CI, see tidy, xmllint or https://validator.w3.org/
(defvar tongbu-html
  "\
<!DOCTYPE html>
<html lang='en'>
  <head>
    <meta charset='utf-8'>
    <meta name='google' content='notranslate'>

    <meta name='viewport' content='width=device-width, initial-scale=1'>
    <title>Share text and files</title>
    <style>
      %s
    </style>
  </head>
  <body>

<h3>Share text</h3>

    <form method='post' enctype='multipart/form-data'>
      <textarea name='text'>%s</textarea>
      <input type='submit' value='Save'>
    </form>

    <br>

<h3>Upload file</h3>

    <form action='%s' method='post' enctype='multipart/form-data'>
      <input type='file' name='file' required>
      <input type='submit'>
    </form>

<h3>Directory listing for %s</h3>
<table>
  <thead>
    <tr>
      <th>Filename</th>
      <th>Size</th>
      <th>Date Modified</th>
    </tr>
  </thead>
  <tbody>
    %s
  </tbody>
</table>

%s
  </body>
</html>
"
  "HTML template.

There are 5 %s in this template, they are for

- `tongbu-css'
- `tongbu-text'
- directory name you are visiting
- directory name you are visiting
- directory listing
- `tongbu-js'.")

(defvar tongbu-docroot (expand-file-name "~/")
  "The web server document root.")

(defvar tongbu-text ""
  "The text for sharing.")

(defun tongbu-build-html (dir)
  "Build HTML for render.

The DIR is an absolute path. For example, if user is visiting

  http://localhost:8888/Pictures/Screenshots/

then the DIR is like \"/Users/xcy/Pictures/Screenshots/\"."
  (format tongbu-html
          tongbu-css
          tongbu-text
          (substring dir (1- (length tongbu-docroot)))
          (abbreviate-file-name dir)
          (tongbu-list-directory dir)
          tongbu-js))

(defun tongbu-directory-files (dir)
  (append
   (unless (string= dir tongbu-docroot)
     (list (cons ".." (file-attributes ".."))))
   (directory-files-and-attributes dir nil (rx bos (not (any "."))))))

(defun tongbu-list-directory (dir)
  (mapconcat
   (lambda (fn-and-attrs)
     (let* ((f (car fn-and-attrs))
            (attrs (cdr fn-and-attrs))
            (size (nth 7 attrs))
            (modtime (nth 5 attrs)))
       (let ((dirp (file-directory-p (expand-file-name f dir))))
         (format "<tr> <td>%s</td> <td>%s</td> <td>%s</td> </tr>"
                 (format "<a href='%s'>%s</a>"
                         (concat (url-hexify-string f) (and dirp "/"))
                         (concat f (and dirp "/")))
                 (if dirp
                     ""
                   (file-size-human-readable size))
                 (format-time-string "%Y/%m/%d %H:%M" modtime)))))
   (tongbu-directory-files dir)
   "\n"))

(defun tongbu-handle-index (request)
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/html"))
    (process-send-string process (tongbu-build-html tongbu-docroot))))

(defun tongbu-inhibit-download-p (path)
  "Return non-nil to not allow user to download PATH."
  (let ((size (nth 7 (file-attributes path)))
        (limit (* 1 1024 1024 1024)))
    (when (> size limit)
      (message "Can't download %s (%s) because it's large than %s"
               path
               (file-size-human-readable size)
               (file-size-human-readable limit)))))

(defun tongbu-handle-file (request)
  (with-slots (process headers) request
    (let* ((path (alist-get :GET headers))
           (path (tongbu-normalize-path path))
           (path (expand-file-name path tongbu-docroot)))
      (cond
       ((file-directory-p path)
        (ws-response-header process 200 (cons "Content-type" "text/html"))
        (process-send-string process (tongbu-build-html path)))
       ((file-regular-p path)
        (pcase (tongbu-inhibit-download-p path)
          ('nil (ws-send-file process path))
          (msg
           (ws-response-header process 500
                               '("Content-type" . "text/plain; charset=utf-8"))
           (process-send-string
            process (format "500 Internal Server Error\n\n%s\n" msg))
           (throw 'close-connection nil))))))))

(defun tongbu-handle-404 (request)
  (with-slots (process headers) request
    (ws-send-404 process
                 "404 Not Found\n\n%s"
                 (pp-to-string headers))))

(defun tongbu-normalize-path (uri)
  "Normalize Request-URI as path.

  (tongbu-normalize-path \"/Pictures/Screen%20Shot%202020-02-05%20at%2007.55.28.png\")
  ;; => \"Pictures/Screen Shot 2020-02-05 at 07.55.28.png\"
."
  (url-unhex-string (substring uri 1)))

(defun tongbu-file-request-p (request)
  "Return non-nil if REQUEST is GET /path/to/file.
The path/to/file (relative to `tongbu-docroot') has to be exists.
Otherwise, return nil."
  (with-slots (headers) request
    (let ((path (alist-get :GET headers)))
      (when path
        (setq path (tongbu-normalize-path path))
        ;; Don't allow http://localhost:8888///
        (unless (file-name-absolute-p path)
          (file-readable-p
           (expand-file-name path tongbu-docroot)))))))

(defun tongbu-save-text (request)
  (with-slots (process headers context) request
    (setq tongbu-text
          (pcase-exhaustive context
            ;; usual case
            ('multipart/form-data
             (alist-get 'content (assoc-default "text" headers)))
            ;; unusual case such as EWW does not respect enctype or user use
            ;; curl --data but forget to change content-type, for example,
            ;; curl localhost:8888 -d 'text=hello'
            ('application/x-www-form-urlencoded
             (assoc-default "text" headers))))
    (tongbu-redirect request)))

(defun tongbu-redirect (request)
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/html"))
    (process-send-string
     process
     (format "<meta http-equiv='refresh' content=\"0; url='%s'\">"
             (alist-get :POST headers)))))

(defun tongbu-upload-file-save-to (filename dir)
  "Return an absolute path for saving FILENAME in DIR.

The FILENAME is provided by user, it can be nil or empty (I am
not sure).  The DIR is where the file should be in.

This function is inspired Chrome's download rename behavior, that
is, let's say you try to download hello.txt, if hello.txt exists,
use hello (1).txt, if hello (1).txt also exists, use
hello (2).txt, and so on."
  (let* ((default-directory dir)
         (ext (file-name-extension filename))
         (base (if ext
                   (substring filename 0 (- (length filename) (length ext) 1))
                 filename)))
    (save-match-data
      (while (file-exists-p filename)
        (cond
         ;; hello (1).txt
         ((string-match (rx " (" (group (1+ num)) ")" eos) base)
          (setq base
                (replace-match
                 (number-to-string
                  (1+ (string-to-number (match-string 1 base))))
                 t nil base 1)))
         (t
          (setq base (concat base " (1)"))))
        (setq filename (pcase ext
                         ('nil base)
                         (_ (concat base "." ext))))))
    (expand-file-name filename dir)))

(defun tongbu-upload-file (request)
  (with-slots (process headers) request
    (let* ((path (tongbu-normalize-path (alist-get :POST headers)))
           (dir (expand-file-name path tongbu-docroot))
           (alist (assoc-default "file" headers))
           (c-filename (alist-get 'filename alist))
           (c-filename (if (and c-filename (not (string= "" c-filename)))
                           c-filename
                         (format-time-string "upload-%Y-%m-%d-%H:%M:%S")))
           (new-name (tongbu-upload-file-save-to c-filename dir))
           (c-content (alist-get 'content alist)))
      (let ((coding-system-for-write 'binary))
        (write-region c-content nil new-name))
      (message "[%s] saved %d bytes to %s"
               (current-time-string)
               (string-bytes c-content)
               new-name)
      (tongbu-redirect request))))

(defun tongbu-handle-post (request)
  (with-slots (headers) request
    (cond
     ((assoc-default "text" headers) (tongbu-save-text request))
     ((assoc-default "file" headers) (tongbu-upload-file request))
     (t (tongbu-handle-404 request)))))

(defvar tongbu-log-buffer nil
  "The server log buffer.

nil means does not log, otherwise it should be a buffer or buffer
name, if the buffer does not exist, it will be created
automatically.

It is nil by default since the log is not very interesting,
unlike nginx or apache's log, e.g.,

2020.03.02.23.07.42.450815000	127.0.0.1	59408	accept from 127.0.0.1

but it's better than nothing, hence the variable.")

;;;###autoload
(defun tongbu ()
  "Start the web server for sharing text/files."
  (interactive)
  (ws-start
   '(((:GET  . "^/$")       . tongbu-handle-index)
     ((:POST . ".*")        . tongbu-handle-post)
     (tongbu-file-request-p . tongbu-handle-file)
     ((lambda (_) t)             . tongbu-handle-404))
   tongbu-port
   tongbu-log-buffer
   :host tongbu-host)
  (message "http://%s:%d" tongbu-host tongbu-port))

(provide 'tongbu)
;;; tongbu.el ends here
