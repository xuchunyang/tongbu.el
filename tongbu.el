;;; tongbu.el --- A web server to share text or files between two devices  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/tongbu.el
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2"))
;; Package-Version: 20200414.507
;; Package-Commit: 6f6e5c5446f0c5735357ab520b249ab97295653e
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

;; This package provides A web server for sharing text/files between two devices.

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
* {
    box-sizing: border-box;
}

body {
    max-width: 760px;
    margin: 0 auto;
}

.container {
    margin: 8px;
}

textarea {
    font-size: 1rem;
    line-height: 1.5;
    padding: 10px;
    width: 100%;
}

table {
    width: 100%;
}

thead th {
    text-align: left;
}
.login {
    width:300px;
    height:35px;
}
.login-submit {
    width:300px;
    height:50px;
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
    <div class='container'>
      <h3>Share text</h3>

      <form method='post' enctype='multipart/form-data'>
        <textarea name='text'>%s</textarea>
        <input type='submit' value='Save'>
      </form>

      <br>

      <h3>Upload file</h3>

      <form action='%s' method='post' enctype='multipart/form-data'>
        <input type='file' name='file' multiple='multiple' required>
        <input type='submit' value='Upload'>
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
    </div>
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

(defvar tongbu-ws-server nil
  "The web-server object of tongbu.")

(defun tongbu-build-html (dir)
  "Build HTML for render.

The DIR is an absolute path.  For example, if user is visiting

  http://localhost:8888/Pictures/Screenshots/

then the DIR is like \"/Users/xcy/Pictures/Screenshots/\"."
  (format tongbu-html
          tongbu-css
          tongbu-text
          (substring dir (1- (length tongbu-docroot)))
          (abbreviate-file-name dir)
          (tongbu-list-directory dir)
          tongbu-js))

(defvar tongbu-file-regexp (rx bos (not (any ".")))
  "Only files matching this regexp will be listed.")

(defun tongbu-directory-files-and-attributes (dir)
  "Return files matching `tongbu-file-regexp' in DIR."
  (append
   (unless (string= dir tongbu-docroot)
     (list (cons ".." (file-attributes ".."))))
   (directory-files-and-attributes dir nil tongbu-file-regexp)))

(defun tongbu-list-directory (dir)
  "Return html table data listing files in DIR."
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
   (tongbu-directory-files-and-attributes dir)
   "\n"))

(defun tongbu-handle-index (request)
  "Handle REQUEST from GET /."
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/html"))
    (process-send-string process (tongbu-build-html tongbu-docroot))))

(defun tongbu-inhibit-download-p (path)
  "Return non-nil to not allow user to download PATH."
  (let ((size (nth 7 (file-attributes path)))
        (limit (* 1 1024 1024 1024)))
    (when (> size limit)
      (message "Can't download %s (%s) because it's larger than %s"
               path
               (file-size-human-readable size)
               (file-size-human-readable limit)))))

(defun tongbu-handle-file (request)
  "Handle REQUEST of listing directory or downloading file."
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
  "Response 404 \"No Found\" to REQUEST."
  (with-slots (process headers) request
    (ws-send-404 process
                 "404 Not Found\n\n%s"
                 (pp-to-string headers))))

(defun tongbu-normalize-path (uri)
  "Normalize request URI as file path."
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
  "Handle REQUEST, save text to `tongbu-text'."
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
  "Send a redirect HTML to a post REQUEST."
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

This function is inspired by Chrome's download rename behavior, that
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
  "Handle uploading file REQUEST."
  (with-slots (process headers) request
    (let* ((path (tongbu-normalize-path (alist-get :POST headers)))
           (dir (expand-file-name path tongbu-docroot)))
      (dolist (header headers)
        (when (equal (car header) "file")
          (let* ((alist (cdr header))
                 (c-filename (alist-get 'filename alist))
                 (c-filename (if (and c-filename (not (string= "" c-filename)))
                                 c-filename
                               (format-time-string "upload-%Y-%m-%d-%H:%M:%S")))
                 (new-name (decode-coding-string
                            (tongbu-upload-file-save-to c-filename dir)
                            (or buffer-file-coding-system 'prefer-utf-8)))
                 (c-content (alist-get 'content alist)))
            (let ((coding-system-for-write 'binary))
              (write-region c-content nil new-name))
            (message "Tongbu: [%s] %dKB -> \"%s\""
                     (format-time-string "%Y-%m-%d %H:%M:%S")
                     (/ (string-bytes c-content) 1024)
                     new-name))
          (tongbu-redirect request))))))

(defun tongbu-handle-post (request)
  "Handle all POST REQUEST."
  (with-slots (headers) request
    (cond
     ((assoc-default "text" headers) (tongbu-save-text request))
     ((assoc-default "file" headers) (tongbu-upload-file request))
     (t (tongbu-handle-404 request)))))

(defvar tongbu-low-level-log-buffer nil
  "The low level server log buffer.

nil means does not log, otherwise it should be a buffer or buffer
name, if the buffer does not exist, it will be created
automatically.

It is nil by default since the log is not very interesting,
unlike nginx or apache's log, e.g.,

2020.03.02.23.07.42.450815000	127.0.0.1	59408	accept from 127.0.0.1

but it's better than nothing, hence the variable.")

(defvar tongbu-high-level-log-time-format "%Y-%m-%d %H:%M:%S"
  "Logging time format passed to `format-time-string'.")

(defvar tongbu-high-level-log-buffer nil
  "The high level server log buffer.")

;; XXX report the response as well (success or fail)
(defun tongbu-high-level-log (req)
  "Log request REQ."
  (when tongbu-high-level-log-buffer
    (with-current-buffer (get-buffer-create tongbu-high-level-log-buffer)
      (goto-char (point-max))
      (insert (format "%s %s\n"
                      (format-time-string tongbu-high-level-log-time-format)
                      (oref req headers)))))
  ;; must return nil
  nil)

(defvar tongbu-activate-login nil
  "The tongbu activate login function .

nil means does not activate login function, otherwise the login function should be activated.")

(defvar tongbu-login-username "username"
  "The web server login username.")

(defvar tongbu-login-passwrod "password"
  "The web server login password.")

(defvar tongbu-html--login
  "\
<!DOCTYPE html>
<html lang='en'>
  <head>
    <meta charset='utf-8'>
    <meta name='google' content='notranslate'>
    <meta name='viewport' content='width=device-width, initial-scale=1'>
    <title>login</title>
    <style>
%s
    </style>
  </head>
  <body>
    <div class='container'>
      <br>

      <h3>Login</h3>
      
      <form action='%s' method='post'>
        <input class='login' type='text' name='username' placeholder='username' required>
        <br>
        <br>
        <input class='login' type='password' name='password' placeholder='password' required>
        <br>
        <br>
        <input class='login-submit' type='submit' value='Login'>
      </form>
    </div>
  </body>
</html>
"
  "HTML login template.

There are 1 %s in this template, they are for

- `tongbu-css'.")

(defun tongbu-build-login-html ()
  "Build HTML for login.."
  (format tongbu-html--login
          tongbu-css
          "login.do"))



(defun tongbu-login-request-p (request)
  "Return non-nil if logined.
Otherwise, return nil."
  (when tongbu-activate-login
    (with-slots (headers) request
    (let ((cookie (alist-get :COOKIE headers)))
      (cond
       (cookie (let* ((realsession (concat "session=" (sha1 (concat tongbu-login-username tongbu-login-passwrod (format-time-string "%Y-%m-%d"))))))
                 (if  (string-equal realsession cookie)
                     (if (and (assoc :GET  (ws-headers request))
                              (string-equal "/login.do"
                                            (cdr (assoc :GET
                                                        (ws-headers request)))))
                         (with-slots (process headers) request
                           (ws-response-header process 302 '("Location" . "/")))
                       nil
                         )
                   t)))
       (t t))))
    ))

(defun tongbu-handle-login-request (request)
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/html") '("Set-Cookie" . ""))
    (process-send-string process (tongbu-build-login-html)))
  )
(defun tongbu-handle-login (request)
  "Handle REQUEST of login."
  (cond
   ((and (assoc :GET (ws-headers request))
         (string-equal "/login.do"
                       (cdr (assoc :GET
                                   (ws-headers request)))))
    (tongbu-handle-login-request request))
   ((and (assoc :POST  (ws-headers request))
        (string-equal "/login.do"
                       (cdr (assoc :POST
                                   (ws-headers request)))))
    (with-slots (process headers) request
      (let* ((username (assoc-default "username" headers))
             (password (assoc-default "password" headers))
             (logintime (format-time-string "%Y-%m-%d"))
             (session (cons "Set-Cookie" (concat "session=" (sha1 (concat username password logintime))))))
        (if (and (string-equal username tongbu-login-username) (string-equal password tongbu-login-passwrod))
            (progn
              (message "login %s in %s" username (format-time-string "%Y-%m-%d %H:%M:%S"))
              (ws-response-header process 200 '("Content-type" . "text/html") session)
              (process-send-string process (tongbu-build-html tongbu-docroot))
              )
         (tongbu-handle-login-request request))
        )))
   (t (with-slots (process headers) request
                           (ws-response-header process 302 '("Location" . "/login.do"))))))

;;;###autoload
(defun tongbu ()
  "Start the web server for sharing text/files."
  (interactive)
  (if tongbu-ws-server
      (message "Tongbu server is running, which url is \"http://%s:%d\"."
               tongbu-host tongbu-port)
    (setq tongbu-ws-server
          (ws-start
           (list
            (cons #'tongbu-high-level-log  #'ignore)
            (cons #'tongbu-login-request-p  #'tongbu-handle-login)
            (cons '(:GET  . "^/$")         #'tongbu-handle-index)
            (cons '(:POST . ".*")          #'tongbu-handle-post)
            (cons #'tongbu-file-request-p  #'tongbu-handle-file)
            (cons (lambda (_) t)                #'tongbu-handle-404))
           tongbu-port
           tongbu-low-level-log-buffer
           :host tongbu-host))
    (message "http://%s:%d" tongbu-host tongbu-port)))




;;;###autoload
(defun tongbu-stop ()
  "Stop tongbu server."
  (interactive)
  (when (ws-server-p tongbu-ws-server)
    (ws-stop tongbu-ws-server)
    (setq tongbu-ws-server nil)
    (message "Tongbu server has been stop.")))

(provide 'tongbu)
;;; tongbu.el ends here
