;;; tongbu-tests.el --- Tests                        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang

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

;; tongbu.el tests.

;;; Code:

(require 'tongbu)
(require 'ert)

(ert-deftest tongbu-upload-file-save-to ()
  (should (string= (file-name-nondirectory
                    (tongbu-upload-file-save-to "README.md" default-directory))
                   "README (1).md"))
  (should (string= (file-name-nondirectory
                    (tongbu-upload-file-save-to "non-exist" default-directory))
                   "non-exist")))

(provide 'tongbu-tests)
;;; tongbu-tests.el ends here
