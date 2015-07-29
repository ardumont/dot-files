;;; init.el --- work configuration dependent stuff   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine Romain Dumont

;; Author: Antoine Romain Dumont <antoine.romain.dumont@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'files)
(require 'dash-functional)
(require 'f)

(defun create-paths-to-load (rootdir)
  "Create the paths from the ROOTDIR directory."
  (--> rootdir
       (directory-file-name it)
       expand-file-name
       (directory-files it 'with-fullname "^\\([^.]\\|.emacs.d|\\.\\([^.]\\|\\..\\)\\).*")
       (-filter 'f-directory? it)
       (mapcar (-compose (-partial 's-join "/") (-rpartial 'list ".emacs.d")) it)))

;; load the init.el file present in each ~/work/<dir>/.emacs.d/init.el file
(mapc (lambda (path)
        (let ((init-file (f-join path "init.el")))
          (when (f-exists? init-file)
            (add-to-list 'load-path path)
            (message init-file)
            (load-file init-file))))
      (create-paths-to-load "~/work"))

(provide 'init)
;;; init.el ends here
