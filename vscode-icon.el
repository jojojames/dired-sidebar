;;; vscode-icon.el --- Utility package to provide Vscode style icons -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/vscode-icon
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: dired, files, tools
;; HomePage: https://github.com/jojojames/vscode-icon

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
;; This package provides a utility function that returns vscode style icons.
;; The entry point is `vscode-icon-for-file'.

;;; Code:
(defcustom vscode-icon-scale .18
  "The scale of icons."
  :type 'number
  :group 'vscode-icon)

(defvar vscode-icon-root (file-name-directory load-file-name)
  "Store the directory dired-sidebar.el was loaded from.")

(defvar vscode-icon-dir (format "%sicons/" vscode-icon-root)
  "Store the icons directory of `vscode-icon'.")

(defvar vscode-icon-dir-alist
  '(("scripts" . "script")
    ("build" . "binary")
    ("node_modules" . "npm")
    ("tests" . "test")
    ("out" . "binary")))

(defvar vscode-icon-file-alist
  '(;; Files.
    (".clang-format" . "cpp")
    (".projectile" . "emacs")
    ("projectile.cache" . "emacs")
    ("gradle.properties" . "gradle")
    ("gradlew" . "gradle")
    ("gradlew.bat" . "gradle")
    (".gitignore" . "git")
    (".gitattributes". "git")
    ("yarn.lock" . "yarn")
    ("Info.plist" . "objectivec")
    ("Cask" . "emacs")
    (".luminus" . "clojure")
    ("Dockerfile" . "docker")
    ("mix.lock" . "elixir")
    ("recentf" . "emacs")
    (".flowconfig" . "flow")
    (".editorconfig" . "editorconfig")
    (".babelrc" . "babel")
    ("cargo.lock" . "cargo")
    (".tramp" . "emacs")
    ;; Can lowercase if needed in the future.
    ("LICENSE" . "license")
    ("Makefile" . "makefile")
    ;; Extensions.
    ("restclient" . "rest")
    ("txt" . "text")
    ("ts" . "typescript")
    ("exs" . "elixir")
    ("cljc" . "clojure")
    ("clj" . "clojure")
    ("cljs" . "clojure")
    ("py" . "python")
    ("sh" . "shell")
    ("md" . "markdown")
    ("yml" . "yaml")
    ("hpp" . "cpp2")
    ("cc" . "cpp")
    ("m" . "objectivec")
    ("png" . "image")
    ("h" . "cppheader")
    ("elc" . "emacs")
    ("el" . "emacs")))

(defun vscode-icon-for-file (file)
  "Return an vscode icon image given FILE.

Icon Source: https://github.com/vscode-icons/vscode-icons"
  (let ((default-directory vscode-icon-dir))
    (if (file-directory-p file)
        (vscode-icon-dir file)
      (vscode-icon-file file))))

(defun vscode-icon-dir (file)
  "Get directory icon given FILE."
  (if (vscode-icon-dir-exists-p (file-name-base file))
      (vscode-icon-get-dir-image (file-name-base file))
    (if-let ((val (cdr (assoc
                        (file-name-base file) vscode-icon-dir-alist))))
        (if (vscode-icon-dir-exists-p val)
            (vscode-icon-get-dir-image val)
          (vscode-icon-default-folder))
      (vscode-icon-default-folder))))

(defun vscode-icon-file (file)
  "Get file icon given FILE."
  (if (vscode-icon-file-exists-p (file-name-extension file))
      (vscode-icon-get-file-image (file-name-extension file))
    (if-let ((val (or
                   (cdr (assoc (vscode-icon-basefile-with-extension file)
                               vscode-icon-file-alist))
                   (cdr (assoc file vscode-icon-file-alist))
                   (cdr (assoc (file-name-extension file)
                               vscode-icon-file-alist)))))
        (if
            (vscode-icon-file-exists-p val)
            (vscode-icon-get-file-image val)
          (vscode-icon-default-file))
      (vscode-icon-default-file))))

(defun vscode-icon-get-dir-image (key)
  "Return icon for KEY."
  (vscode-icon-create-image
   (expand-file-name (format "folder_type_%s.png" key))))

(defun vscode-icon-get-file-image (key)
  "Return icon for KEY."
  (vscode-icon-create-image
   (expand-file-name (format "file_type_%s.png" key))))

(defun vscode-icon-file-exists-p (key)
  "Check if there is an icon for KEY."
  (file-exists-p (expand-file-name (format "file_type_%s.png" key))))

(defun vscode-icon-dir-exists-p (key)
  "Check if there is an icon for KEY."
  (file-exists-p (expand-file-name (format "folder_type_%s.png" key))))

(defun vscode-icon-create-image (filename)
  "Helper method to create and return an image given FILENAME."
  (let ((scale vscode-icon-scale))
    (create-image filename 'png nil :scale scale :ascent 'center)))

(defun vscode-icon-default-folder ()
  "Return image for default folder."
  (vscode-icon-create-image (expand-file-name "default_folder.png")))

(defun vscode-icon-default-file ()
  "Return image for default file."
  (vscode-icon-create-image (expand-file-name "default_file.png")))

(defun vscode-icon-basefile-with-extension (file)
  "Return base filename with extension given FILE.

ex. ~/a/b.json -> b.json

If there is no extension, just return the base file name."
  (let ((base (file-name-base file))
        (ext (file-name-extension file)))
    (if (and base ext)
        (format "%s.%s" base ext)
      base)))

(provide 'vscode-icon)
;;; vscode-icon.el ends here
