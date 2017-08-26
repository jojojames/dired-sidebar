;;;; dired-sidebar.el -- Tree browser leveraging dired -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2099  Free Software Foundation, Inc.

;; Author: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/dired-sidebar
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: dired

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a tree browser similar to `neotree' or `treemacs'
;; but leverages `dired' to do the job of display.

;;
;; (use-package dired-subtree
;;   :ensure t
;;   :commands (dired-subtree-toggle dired-subtree-cycle)
;;   :config
;;   (setq dired-subtree-use-backgrounds nil))
;;
;; (use-package dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar/toggle-sidebar))
;;   :ensure nil
;;   :commands (dired-sidebar/toggle-sidebar)
;;   :config
;;   (use-package all-the-icons-dired
;;     ;; M-x all-the-icons-install-fonts
;;     :ensure t
;;     :commands (all-the-icons-dired-mode)))
;;

;;; Code:
(require 'dired)
(require 'dired-subtree)
(require 'evil nil t)
(require 'face-remap)
(require 'projectile nil t)
(eval-when-compile (require 'subr-x))

;; Customizations

(defgroup dired-sidebar nil
  "A major mode leveraging `dired-mode' to display a filesystem in a tree
layout."
  :group 'files)

(defcustom dired-sidebar/use-custom-font t
  "Show `dired-sidebar' with font face using `dired-sidebar/font-face'."
  :type 'boolean
  :group 'dired-sidebar)

(defvar dired-sidebar/font-face '(:family "Helvetica" :height 130)
  "Face used by `dired-sidebar' for font if `dired-sidebar/use-custom-font'
is true.")

(defcustom dired-sidebar/use-custom-modeline t
  "Show `dired-sidebar' with custom modeline using
`dired-sidebar/mode-line-format'."
  :type 'boolean
  :group 'dired-sidebar)

(defvar dired-sidebar/mode-line-format
  '("%e" mode-line-front-space
    mode-line-buffer-identification
    "  " (vc-mode vc-mode) "  "  mode-line-end-spaces)
  "Mode line format for `dired-sidebar'.")

(defcustom dired-sidebar/use-all-the-icons t
  "Use `all-the-icons' if true."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar/width 35
  "Width of the `dired-sidebar' buffer."
  :type 'integer
  :group 'dired-sidebar)

(defcustom dired-sidebar/refresh-on-projectile-switch t
  "Refresh sidebar when `projectile' changes projects."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar/pop-to-sidebar-on-toggle-open t
  "Also jump to sidebar when toggling sidebar open with
`dired-sidebar/toggle-sidebar'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar/use-evil-integration t
  "Set up some keybindings with `evil-mode' if true.
This needs to be set before `dired-sidebar-mode' is called for the first time."
  :type 'boolean
  :group 'dired-sidebar)

;; Internal
(defvar dired-sidebar/alist '()
  "An alist that maps from frame to currently opened `dired-sidebar' buffer.")

;; Mode

(defmacro dired-sidebar/with-no-dedication (&rest body)
  "Run BODY after undedicating window."
  `(progn
     (set-window-dedicated-p (get-buffer-window (current-buffer)) nil)
     ,@body
     (set-window-dedicated-p (get-buffer-window (current-buffer)) t)))

(defvar dired-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'dired-subtree-toggle)
    (define-key map (kbd "C-m") 'dired-sidebar/find-file)
    (define-key map (kbd "RET") 'dired-sidebar/find-file)
    (define-key map (kbd "<return>") 'dired-sidebar/find-file)
    (define-key map "^" 'dired-sidebar/up-directory)
    (define-key map (kbd "C-o") 'dired-sidebar/find-file-ace)
    map)
  "Keymap used for `dired-sidebar-mode'.")

(define-minor-mode dired-sidebar-mode
  "A minor mode that leverages `dired' to emulate a Tree browser."
  :init-value nil
  :lighter ""
  :keymap dired-sidebar-mode-map

  (setq window-size-fixed 'width)

  (auto-revert-mode -1)

  ;; We don't want extra details in the sidebar.
  (dired-hide-details-mode)

  (when dired-sidebar/use-evil-integration
    (with-eval-after-load 'evil
      (evil-define-minor-mode-key 'normal 'dired-sidebar-mode
        [tab] 'dired-subtree-toggle
        (kbd "C-m") 'dired-sidebar/find-file
        (kbd "RET") 'dired-sidebar/find-file
        (kbd "<return>") 'dired-sidebar/find-file
        "^" 'dired-sidebar/up-directory
        (kbd "C-o") 'dired-sidebar/find-file-ace)))

  (when (and
         dired-sidebar/use-all-the-icons
         (display-graphic-p)
         (fboundp 'all-the-icons-dired-mode))
    (all-the-icons-dired-mode))

  (when dired-sidebar/use-custom-font
    (dired-sidebar/set-font))

  (when dired-sidebar/use-custom-modeline
    (dired-sidebar/set-mode-line))

  (when dired-sidebar/refresh-on-projectile-switch
    (add-hook
     'projectile-after-switch-project-hook
     (lambda ()
       (dired-sidebar/switch-to-dir (projectile-project-root)))))

  (dired-unadvertise (dired-current-directory))
  (dired-sidebar/update-buffer-name))

;; User Interface

;;;###autoload
(defun dired-sidebar/toggle-sidebar ()
  "Toggle the project explorer window."
  (interactive)
  (if (dired-sidebar/showing-sidebar-in-frame-p)
      (dired-sidebar/hide-sidebar)
    (dired-sidebar/show-sidebar)
    (when dired-sidebar/pop-to-sidebar-on-toggle-open
      (pop-to-buffer (dired-sidebar/sidebar-buffer-in-frame)))))

;;;###autoload
(defun dired-sidebar/show-sidebar (&optional b)
  "Project dired buffer on the side of the frame.
Shows the projectile root folder using dired on the left side of
the frame and makes it a dedicated window for that buffer."
  (interactive)
  (let ((buffer (or b (dired-sidebar/get-or-create-buffer))))
    (display-buffer-in-side-window buffer '((side . left)))
    (let ((window (get-buffer-window buffer)))
      (set-window-dedicated-p window t)
      (with-selected-window window
        (let ((window-size-fixed))
          (dired-sidebar/set-width dired-sidebar/width))))
    (with-current-buffer buffer
      (dired-sidebar-mode))
    (dired-sidebar/update-state-in-frame buffer)))

;;;###autoload
(defun dired-sidebar/hide-sidebar ()
  "Hide the sidebar window."
  (interactive)
  (let ((buffer (dired-sidebar/sidebar-buffer-in-frame)))
    (delete-window (get-buffer-window buffer))
    (dired-sidebar/update-state-in-frame nil)))

(defun dired-sidebar/find-file (&optional ace)
  "Wrapper over `dired-find-file'."
  (interactive)
  (let ((find-file-run-dired t)
        (dired-file-name (dired-get-file-for-visit)))
    (if (file-directory-p dired-file-name)
        (dired-sidebar/with-no-dedication
         (let ((buf-name (dired-sidebar/sidebar-buffer-name
                          dired-file-name)))
           (if (dired-sidebar/buffer-exists-p buf-name)
               (progn
                 (switch-to-buffer buf-name)
                 (dired-sidebar/update-state-in-frame (current-buffer)))
             ;; Copied from `dired-find-file'.
             (find-file dired-file-name)
             (dired-sidebar-mode)
             (dired-sidebar/update-state-in-frame (current-buffer)))))
      (select-window (if (and ace
                              (fboundp 'aw-select))
                         (aw-select "Select buffer")
                       (next-window)))
      (find-file dired-file-name))))

(defun dired-sidebar/find-file-ace ()
  "Wrapper over `dired-find-file' but open file using `ace-window'
if file was a file and not a directory."
  (interactive)
  (dired-sidebar/find-file :ace))

(defun dired-sidebar/up-directory ()
  "Wrapper over `dired-up-directory'."
  (interactive)
  (dired-sidebar/with-no-dedication
   (let* ((dir (dired-current-directory))
          (up (file-name-directory (directory-file-name dir)))
          (up-name (dired-sidebar/sidebar-buffer-name up)))
     (if (dired-sidebar/buffer-exists-p up-name)
         (progn
           (switch-to-buffer up-name)
           (dired-sidebar/update-state-in-frame (current-buffer)))
       (dired-up-directory)
       (dired-sidebar-mode)
       (dired-sidebar/update-state-in-frame (current-buffer))))))

;; Helpers

(defun dired-sidebar/buffer-exists-p (buffer-name)
  "Check if a `dired-sidebar' buffer exists for BUFFER-NAME."
  (get-buffer buffer-name))

(defun dired-sidebar/sidebar-root ()
  "Return directory."
  (condition-case nil
      (projectile-project-root)
    (error default-directory)))

(defun dired-sidebar/sidebar-buffer-name (root)
  "Return name of buffer given ROOT."
  (let ((b (concat ":" (abbreviate-file-name root))))
    (cond
     ((string-suffix-p ".." b)
      ;; ~/.emacs.d/elpa/.. -> ~/.emacs.d/
      (file-name-directory (substring b 0 (- (length b) 3))))
     ((not (string-suffix-p "/" b))
      (concat b "/"))
     (:default
      b))))

(defun dired-sidebar/get-or-create-buffer (&optional dir)
  "Return an existing `dired-sidebar' buffer or create a new one
and return that."
  (interactive)
  (let* ((root (or dir (dired-sidebar/sidebar-root)))
         (name (dired-sidebar/sidebar-buffer-name root))
         (buffer (or (get-buffer name)
                     (dired-noselect root))))
    ;; Rename the buffer generated by `dired-noselect'.
    (when (not (string-equal (buffer-name buffer) name))
      (with-current-buffer buffer
        (rename-buffer name)))
    buffer))

(defun dired-sidebar/set-font ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face dired-sidebar/font-face)
  (buffer-face-mode))

(defun dired-sidebar/set-mode-line ()
  "Set up modeline."
  (setq mode-line-format dired-sidebar/mode-line-format))

(defun dired-sidebar/set-width (width)
  "Set the width of the buffer to WIDTH when it is created.

Copied from `treemacs--set-width'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun dired-sidebar/update-buffer-name ()
  "Change buffer name to avoid collision with regular `dired' buffers."
  (rename-buffer
   (dired-sidebar/sidebar-buffer-name (dired-current-directory))))

(defun dired-sidebar/update-state-in-frame (buffer &optional f)
  "Update current state of sidebar in F or selected frame."
  (let ((frame (or f (selected-frame))))
    (if (assq frame dired-sidebar/alist)
        (setcdr (assq frame dired-sidebar/alist) buffer)
      (push `(,frame . ,buffer) dired-sidebar/alist))))

(defun dired-sidebar/showing-sidebar-in-frame-p (&optional f)
  "Check if F or selected frame contains a sidebar and return
corresponding buffer if buffer has a window attached to it.

Return buffer if so."
  (if-let (buffer (alist-get (or f (selected-frame)) dired-sidebar/alist))
      (if (get-buffer-window buffer)
          buffer
        nil)
    nil))

(defun dired-sidebar/sidebar-buffer-in-frame (&optional f)
  "Return the current sidebar buffer in f or selected frame."
  (let ((frame (or f (selected-frame))))
    (alist-get frame dired-sidebar/alist)))

(defun dired-sidebar/switch-to-dir (dir)
  "Update buffer with DIR as root."
  (when (dired-sidebar/showing-sidebar-in-frame-p)
    (let ((buffer (dired-sidebar/get-or-create-buffer dir)))
      (dired-sidebar/show-sidebar buffer))))

(provide 'dired-sidebar)
;;; dired-sidebar.el ends here
