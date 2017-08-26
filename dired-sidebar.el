;;; dired-sidebar.el --- Tree browser leveraging dired -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/dired-sidebar
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: dired, files, tools
;; HomePage: https://github.com/jojojames/dired-sidebar

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
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure nil
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :config
;;   (use-package all-the-icons-dired
;;     ;; M-x all-the-icons-install-fonts
;;     :ensure t
;;     :commands (all-the-icons-dired-mode)))
;;

;;; Code:

(require 'dired)
(require 'evil nil t)
(require 'face-remap)
(eval-when-compile (require 'subr-x))

;; Customizations

(defgroup dired-sidebar nil
  "A major mode leveraging `dired-mode' to display a filesystem in a tree
layout."
  :group 'files)

(defcustom dired-sidebar-use-custom-font t
  "Show `dired-sidebar' with custom font.

This face can be customized using `dired-sidebar-font-face'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-font-face
  (cond
   ((eq system-type 'darwin)
    '(:family "Helvetica" :height 140))
   ((eq system-type 'windows-nt)
    '(:family "Times New Roman" :height 150))
   (:default
    '(:family "Arial" :height 150)))
  "Face used by `dired-sidebar' for custom font.

This only takes effect if `dired-sidebar-use-custom-font' is true."
  :type 'list
  :group 'dired-sidebar)

(defcustom dired-sidebar-use-custom-modeline t
  "Show `dired-sidebar' with custom modeline.

This uses format specified by `dired-sidebar-mode-line-format'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-mode-line-format
  '("%e" mode-line-front-space
    mode-line-buffer-identification
    "  " (vc-mode vc-mode) "  "  mode-line-end-spaces)
  "Mode line format for `dired-sidebar'."
  :type 'list
  :group 'dired-sidebar)

(defcustom dired-sidebar-use-all-the-icons t
  "Use `all-the-icons' if true.

This has no effect in Terminals."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-width 35
  "Width of the `dired-sidebar' buffer."
  :type 'integer
  :group 'dired-sidebar)

(defcustom dired-sidebar-refresh-on-projectile-switch t
  "Refresh sidebar when `projectile' changes projects."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-pop-to-sidebar-on-toggle-open t
  "Whether to jump to sidebar upon toggling open.

This is used in conjunction with `dired-sidebar-toggle-sidebar'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-use-evil-integration t
  "Whether to integrate with evil.

This needs to be set before calling command `dired-sidebar-mode'
for the first time.

If using ‘use-package’, set this in :init."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-cycle-subtree-on-click t
  "Whether to cycle subtree on click.

This only takes effect if `dired-subtree' is installed."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-delay-auto-revert-updates t
  "Whether to delay automatically reverting buffer.

When true, only allow function `auto-revert-mode' to update every
`dird-sidebar-stale-buffer-time-idle-delay' seconds."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-stale-buffer-time-idle-delay 1.5
  "The time in idle seconds to wait before checking if buffer is stale."
  :type 'number
  :group 'dired-sidebar)

;; Internal

(defvar dired-sidebar-alist '()
  "An alist that maps from frame to currently opened `dired-sidebar' buffer.")

(defvar-local dired-sidebar-stale-buffer-timer nil
  "Timer used for setting `dired-sidebar-check-for-stale-buffer-p'.

This is buffer local.")

(defvar-local dired-sidebar-check-for-stale-buffer-p nil
  "Whether to check if buffer is stale.

When this is true `dired-sidebar-buffer-stale-p'
will check if buffer is stale through `auto-revert-mode'.")

;; Mode

(defmacro dired-sidebar-with-no-dedication (&rest body)
  "Run BODY after undedicating window."
  `(progn
     (set-window-dedicated-p (get-buffer-window (current-buffer)) nil)
     ,@body
     (set-window-dedicated-p (get-buffer-window (current-buffer)) t)))

(defvar dired-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (when (fboundp 'dired-subtree-toggle)
      (define-key map [tab] 'dired-subtree-toggle))
    (define-key map (kbd "C-m") 'dired-sidebar-find-file)
    (define-key map (kbd "RET") 'dired-sidebar-find-file)
    (define-key map (kbd "<return>") 'dired-sidebar-find-file)
    (define-key map "^" 'dired-sidebar-up-directory)
    (define-key map (kbd "C-o") 'dired-sidebar-find-file-ace)
    (define-key map [mouse-2] 'dired-sidebar-mouse-subtree-cycle-or-find-file)

    ;; Not sure why this doesn't load the bindings if it's
    ;; set up in the minor mode.
    (when dired-sidebar-use-evil-integration
      (with-eval-after-load 'evil
        (when (fboundp 'dired-subtree-toggle)
          (evil-define-minor-mode-key 'normal 'dired-sidebar-mode
            [tab] 'dired-subtree-toggle))
        (evil-define-minor-mode-key 'normal 'dired-sidebar-mode
          (kbd "C-m") 'dired-sidebar-find-file
          (kbd "RET") 'dired-sidebar-find-file
          (kbd "<return>") 'dired-sidebar-find-file
          "^" 'dired-sidebar-up-directory
          (kbd "C-o") 'dired-sidebar-find-file-ace
          [mouse-2] 'dired-sidebar-mouse-subtree-cycle-or-find-file)))
    map)
  "Keymap used for symbol `dired-sidebar-mode'.")

(define-minor-mode dired-sidebar-mode
  "A minor mode that leverages `dired' to emulate a Tree browser."
  :init-value nil
  :lighter ""
  :keymap dired-sidebar-mode-map

  (setq window-size-fixed 'width)

  ;; We don't want extra details in the sidebar.
  (dired-hide-details-mode)

  (when dired-sidebar-delay-auto-revert-updates
    (setq-local buffer-stale-function #'dired-sidebar-buffer-stale-p)
    (let ((current-buffer (current-buffer)))
      (setq dired-sidebar-stale-buffer-timer
            (run-with-idle-timer
             dired-sidebar-stale-buffer-time-idle-delay
             t (lambda ()
                 ;; Only do a check if `dired-sidebar' buffer is in the foreground.
                 (when (get-buffer-window current-buffer)
                   (with-current-buffer current-buffer
                     (setq dired-sidebar-check-for-stale-buffer-p t))))))

      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (timerp dired-sidebar-stale-buffer-timer)
                    (cancel-timer dired-sidebar-stale-buffer-timer)))
                nil t)))

  (when (and
         dired-sidebar-use-all-the-icons
         (display-graphic-p)
         (fboundp 'all-the-icons-dired-mode))
    (all-the-icons-dired-mode))

  (when dired-sidebar-use-custom-font
    (dired-sidebar-set-font))

  (when dired-sidebar-use-custom-modeline
    (dired-sidebar-set-mode-line))

  (when dired-sidebar-refresh-on-projectile-switch
    (add-hook
     'projectile-after-switch-project-hook
     (lambda ()
       (when (fboundp 'projectile-project-root)
         (dired-sidebar-switch-to-dir (projectile-project-root))))))

  (dired-unadvertise (dired-current-directory))
  (dired-sidebar-update-buffer-name))

;; User Interface

;;;###autoload
(defun dired-sidebar-toggle-sidebar ()
  "Toggle the project explorer window."
  (interactive)
  (if (dired-sidebar-showing-sidebar-in-frame-p)
      (dired-sidebar-hide-sidebar)
    (dired-sidebar-show-sidebar)
    (when dired-sidebar-pop-to-sidebar-on-toggle-open
      (pop-to-buffer (dired-sidebar-sidebar-buffer-in-frame)))))

;;;###autoload
(defun dired-sidebar-show-sidebar (&optional b)
  "Show sidebar using B or use currect project root in the selected frame."
  (interactive)
  (let ((buffer (or b (dired-sidebar-get-or-create-buffer))))
    (display-buffer-in-side-window buffer '((side . left)))
    (let ((window (get-buffer-window buffer)))
      (set-window-dedicated-p window t)
      (with-selected-window window
        (let ((window-size-fixed))
          (dired-sidebar-set-width dired-sidebar-width))))
    (with-current-buffer buffer
      (dired-sidebar-mode))
    (dired-sidebar-update-state-in-frame buffer)))

;;;###autoload
(defun dired-sidebar-hide-sidebar ()
  "Hide the sidebar in the selected frame."
  (interactive)
  (let ((buffer (dired-sidebar-sidebar-buffer-in-frame)))
    (delete-window (get-buffer-window buffer))
    (dired-sidebar-update-state-in-frame nil)))

(defun dired-sidebar-find-file (&optional dir ace)
  "Wrapper over `dired-find-file'.
Optional argument DIR Fine file using DIR of available.
Optional argument ACE Whether or not to use `ace-window' when opening file."
  (interactive)
  (let ((find-file-run-dired t)
        (dired-file-name (or dir (dired-get-file-for-visit))))
    (if (file-directory-p dired-file-name)
        (dired-sidebar-with-no-dedication
         (let ((buf-name (dired-sidebar-sidebar-buffer-name
                          dired-file-name)))
           (if (dired-sidebar-buffer-exists-p buf-name)
               (progn
                 (switch-to-buffer buf-name)
                 (dired-sidebar-update-state-in-frame (current-buffer)))
             ;; Copied from `dired-find-file'.
             (find-file dired-file-name)
             (dired-sidebar-mode)
             (dired-sidebar-update-state-in-frame (current-buffer)))))
      ;; Select the sidebar window so that `next-window' is consistent
      ;; in picking the window next to the sidebar.
      ;; This is useful for when `dired-sidebar-find-file' is called
      ;; from a buffer that is not already in the sidebar buffer.
      ;; e.g. A mouse click event.
      (with-selected-window (get-buffer-window
                             (dired-sidebar-sidebar-buffer-in-frame))
        (select-window (if (and ace
                                (fboundp 'aw-select))
                           (aw-select "Select buffer")
                         (next-window)))
        (find-file dired-file-name)))))

(defun dired-sidebar-find-file-ace ()
  "Like `dired-sidebar-find-file' but open files with `ace-window'."
  (interactive)
  (dired-sidebar-find-file nil :ace))

(defun dired-sidebar-up-directory ()
  "Wrapper over `dired-up-directory'."
  (interactive)
  (dired-sidebar-with-no-dedication
   (let* ((dir (dired-current-directory))
          (up (file-name-directory (directory-file-name dir)))
          (up-name (dired-sidebar-sidebar-buffer-name up)))
     (if (dired-sidebar-buffer-exists-p up-name)
         (progn
           (switch-to-buffer up-name)
           (dired-sidebar-update-state-in-frame (current-buffer)))
       (dired-up-directory)
       (dired-sidebar-mode)
       (dired-sidebar-update-state-in-frame (current-buffer))))))

(defun dired-sidebar-mouse-subtree-cycle-or-find-file (event)
  "Handle a mouse click EVENT in `dired-sidebar'.

For directories, if `dired-sidebar-cycle-subtree-on-click' is true and
`dired-subtree' is installed, cycle the directory.

Otherwise, behaves the same as if user clicked on a file.

For files, use `dired-sidebar-find-file'.

This uses the same code as `dired-mouse-find-file-other-window' to find
the relevant file-directory clicked on by the mouse."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    ;; There's a flicker doing this but it doesn't seem like
    ;; `dired-subtree-cycle' works without first selecting the window.
    (with-selected-window window
      (if (and dired-sidebar-cycle-subtree-on-click
               (fboundp 'dired-subtree-cycle)
               (file-directory-p file)
               (not (string-suffix-p "." file)))
          (dired-subtree-cycle)
        (dired-sidebar-find-file file)))))

;; Helpers

(defun dired-sidebar-buffer-exists-p (buffer-name)
  "Check if a `dired-sidebar' buffer exists for BUFFER-NAME."
  (get-buffer buffer-name))

(defun dired-sidebar-sidebar-root ()
  "Return directory using `projectile' or current directory otherwise."
  (condition-case nil
      (if (fboundp 'projectile-project-root)
          (projectile-project-root)
        default-directory)
    (error default-directory)))

(defun dired-sidebar-sidebar-buffer-name (dir)
  "Return name of `dired-sidebar' buffer given DIR."
  (let ((b (cond
            ((string-suffix-p ".." dir)
             ;; ~/.emacs.d/elpa/.. -> ~/.emacs.d/
             (file-name-directory (substring dir 0 (- (length dir) 3))))
            ((not (string-suffix-p "/" dir))
             (concat dir "/"))
            (:default
             dir))))
    (concat ":" (abbreviate-file-name b))))

(defun dired-sidebar-get-or-create-buffer (&optional dir)
  "Get or create a `dired-sidebar' buffer matching DIR."
  (interactive)
  (let* ((root (or dir (dired-sidebar-sidebar-root)))
         (name (dired-sidebar-sidebar-buffer-name root))
         (buffer (or (get-buffer name)
                     (dired-noselect root))))
    ;; Rename the buffer generated by `dired-noselect'.
    (when (not (string-equal (buffer-name buffer) name))
      (with-current-buffer buffer
        (rename-buffer name)))
    buffer))

(defun dired-sidebar-set-font ()
  "Customize font in `dired-sidebar'.

Set font to a variable width (proportional) in the current buffer."
  (interactive)
  (setq buffer-face-mode-face dired-sidebar-font-face)
  (buffer-face-mode))

(defun dired-sidebar-set-mode-line ()
  "Customize modeline in `dired-sidebar'."
  (setq mode-line-format dired-sidebar-mode-line-format))

(defun dired-sidebar-set-width (width)
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

(defun dired-sidebar-update-buffer-name ()
  "Change buffer name to avoid collision with regular `dired' buffers."
  (rename-buffer
   (dired-sidebar-sidebar-buffer-name (dired-current-directory))))

(defun dired-sidebar-update-state-in-frame (buffer &optional f)
  "Update current state with BUFFER for sidebar in F or selected frame."
  (let ((frame (or f (selected-frame))))
    (if (assq frame dired-sidebar-alist)
        (setcdr (assq frame dired-sidebar-alist) buffer)
      (push `(,frame . ,buffer) dired-sidebar-alist))))

(defun dired-sidebar-showing-sidebar-in-frame-p (&optional f)
  "Whether F or selected frame is showing a sidebar.

Check if F or selected frame contains a sidebar and return
corresponding buffer if buffer has a window attached to it.

Return buffer if so."
  (if-let (buffer (alist-get (or f (selected-frame)) dired-sidebar-alist))
      (if (get-buffer-window buffer)
          buffer
        nil)
    nil))

(defun dired-sidebar-showing-sidebar-buffer (buffer)
  "Check if BUFFER is being shown in sidebar."
  (when (eq buffer (alist-get (selected-frame) dired-sidebar-alist))
    (get-buffer-window buffer)))

(defun dired-sidebar-sidebar-buffer-in-frame (&optional f)
  "Return the current sidebar buffer in F or selected frame."
  (let ((frame (or f (selected-frame))))
    (alist-get frame dired-sidebar-alist)))

(defun dired-sidebar-switch-to-dir (dir)
  "Update buffer with DIR as root."
  (when (dired-sidebar-showing-sidebar-in-frame-p)
    (let ((buffer (dired-sidebar-get-or-create-buffer dir)))
      (dired-sidebar-show-sidebar buffer))))

(defun dired-sidebar-buffer-stale-p (&optional noconfirm)
  "Wrapper over `dired-buffer-stale-p'.

Check if buffer is stale only if `dired-sidebar-stale-buffer-time-idle-delay'

has elapsed.

Optional argument NOCONFIRM Pass NOCONFIRM on to `dired-buffer-stale-p'."
  (when dired-sidebar-check-for-stale-buffer-p
    (setq dired-sidebar-check-for-stale-buffer-p nil)
    (dired-buffer-stale-p noconfirm)))

(provide 'dired-sidebar)
;;; dired-sidebar.el ends here
