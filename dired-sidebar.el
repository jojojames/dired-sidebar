;;; dired-sidebar.el --- Tree browser leveraging dired -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/dired-sidebar
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dired-subtree "0.0.1"))
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
(require 'dired-subtree)
(require 'evil nil t)
(require 'face-remap)
(eval-when-compile (require 'subr-x))

;; Compatibility
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let)
      (function-put #'if-let* 'lisp-indent-function 2)
      (function-put #'when-let* 'lisp-indent-function 1))))

;; Customizations

(defgroup dired-sidebar nil
  "A major mode leveraging `dired-mode' to display a filesystem in a tree
layout."
  :group 'files)

(defcustom dired-sidebar-use-custom-font nil
  "Show `dired-sidebar' with custom font.

This face can be customized using `dired-sidebar-face'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-face nil
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
    " "  mode-line-end-spaces)
  "Mode line format for `dired-sidebar'."
  :type 'list
  :group 'dired-sidebar)

(defcustom dired-sidebar-theme 'icons
  "*The tree style to display.
`ascii' is the simplest style, it will use +/- to display the fold state,
it is suitable for terminal.
`icons' use `all-the-icons'.
`nerd' use the nerdtree indentation mode and arrow."
  :group 'dired-sidebar
  :type '(choice (const ascii)
                 (const icons)
                 (const nerd)))

(defcustom dired-sidebar-width 35
  "Width of the `dired-sidebar' buffer."
  :type 'integer
  :group 'dired-sidebar)

(defcustom dired-sidebar-refresh-on-projectile-switch t
  "Refresh sidebar when `projectile' changes projects."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-should-follow-file nil
  "Refresh sidebar to match current file."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-pop-to-sidebar-on-toggle-open t
  "Whether to jump to sidebar upon toggling open.

This is used in conjunction with `dired-sidebar-toggle-sidebar'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-follow-file-at-point-on-toggle-open t
  "Whether to recursively cycle the subtree and put point on file.

Similar to `dired-jump'.  This moves point inside sidebar buffer
to where current-buffer-file is \(if it exists\) but does not necessarily
select the sidebar window."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-use-evil-integration t
  "Whether to integrate with evil.

This needs to be set before calling command `dired-sidebar-mode'
for the first time.

If using `use-package', set this in :init."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-use-magit-integration t
  "Whether to integrate with `magit-mode'.

When true:

When finding file to point at for
`dired-sidebar-follow-file-at-point-on-toggle-open', use file at point
in `magit' buffer.

When finding root directory for sidebar, use directory specified by `magit'."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-use-term-integration nil
  "Whether to integrate with `term-mode'.

When true:

When finding root directory for sidebar, use PWD of `term-mode'. This is turned
off by default due to the experimental nature of getting the PWD from the
terminal.

Look at `dired-sidebar-term-get-pwd' for implementation."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-cycle-subtree-on-click t
  "Whether to cycle subtree on click."
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

(defcustom dired-sidebar-follow-file-idle-delay 2
  "The time in idle seconds to wait before checking if sidebar should
follow file."
  :type 'number
  :group 'dired-sidebar)

(defcustom dired-sidebar-tui-update-delay 0.05
  "The time in idle seconds to wait before updating tui interface.

This only takes effect if `all-the-icons-dired' is disabled."
  :type 'number
  :group 'dired-sidebar)

(defcustom dired-sidebar-refresh-on-special-commands t
  "Whether or not to trigger auto-revert after certain functions.

Warning: This is implemented by advising specific dired functions."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-disable-dired-collapse t
  "Whether or not to disable `dired-collapse' if it's enabled."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-special-refresh-commands
  '(dired-do-delete
    dired-do-rename
    dired-do-copy
    dired-do-flagged-delete
    dired-create-directory
    (delete-file . 5)
    (save-buffer . 5))
  "A list of commands that will trigger a refresh of the sidebar.

The command can be an alist with the CDR of the alist being the amount of time
to wait to refresh the sidebar after the CAR of the alist is called.

Set this to nil or set `dired-sidebar-refresh-on-special-commands' to nil
to disable automatic refresh when a special command is triggered."
  :type 'list
  :group 'dired-sidebar)

(defcustom dired-sidebar-alternate-select-window-function
  #'dired-sidebar-default-alternate-select-window
  "Function to call when using alternative window selection.

Alternative window selection is used when `dired-sidebar-find-file' is called
with a prefix arg or when `dired-sidebar-find-file-alt' is called."
  :type 'function
  :group 'dired-sidebar)

(defcustom dired-sidebar-recenter-cursor-on-follow-file t
  "Whether or not to center cursor when pointing at file."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-recenter-cursor-on-tui-update nil
  "Whether or not to center cursor when updating tui interface."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-display-autorevert-messages nil
  "Whether or not to display `autorevert' messages."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-open-file-in-most-recently-used-window t
  "Whether or not to open files in most recently used window."
  :type 'boolean
  :group 'dired-sidebar)

;; Internal

(defvar dired-sidebar-alist '()
  "An alist that maps from frame to currently opened `dired-sidebar' buffer.")

(defvar-local dired-sidebar-stale-buffer-timer nil
  "Timer used for setting `dired-sidebar-check-for-stale-buffer-p'.

This is buffer local.")

(defvar-local dired-sidebar-follow-file-timer nil
  "Timer used when `dired-sidebar-should-follow-file' is true.")

(defvar-local dired-sidebar-check-for-stale-buffer-p nil
  "Whether to check if buffer is stale.

When this is true `dired-sidebar-buffer-stale-p'
will check if buffer is stale through `auto-revert-mode'.")

;; Mode

(defmacro dired-sidebar-with-no-dedication (&rest body)
  "Run BODY after undedicating window."
  (declare (debug (&rest form)))
  `(progn
     (let ((window (get-buffer-window (current-buffer))))
       (set-window-dedicated-p window nil)
       ,@body
       (set-window-dedicated-p window t))))

(defvar dired-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (when (fboundp 'dired-subtree-toggle)
      (define-key map [tab] 'dired-subtree-toggle))
    (define-key map (kbd "C-m") 'dired-sidebar-find-file)
    (define-key map (kbd "RET") 'dired-sidebar-find-file)
    (define-key map (kbd "<return>") 'dired-sidebar-find-file)
    (define-key map "^" 'dired-sidebar-up-directory)
    (define-key map (kbd "C-o") 'dired-sidebar-find-file-alt)
    (define-key map [mouse-2] 'dired-sidebar-mouse-subtree-cycle-or-find-file)

    ;; Not sure why this doesn't load the bindings if it's
    ;; set up in the minor mode.
    (when dired-sidebar-use-evil-integration
      (with-eval-after-load 'evil
        (when (fboundp 'dired-subtree-toggle)
          (when (fboundp 'evil-define-minor-mode-key)
            (evil-define-minor-mode-key 'normal 'dired-sidebar-mode
              [tab] 'dired-subtree-toggle)))
        (when (fboundp 'evil-define-minor-mode-key)
          (evil-define-minor-mode-key 'normal 'dired-sidebar-mode
            (kbd "C-m") 'dired-sidebar-find-file
            (kbd "RET") 'dired-sidebar-find-file
            (kbd "<return>") 'dired-sidebar-find-file
            "^" 'dired-sidebar-up-directory
            (kbd "C-o") 'dired-sidebar-find-file-alt
            [mouse-2] 'dired-sidebar-mouse-subtree-cycle-or-find-file))
        ;; Although `evil-define-minor-mode-key' is supposed to define bindings
        ;; immediately, I did not see that happen when restoring `dired-sidebar'
        ;; with `desktop-read'.
        ;; Force keymaps to normalize.
        (when (fboundp 'evil-normalize-keymaps)
          (evil-normalize-keymaps))))
    map)
  "Keymap used for symbol `dired-sidebar-mode'.")

(define-minor-mode dired-sidebar-mode
  "A minor mode that leverages `dired' to emulate a Tree browser."
  :init-value nil
  :lighter ""
  :keymap dired-sidebar-mode-map

  (setq window-size-fixed 'width)

  ;; Match backgrounds.
  (setq-local dired-subtree-use-backgrounds nil)

  ;; We don't want extra details in the sidebar.
  (dired-hide-details-mode)

  (when (and dired-sidebar-disable-dired-collapse
             (fboundp 'dired-collapse-mode)
             (bound-and-true-p dired-collapse-mode))
    (dired-collapse-mode -1))

  (when (and
         (not dired-sidebar-display-autorevert-messages)
         (boundp 'auto-revert-verbose))
    (setq-local auto-revert-verbose nil))

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

  (when dired-sidebar-refresh-on-special-commands
    (mapc
     (lambda (x)
       (if (consp x)
           (let ((command (car x))
                 (delay (cdr x)))
             (advice-add
              command
              :after
              (defalias (intern (format "dired-sidebar-refresh-after-%S" command))
                (function
                 (lambda (&rest _)
                   (let ((timer-symbol
                          (intern
                           (format
                            "dired-sidebar-refresh-%S-timer" command))))
                     (when (and (boundp timer-symbol)
                                (timerp (symbol-value timer-symbol)))
                       (cancel-timer (symbol-value timer-symbol)))
                     (setf
                      (symbol-value timer-symbol)
                      (run-with-idle-timer
                       delay
                       nil
                       #'dired-sidebar-refresh-buffer))))))))
         (advice-add x :after #'dired-sidebar-refresh-buffer)))
     dired-sidebar-special-refresh-commands))

  (cond
   ((and (eq dired-sidebar-theme 'icons)
         (display-graphic-p)
         (or
          (fboundp 'all-the-icons-dired-mode)
          (autoloadp (symbol-function 'all-the-icons-dired-mode))))
    (with-no-warnings
      (all-the-icons-dired-mode)))
   ((eq dired-sidebar-theme 'nerd)
    (dired-sidebar-setup-tui))
   (:default
    (dired-sidebar-setup-tui)))

  (when dired-sidebar-use-custom-font
    (dired-sidebar-set-font))

  (when dired-sidebar-use-custom-modeline
    (dired-sidebar-set-mode-line))

  (when dired-sidebar-refresh-on-projectile-switch
    (add-hook 'projectile-after-switch-project-hook
              #'dired-sidebar-follow-file))

  (when dired-sidebar-should-follow-file
    (setq dired-sidebar-follow-file-timer
          (run-with-idle-timer
           dired-sidebar-follow-file-idle-delay
           t #'dired-sidebar-follow-file)))

  (dired-unadvertise (dired-current-directory))
  (dired-sidebar-update-buffer-name)
  (dired-sidebar-update-state-in-frame (current-buffer)))

;; User Interface

;;;###autoload
(defun dired-sidebar-toggle-sidebar (&optional dir)
  "Toggle the project explorer window.
Optional argument DIR Use DIR as sidebar root if available.

With universal argument, use current directory."
  (interactive)
  (if (dired-sidebar-showing-sidebar-in-frame-p)
      (dired-sidebar-hide-sidebar)
    (let* ((file-to-show (dired-sidebar-get-file-to-show))
           (dir-to-show (or dir
                            (when current-prefix-arg default-directory)
                            (dired-sidebar-get-dir-to-show)))
           (sidebar-buffer (dired-sidebar-get-or-create-buffer dir-to-show)))
      (dired-sidebar-show-sidebar sidebar-buffer)
      (if (and dired-sidebar-follow-file-at-point-on-toggle-open
               file-to-show)
          (if dired-sidebar-pop-to-sidebar-on-toggle-open
              (dired-sidebar-point-at-file file-to-show dir-to-show)
            (with-selected-window (selected-window)
              (dired-sidebar-point-at-file file-to-show dir-to-show)))
        (when dired-sidebar-pop-to-sidebar-on-toggle-open
          (pop-to-buffer (dired-sidebar-sidebar-buffer-in-frame)))))))

(defun dired-sidebar-point-at-file (name root)
  "Try to point at NAME from sidebar.

Keep `dired' pointed at ROOT while cycling directories until
NAME is found in ROOT path.

This is dependent on `dired-subtree-cycle'."
  (let ((sidebar (dired-sidebar-sidebar-buffer-in-frame)))
    (pop-to-buffer sidebar)
    (when (and name
               (fboundp 'dired-subtree-cycle)
               ;; Checking for a private method. *shrug*
               (fboundp 'dired-subtree--is-expanded-p))
      (pop-to-buffer sidebar)
      (goto-char 0)
      (let* ((path root)
             ;; Imagine root is /root/var/ and name is
             ;; /root/var/a/b/c.
             ;; This will return a list of '\("a" "b" "c"\).
             (dirs (split-string (cadr (split-string name root)) "/")))
        (dolist (dir dirs)
          (setq path (concat path dir))
          (if (file-regular-p path)
              ;; Try to use `dired-goto-file' to go to the correct
              ;; file. If that fails, just search for the text.
              (let ((default-directory (file-name-directory path)))
                (unless (dired-goto-file path)
                  ;; It's hard to get this right so just using a
                  ;; heuristic will get 90% of the way there.
                  ;; Making sure there's a space in front of the name
                  ;; skips matches that contains the name as a
                  ;; substring which is probably good enough...
                  (re-search-forward (concat "^.*[[:space:]]"
                                             (regexp-quote dir)))))
            (re-search-forward (concat "^.*[[:space:]]" (regexp-quote dir)))
            ;; Check if subtree has already been expanded.
            ;; Basically, we're using `dired-subtree-cycle' more
            ;; like dired-subtree-expand.
            (when (not (dired-subtree--is-expanded-p))
              ;; This will probably throw an error when trying to expand
              ;; directories that have been collapsed by `dired-collapse'.
              (dired-subtree-cycle))
            (setq path (concat path "/")))))
      (when dired-sidebar-recenter-cursor-on-follow-file
        (recenter nil)))))

;;;###autoload
(defun dired-sidebar-toggle-with-current-directory ()
  "Like `dired-sidebar-toggle-sidebar' but use current-directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u
    (call-interactively #'dired-sidebar-toggle-sidebar)))

;;;###autoload
(defun dired-sidebar-show-sidebar (&optional b)
  "Show sidebar displaying buffer B."
  (interactive)
  (let ((buffer (or b
                    ;; Only expect this to be hit when called interactively.
                    (dired-sidebar-get-or-create-buffer
                     (dired-sidebar-get-dir-to-show)))))
    (display-buffer-in-side-window buffer '((side . left)))
    (let ((window (get-buffer-window buffer)))
      (set-window-dedicated-p window t)
      (with-selected-window window
        (let ((window-size-fixed))
          (dired-sidebar-set-width dired-sidebar-width))))
    (with-current-buffer buffer
      ;; For the case where we've already turned on the mode.
      (unless (bound-and-true-p dired-sidebar-mode)
        (dired-sidebar-mode)))
    (dired-sidebar-update-state-in-frame buffer)))

;;;###autoload
(defun dired-sidebar-hide-sidebar ()
  "Hide the sidebar in the selected frame."
  (interactive)
  (when-let* ((buffer (dired-sidebar-sidebar-buffer-in-frame)))
    (delete-window (get-buffer-window buffer))
    (dired-sidebar-update-state-in-frame nil)))

(defun dired-sidebar-find-file (&optional dir)
  "Wrapper over `dired-find-file'.
Optional argument DIR Fine file using DIR of available.

With prefix argument, use `dired-sidebar-alternate-select-window-function' for
window selection."
  (interactive)
  (let ((find-file-run-dired t)
        (dired-file-name (or dir (dired-get-file-for-visit)))
        (select-with-alt-window-function current-prefix-arg))
    (if (and (file-directory-p dired-file-name)
             ;; For "." open a full-blown dired buffer, since the directory is
             ;; already open in the sidebar.
             (not (string= (file-name-nondirectory dired-file-name)
                           ".")))
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
      (switch-to-buffer (dired-sidebar-sidebar-buffer-in-frame))
      (select-window
       (if select-with-alt-window-function
           (funcall dired-sidebar-alternate-select-window-function)
         (if dired-sidebar-open-file-in-most-recently-used-window
             (get-mru-window)
           (next-window))))
      (find-file dired-file-name))))

(defun dired-sidebar-find-file-alt ()
  "Like `dired-sidebar-find-file' but select window with alterate method.

Select alternate window using `dired-sidebar-alternate-select-window-function'."
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u
    (call-interactively 'dired-sidebar-find-file)))

(defun dired-sidebar-up-directory ()
  "Wrapper over `dired-up-directory'."
  (interactive)
  (dired-sidebar-with-no-dedication
   ;; If `dired-subtree' is used, `dired-current-directory' is redefined.
   ;; So move point to the top of the buffer to get the actual directory and
   ;; not the one at point.
   (goto-char (point-min))
   (let* ((dir (dired-current-directory))
          (up (file-name-directory (directory-file-name dir)))
          (up-name (dired-sidebar-sidebar-buffer-name up)))
     (if (dired-sidebar-buffer-exists-p up-name)
         (progn
           (switch-to-buffer up-name)
           (dired-sidebar-update-state-in-frame (current-buffer)))
       (dired-up-directory)
       (dired-sidebar-mode)
       (dired-sidebar-update-state-in-frame (current-buffer)))
     (let ((default-directory up))
       (dired-goto-file dir)))))

(defun dired-sidebar-mouse-subtree-cycle-or-find-file (event)
  "Handle a mouse click EVENT in `dired-sidebar'.

For directories, if `dired-sidebar-cycle-subtree-on-click' is true,
cycle the directory.

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

(defun dired-sidebar-get-or-create-buffer (root)
  "Get or create a `dired-sidebar' buffer matching ROOT."
  (interactive)
  (let ((name (dired-sidebar-sidebar-buffer-name root)))
    (if-let* ((existing-buffer (get-buffer name)))
        existing-buffer
      (let ((buffer (dired-noselect root)))
        ;; When opening a sidebar while in a dired buffer that matches
        ;; the sidebar's root directory.
        (if (eq (current-buffer) buffer)
            ;; https://github.com/Fuco1/dired-hacks/issues/102
            (if (member 'dired-collapse-mode dired-mode-hook)
                (progn
                  (remove-hook 'dired-mode-hook 'dired-collapse-mode)
                  (let ((clone (clone-buffer)))
                    (add-hook 'dired-mode-hook 'dired-collapse-mode)
                    clone))
              (clone-buffer))
          ;; Rename the buffer generated by `dired-noselect'.
          (when (not (string-equal (buffer-name buffer) name))
            (with-current-buffer buffer
              (rename-buffer name)))
          buffer)))))

(defun dired-sidebar-set-font ()
  "Customize font in `dired-sidebar'.

Set font to a variable width (proportional) in the current buffer."
  (interactive)
  (setq-local buffer-face-mode-face dired-sidebar-face)
  (buffer-face-mode))

(defun dired-sidebar-set-mode-line ()
  "Customize modeline in `dired-sidebar'."
  (setq mode-line-format dired-sidebar-mode-line-format))

(defun dired-sidebar-set-width (width)
  "Set the width of the buffer to WIDTH when it is created."
  ;; Copied from `treemacs--set-width' as well as `neotree'.
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
  (when-let* ((buffer (dired-sidebar-sidebar-buffer-in-frame f)))
    (get-buffer-window buffer)))

(defun dired-sidebar-sidebar-buffer-in-frame (&optional f)
  "Return the current sidebar buffer in F or selected frame.

This can return nil if the buffer has been killed."
  (let* ((frame (or f (selected-frame)))
         (buffer (alist-get frame dired-sidebar-alist)))
    ;; The buffer can be killed for a variety of reasons.
    ;; This side effect is kind of messy but it's the simplest place
    ;; to put the clean up code for `dired-sidebar-alist'.
    (if (buffer-live-p buffer)
        buffer
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
      ;; Documentation for `assq-delete-all'.
      ;; What kind of API is this?? :()
      ;; Why does it only modify 'often' and not 'always'? ¯\_(ツ)_/¯
      ;; It returns the shortened alist, and often modifies the original list
      ;; structure of alist.
      ;; For correct results, use the return value of assq-delete-all rather
      ;; than looking at the saved value of alist.
      (setq dired-sidebar-alist
            (assq-delete-all frame dired-sidebar-alist))
      nil)))

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

(defun dired-sidebar-refresh-buffer (&rest _)
  "Refresh sidebar buffer."
  (when-let* ((sidebar (dired-sidebar-sidebar-buffer-in-frame)))
    (with-current-buffer sidebar
      (let ((auto-revert-verbose nil))
        (revert-buffer)))))

(defun dired-sidebar-follow-file ()
  "Follow new file.

The root of the sidebar will be determined by `dired-sidebar-get-dir-to-show'
and the file followed is will be determined by `dired-sidebar-get-file-to-show',

both accounting for the currently selected window."
  (when (dired-sidebar-showing-sidebar-in-frame-p)
    ;; Wrap in `with-selected-window' because we don't want to pop to
    ;; the sidebar buffer.
    ;; We also need to pick the correct selected-window so that
    ;; `dired-sidebar-get-dir-to-show' can get the correct root to change to.
    (with-selected-window (selected-window)
      (let ((root (dired-sidebar-get-dir-to-show)))
        (dired-sidebar-switch-to-dir root)
        (when dired-sidebar-follow-file-at-point-on-toggle-open
          (when-let* ((file (dired-sidebar-get-file-to-show)))
            (dired-sidebar-point-at-file file root)))))))

(defun dired-sidebar-default-alternate-select-window ()
  "Default function for `dired-sidebar-alternate-select-window-function'."
  (if (fboundp 'aw-select)
      (aw-select "Select Window")
    (next-window)))

(defun dired-sidebar-get-dir-to-show ()
  "Return the directory `dired-sidebar' should open to."
  (cond
   ((and (derived-mode-p 'magit-mode)
         dired-sidebar-use-magit-integration
         (fboundp 'magit-toplevel))
    (magit-toplevel))
   ((and (eq major-mode 'term-mode)
         dired-sidebar-use-term-integration)
    (dired-sidebar-term-get-pwd))
   ((and (eq major-mode 'dired-mode)
         (not dired-sidebar-mode))
    (expand-file-name default-directory))
   ((and (eq major-mode 'ibuffer-mode)
         (fboundp 'ibuffer-current-buffer)
         (ibuffer-current-buffer))
    (let ((buffer-at-point (ibuffer-current-buffer)))
      (if (fboundp 'ibuffer-projectile-root)
          (if-let* ((ibuffer-projectile-root
                     (ibuffer-projectile-root buffer-at-point)))
              (cdr ibuffer-projectile-root)
            (with-current-buffer buffer-at-point
              default-directory))
        (with-current-buffer buffer-at-point
          default-directory))))
   (:default
    (dired-sidebar-sidebar-root))))

(defun dired-sidebar-get-file-to-show ()
  "Return the file `dired-sidebar' should open to.

This may return nil if there's no suitable file to show."
  (cond
   ((and dired-sidebar-use-magit-integration
         (derived-mode-p 'magit-mode)
         (fboundp 'magit-file-at-point)
         (magit-file-at-point))
    (expand-file-name (magit-file-at-point)))
   ((and (eq major-mode 'dired-mode)
         (not dired-sidebar-mode))
    ;; Not sure if `dired-get-filename' is more appropriate.
    (condition-case nil
        (dired-get-file-for-visit)
      (error nil)))
   ((and (eq major-mode 'ibuffer-mode)
         (fboundp 'ibuffer-current-buffer))
    (let ((bf-name (buffer-file-name (ibuffer-current-buffer))))
      (and bf-name (file-exists-p bf-name) bf-name)))
   (:default
    (and buffer-file-name (file-exists-p buffer-file-name) buffer-file-name))))

(defun dired-sidebar-term-get-pwd ()
  "Get current directory of `term-mode'.

This is somewhat experimental/hacky."
  (interactive)
  (forward-paragraph)
  (when (fboundp 'term-previous-prompt)
    (term-previous-prompt 1))
  (when (fboundp 'term-simple-send)
    (term-simple-send (get-buffer-process (current-buffer)) "pwd"))
  (sleep-for 0 50)
  (forward-line 1)
  (let ((result (string-trim (thing-at-point 'line))))
    (kill-whole-line)
    (forward-line -1)
    (kill-whole-line)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Text User Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local dired-sidebar-tui-dired-displayed nil
  "Flags whether icons have been added.")

(defun dired-sidebar-tui-dired-reset (&optional _arg _noconfirm)
  "Function used as advice when redisplaying buffer."
  (setq-local dired-sidebar-tui-dired-displayed nil))

(defun dired-sidebar-tui-dired-display ()
  "Display the icons of files in a dired buffer."
  (interactive)
  (when (or t (and (not dired-sidebar-tui-dired-displayed) dired-subdir-alist))
    (setq-local dired-sidebar-tui-dired-displayed t)
    (let ((inhibit-read-only t)
          (collapsible-icon (if (eq dired-sidebar-theme 'nerd) "▾" "-"))
          (expandable-icon (if (eq dired-sidebar-theme 'nerd) "▸" "+")))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (dired-move-to-filename nil)
            (dired-move-to-filename)
            (let ((file (dired-get-filename 'verbatim t)))
              (unless (member file '("." ".."))
                (let ((filename (dired-get-filename nil t)))
                  (if (file-directory-p filename)
                      (if (dired-subtree--is-expanded-p)
                          (insert (concat collapsible-icon " "))
                        (insert (concat expandable-icon " ")))
                    (insert ""))))))
          (forward-line 1))))))

(defun dired-sidebar-tui-update-with-delay (&rest _)
  "Update tui interface after a delay."
  (run-with-idle-timer
   dired-sidebar-tui-update-delay nil
   (lambda ()
     (when-let* ((buffer (dired-sidebar-sidebar-buffer-in-frame)))
       (with-current-buffer buffer
         (dired-revert)
         (when dired-sidebar-recenter-cursor-on-tui-update
           (recenter)))))))

(defun dired-sidebar-tui-reset-in-sidebar (&rest _)
  "Runs `dired-sidebar-tui-dired-reset' in current `dired-sidebar' buffer."
  (when-let* ((buffer (dired-sidebar-sidebar-buffer-in-frame)))
    (with-current-buffer buffer
      (dired-sidebar-tui-dired-reset))))

(defun dired-sidebar-setup-tui ()
  "Sets up text user interface for `dired-sidebar'.

This is used in place of `all-the-icons' to add directory indicators.

e.g. + and -."
  (add-hook 'dired-after-readin-hook
            'dired-sidebar-tui-dired-display :append :local)
  (advice-add 'dired-revert :before 'dired-sidebar-tui-reset-in-sidebar)
  (setq-local dired-subtree-line-prefix " ")
  (advice-add 'dired-subtree-toggle :after #'dired-sidebar-tui-update-with-delay)
  (dired-revert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Text User Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-sidebar)
;;; dired-sidebar.el ends here
