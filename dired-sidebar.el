;;; dired-sidebar.el --- Tree browser leveraging dired -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

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

;; (use-package dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure nil
;;   :commands (dired-sidebar-toggle-sidebar))

;;; Code:

(require 'dired)
(require 'dired-subtree)
(eval-when-compile (require 'subr-x)) ; `if-let*' and `when-let*'

(declare-function buffer-face-mode-invoke "face-remap")

;; Compatibility

(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'dired-sidebar-if-let* #'if-let)
          (defalias 'dired-sidebar-when-let* #'when-let)
          (function-put #'dired-sidebar-if-let* 'lisp-indent-function 2)
          (function-put #'dired-sidebar-when-let* 'lisp-indent-function 1))
      (defalias 'dired-sidebar-if-let* #'if-let*)
      (defalias 'dired-sidebar-when-let* #'when-let*))))

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

(defface dired-sidebar-face nil
  "Face used by `dired-sidebar' for custom font.

This only takes effect if `dired-sidebar-use-custom-font' is true."
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
`nerd' use the nerdtree indentation mode and arrow.
`none' use no theme.
`vscode' use `vscode' icons.

This only takes effect if on a local connection. (e.g. Not Tramp)"
  :group 'dired-sidebar
  :type '(choice (const ascii)
                 (const icons)
                 (const nerd)
                 (const none)
                 (const vscode)))

(defcustom dired-sidebar-width 35
  "Width of the `dired-sidebar' buffer.
This option does not have effect if `dired-sidebar-resize-on-open' is nil.
If you set `dired-sidebar-resize-on-open' to nil, you can customize `dired-sidebar-display-alist'
to control the width anyway."
  :type 'integer
  :group 'dired-sidebar)

(defcustom dired-sidebar-window-fixed 'width
  "Whether the width or height of the sidebar window should be fixed (to prevent from resizing)."
  :type '(choice (const nil)
                 (const width)
                 (const height))
  :group 'dired-sidebar)

(defcustom dired-sidebar-refresh-on-project-switch t
  "Refresh sidebar when `projectile' or `project' changes projects."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-should-follow-file nil
  "Refresh sidebar to match current file."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-skip-subtree-parent t
  "Whether to skip subtree parent directory when jumping up."
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

(defcustom dired-sidebar-use-wdired-integration t
  "Whether to integrate with `wdired'."
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

(defcustom dired-sidebar-tui-update-delay 0.02
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
    (save-buffer . 5)
    magit-format-patch)
  "A list of commands that will trigger a refresh of the sidebar.

The command can be an alist with the CDR of the alist being the amount of time
to wait to refresh the sidebar after the CAR of the alist is called.

Set this to nil or set `dired-sidebar-refresh-on-special-commands' to nil
to disable automatic refresh when a special command is triggered."
  :type 'list
  :group 'dired-sidebar)

(defcustom dired-sidebar-toggle-hidden-commands
  '(balance-windows)
  "A list of commands that won't work when `dired-sidebar' is visible.

When the command is triggered, `dired-sidebar' will hide temporarily until
command is completed.

This functionality is implemented using advice.

Set this to nil to disable this advice."
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

(defcustom dired-sidebar-resize-on-open t
  "When dired sidebar window is showed, automatically adjust its width according to `dired-sidebar-width'"
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

(defcustom dired-sidebar-subtree-line-prefix dired-subtree-line-prefix
  "The line prefix to use when subtree is cycled."
  :type 'string
  :group 'dired-sidebar)

(defcustom dired-sidebar-display-alist '((side . left) (slot . -1))
  "Alist used in `display-buffer-in-side-window'.

e.g. (display-buffer-in-side-window buffer '((side . left) (slot . -1)))"
  :type 'alist
  :group 'dired-sidebar)

(defcustom dired-sidebar-close-sidebar-on-file-open nil
  "Whether or not to close sidebar when `dired-sidebar-find-file' is called.

This behavior only triggers if `dired-sidebar-find-file' is triggered on
a file."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-icon-scale .18
  "The scale of icons \(currently only applies to vscode theme.\)."
  :type 'number
  :group 'dired-sidebar)

(defcustom dired-sidebar-no-delete-other-windows nil
  "Whether or not to add `no-delete-other-window' parameter to window.

If this is true, when calling `delete-other-windows', `dired-sidebar' window
will continue showing.

For more information, look up `delete-other-windows'."
  :type 'boolean
  :group 'dired-sidebar)


(defcustom dired-sidebar-use-one-instance nil
  "Only show one buffer instance for dired-sidebar for each frame."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-display-remote-icons nil
  "Show icons for remote directories. nil by default for performance reasons."
  :type 'boolean
  :group 'dired-sidebar)

(defcustom dired-sidebar-block-icon-display-modes '(all-the-icons-dired-mode)
  "List of modes in `dired-mode-hook' that prevents icon display.

See https://github.com/jojojames/dired-sidebar/issues/43."
  :type 'list
  :group 'dired-sidebar)

;; Internal

(defvar dired-sidebar-basedir (file-name-directory load-file-name)
  "Store the directory dired-sidebar.el was loaded from.")

(defvar dired-sidebar-icons-dir (format "%sicons/" dired-sidebar-basedir)
  "Store the icons directory of `dired-sidebar'.")

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
    (define-key map (kbd "TAB") 'dired-sidebar-subtree-toggle)
    (define-key map [tab] 'dired-sidebar-subtree-toggle)
    (define-key map (kbd "C-m") 'dired-sidebar-find-file)
    (define-key map (kbd "RET") 'dired-sidebar-find-file)
    (define-key map (kbd "<return>") 'dired-sidebar-find-file)
    (define-key map "^" 'dired-sidebar-up-directory)
    (define-key map "-" 'dired-sidebar-up-directory)
    (define-key map (kbd "C-o") 'dired-sidebar-find-file-alt)
    (define-key map [mouse-2] 'dired-sidebar-mouse-subtree-cycle-or-find-file)
    map)
  "Keymap used for symbol `dired-sidebar-mode'.")

(define-derived-mode dired-sidebar-mode dired-mode
  "Dired-sidebar"
  "A major mode that puts `dired' in a sidebar."
  :group 'dired-sidebar

  ;; Hack for https://github.com/jojojames/dired-sidebar/issues/18.
  ;; Would be open to a better fix...
  ;; `dired-remember-hidden' in Emacs 25 (terminal?) seems to throw
  ;; an error upon calling `goto-char'.
  (when (<= emacs-major-version 25)
    (defun dired-sidebar-remember-hidden-hack (f &rest args)
      "Return nil for `dired-remember-hidden'.

Works around marker pointing to wrong buffer in Emacs 25."
      (if (eq major-mode 'dired-sidebar-mode)
          nil
        (apply f args)))
    (advice-remove 'dired-remember-hidden 'dired-sidebar-remember-hidden-hack)
    (advice-add 'dired-remember-hidden :around 'dired-sidebar-remember-hidden-hack))

  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32392
  (when dired-sidebar-use-wdired-integration
    (advice-remove 'wdired-change-to-dired-mode
                   'dired-sidebar-wdired-change-to-dired-mode-advice)
    (advice-remove 'wdired-change-to-wdired-mode
                   'dired-sidebar-wdired-change-to-wdired-mode-advice)

    (advice-add 'wdired-change-to-dired-mode
                :around 'dired-sidebar-wdired-change-to-dired-mode-advice)
    (advice-add 'wdired-change-to-wdired-mode
                :around 'dired-sidebar-wdired-change-to-wdired-mode-advice))

  (setq window-size-fixed dired-sidebar-window-fixed)

  ;; Match backgrounds.
  (setq-local dired-subtree-use-backgrounds nil)

  ;; `dired-subtree''s line prefix is determined by `dired-sidebar'.
  (setq-local dired-subtree-line-prefix dired-sidebar-subtree-line-prefix)

  ;; https://github.com/jojojames/dired-sidebar/issues/7
  ;; Symlinks are displayed incorrectly when these three things happen.
  ;; 1. `dired-hide-details-mode' is on.
  ;; 2. `dired-subtree' toggles a symlink folder via `dired-subtree-toggle'.
  ;; 3. `dired-hide-details-hide-symlink-targets' is set to true.
  ;; Since we use both 1 & 2, disable 3 to avoid the issue.
  ;; This needs to be set to nil before `dired-hide-details-mode' is called.
  (setq-local dired-hide-details-hide-symlink-targets nil)

  ;; Use `dired-sidebar-revert' instead that wraps `dired-revert'.
  (setq-local revert-buffer-function 'dired-sidebar-revert)

  ;; We don't want extra details in the sidebar.
  (dired-hide-details-mode)

  (when (and dired-sidebar-disable-dired-collapse
             (fboundp 'dired-collapse-mode))
    (add-hook 'dired-mode-hook
              (lambda ()
                (when (bound-and-true-p dired-collapse-mode)
                  (dired-collapse-mode -1)))
              :append :local))

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

  (when dired-sidebar-toggle-hidden-commands
    (mapc
     (lambda (x)
       (advice-add x :around #'dired-sidebar-advice-hide-temporarily))
     dired-sidebar-toggle-hidden-commands))

  (when dired-sidebar-use-custom-font
    (dired-sidebar-set-font))

  (when dired-sidebar-use-custom-modeline
    (dired-sidebar-set-mode-line))

  (when dired-sidebar-refresh-on-project-switch
    (advice-add 'project-find-file
                :after #'dired-sidebar-follow-file)
    (add-hook 'projectile-after-switch-project-hook
              #'dired-sidebar-follow-file))

  (when dired-sidebar-should-follow-file
    (setq dired-sidebar-follow-file-timer
          (run-with-idle-timer
           dired-sidebar-follow-file-idle-delay
           t #'dired-sidebar-follow-file)))

  ;; This comment is taken from `dired-readin'.
  ;; Begin --- Copied comment from dired.el.
  ;; Must first make alist buffer local and set it to nil because
  ;; dired-build-subdir-alist will call dired-clear-alist first
  ;; End --- Copied comment from dired.el.
  (setq-local dired-subdir-alist nil)
  (dired-build-subdir-alist)

  (dired-unadvertise (dired-current-directory))
  (dired-sidebar-update-buffer-name)
  (dired-sidebar-update-state (current-buffer))

  ;; Move setting theme until the end after `dired-sidebar' has set up
  ;; its directory structure.
  ;; https://github.com/jojojames/dired-sidebar/issues/29
  (when (dired-sidebar-can-display-icons)
    (cond
     ((dired-sidebar-using-tui-p)
      (dired-sidebar-setup-tui))
     ((and (eq dired-sidebar-theme 'icons)
           (display-graphic-p)
           (or
            (fboundp 'all-the-icons-dired-mode)
            (autoloadp (symbol-function 'all-the-icons-dired-mode))))
      (with-no-warnings
        (all-the-icons-dired-mode)))
     (:default :no-theme))))

;; User Interface

;;;###autoload
(defun dired-sidebar-toggle-sidebar (&optional dir)
  "Toggle the project explorer window.
Optional argument DIR Use DIR as sidebar root if available.

With universal argument, use current directory."
  (interactive)
  (if (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-hide-sidebar)
    (let* ((old-buffer (dired-sidebar-buffer (selected-frame)))
           (file-to-show (dired-sidebar-get-file-to-show))
           (dir-to-show (or dir
                            (when current-prefix-arg
                              (expand-file-name default-directory))
                            (dired-sidebar-get-dir-to-show)))
           (sidebar-buffer (dired-sidebar-get-or-create-buffer dir-to-show)))
      (dired-sidebar-show-sidebar sidebar-buffer)
      (when (and dired-sidebar-use-one-instance old-buffer (not (eq sidebar-buffer old-buffer)))
        (kill-buffer old-buffer))
      (if (and dired-sidebar-follow-file-at-point-on-toggle-open
               file-to-show)
          (if dired-sidebar-pop-to-sidebar-on-toggle-open
              (dired-sidebar-point-at-file file-to-show dir-to-show)
            (with-selected-window (selected-window)
              (dired-sidebar-point-at-file file-to-show dir-to-show)))
        (when dired-sidebar-pop-to-sidebar-on-toggle-open
          (pop-to-buffer (dired-sidebar-buffer)))))))

(defun dired-sidebar-point-at-file (name root)
  "Try to point at NAME from sidebar.

Keep `dired' pointed at ROOT while cycling directories until
NAME is found in ROOT path.

This is dependent on `dired-subtree-cycle'."
  (let ((sidebar (dired-sidebar-buffer)))
    (pop-to-buffer sidebar)
    (when (and name
               ;; Checking for a private method. *shrug*
               (fboundp 'dired-subtree--is-expanded-p))
      (pop-to-buffer sidebar)
      (goto-char 0)
      (let* ((path root)
             ;; Imagine root is /root/var/ and name is
             ;; /root/var/a/b/c.
             ;; This will return a list of '\("a" "b" "c"\).
             (dirs (when (cadr (split-string name root))
                     (split-string (cadr (split-string name root)) "/"))))
        (dolist (dir dirs)
          ;; Trailing `$' is essential to avoid matching the modification date
          ;; fields of the underlying `ls' process
          (let ((path-regex (concat "^.*[[:space:]]" (regexp-quote dir) "$")))
            (setq path (concat path dir))
            (if (file-regular-p path)
                ;; Try to use `dired-goto-file' to go to the correct
                ;; file. If that fails, just search for the text.
                (let ((default-directory (file-name-directory path)))
                  (unless (dired-goto-file path)
                    (condition-case nil
                        ;; It's hard to get this right so just using a
                        ;; heuristic will get 90% of the way there.
                        ;; Making sure there's a space in front of the name
                        ;; skips matches that contains the name as a
                        ;; substring which is probably good enough...
                        (re-search-forward path-regex)
                      ;; Sometimes `dired' gets out of sync with the file.
                      ;; Refresh the buffer and try the search again.
                      ;; One way to reproduce this:
                      ;; 1. Open file A as buffer B.
                      ;; 2. Delete file A in `dired'.
                      ;; 3. Hide `dired-sidebar'.
                      ;; 4. Save buffer B.
                      ;; 5. Re-open `dired-sidebar'.
                      (error
                       (revert-buffer)
                       (re-search-forward path-regex nil :no-error)))))
              (re-search-forward path-regex)
              ;; Check if subtree has already been expanded.
              ;; Basically, we're using `dired-subtree-cycle' more
              ;; like dired-subtree-expand.
              (when (not (dired-subtree--is-expanded-p))
                ;; This will probably throw an error when trying to expand
                ;; directories that have been collapsed by `dired-collapse'.
                (dired-subtree-cycle))
              (setq path (concat path "/"))))))
      (when dired-sidebar-recenter-cursor-on-follow-file
        (recenter nil))
      (dired-sidebar-redisplay-icons))))

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
    (display-buffer-in-side-window buffer dired-sidebar-display-alist)
    (let ((window (get-buffer-window buffer)))
      (when dired-sidebar-no-delete-other-windows
        (set-window-parameter window 'no-delete-other-windows t))
      (set-window-dedicated-p window t)
      (when dired-sidebar-resize-on-open
        (with-selected-window window
          (let ((window-size-fixed))
            (dired-sidebar-set-width dired-sidebar-width)))))
    (with-current-buffer buffer
      (if (eq major-mode 'dired-sidebar-mode)
          (dired-build-subdir-alist)
        (dired-sidebar-mode)))
    (dired-sidebar-update-state buffer)))

;;;###autoload
(defun dired-sidebar-hide-sidebar ()
  "Hide the sidebar in the selected frame."
  (interactive)
  (dired-sidebar-when-let* ((buffer (dired-sidebar-buffer)))
    (delete-window (get-buffer-window buffer))
    (dired-sidebar-update-state nil)))

;;;###autoload
(defun dired-sidebar-jump-to-sidebar ()
  "Jump to `dired-sidebar' buffer if it is showing.

If it's not showing, act as `dired-sidebar-toggle-sidebar'."
  (interactive)
  (if (dired-sidebar-showing-sidebar-p)
      (select-window
       (get-buffer-window (dired-sidebar-buffer (selected-frame))))
    (call-interactively #'dired-sidebar-toggle-sidebar)))

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
         (let ((buf-name (dired-sidebar-buffer-name
                          dired-file-name)))
           (if (dired-sidebar-buffer-exists-p buf-name)
               (progn
                 (switch-to-buffer buf-name)
                 (dired-sidebar-update-state (current-buffer)))
             (if (and dired-sidebar-use-one-instance (file-directory-p dired-file-name))
                 (find-alternate-file dired-file-name)
               ;; Copied from `dired-find-file'.
               (find-file dired-file-name))
             (dired-sidebar-mode)
             (dired-sidebar-update-state (current-buffer)))))
      ;; Select the sidebar window so that `next-window' is consistent
      ;; in picking the window next to the sidebar.
      ;; This is useful for when `dired-sidebar-find-file' is called
      ;; from a buffer that is not already in the sidebar buffer.
      ;; e.g. A mouse click event.
      (switch-to-buffer (dired-sidebar-buffer))
      (let ((window
             (if select-with-alt-window-function
                 (funcall dired-sidebar-alternate-select-window-function)
               (if dired-sidebar-open-file-in-most-recently-used-window
                   (get-mru-window nil nil t)
                 (next-window)))))
        ;; https://github.com/jojojames/dired-sidebar/issues/55
        (if (or (null window) (window-dedicated-p window))
            (select-window (split-window (next-window) nil 'right))
          (select-window window)))
      (find-file dired-file-name)
      (when dired-sidebar-close-sidebar-on-file-open
        (dired-sidebar-hide-sidebar)))))

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
   (when dired-sidebar-skip-subtree-parent
     (goto-char (point-min)))
   (let* ((dir (dired-current-directory))
          (up (file-name-directory (directory-file-name dir)))
          (up-name (dired-sidebar-buffer-name up)))
     (if (dired-sidebar-buffer-exists-p up-name)
         (progn
           (switch-to-buffer up-name)
           (dired-sidebar-update-state (current-buffer)))
       (if dired-sidebar-use-one-instance
           (find-alternate-file "..")
         (dired-up-directory))
       (dired-sidebar-mode)
       (dired-sidebar-update-state (current-buffer)))
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
               (file-directory-p file)
               (not (string-suffix-p "." file)))
          (dired-subtree-cycle)
        (dired-sidebar-find-file file))))
  (dired-sidebar-redisplay-icons))

;; Helpers

(defun dired-sidebar-buffer-exists-p (buffer-name)
  "Check if a `dired-sidebar' buffer exists for BUFFER-NAME."
  (get-buffer buffer-name))

(defun dired-sidebar-sidebar-root ()
  "Return directory using `projectile', `project' or current directory."
  (if (featurep 'projectile)
      (condition-case nil
          (if (fboundp 'projectile-project-root)
              (or (projectile-project-root) default-directory)
            default-directory)
        (error default-directory))
    ;; Use `project' if `projectile' is not loaded yet.
    ;; `projectile' is a big package and takes a while to load so it's better
    ;; to defer loading it as long as possible (until the user chooses).
    (dired-sidebar-if-let* ((pr (project-current)))
        ;; It can happen, at least in Emacs 27.1, that
        ;; `project-current` give a non-nil result, while
        ;; `project-root` is undefined. Fallback to assuming that the
        ;; directory part of `project-current` is the root. See
        ;; https://github.com/jojojames/dired-sidebar/issues/73 for
        ;; more details.
        (if (fboundp 'project-root)
            (project-root pr)
          (cdr pr))
      default-directory)))

(defun dired-sidebar-buffer-name (dir)
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
  (let ((name (dired-sidebar-buffer-name root)))
    (dired-sidebar-if-let* ((existing-buffer (get-buffer name)))
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
  (when (bound-and-true-p dired-sidebar-face)
    (setq-local buffer-face-mode-face dired-sidebar-face)
    (buffer-face-mode)))

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
   (dired-sidebar-buffer-name (dired-current-directory))))

(defun dired-sidebar-update-state (buffer &optional f)
  "Update current state with BUFFER for sidebar in F or selected frame."
  (let ((frame (or f (selected-frame))))
    (if (assq frame dired-sidebar-alist)
        (setcdr (assq frame dired-sidebar-alist) buffer)
      (push `(,frame . ,buffer) dired-sidebar-alist))))

(defun dired-sidebar-showing-sidebar-p (&optional f)
  "Whether F or selected frame is showing a sidebar.

Check if F or selected frame contains a sidebar and return
corresponding buffer if buffer has a window attached to it.

Return buffer if so."
  (dired-sidebar-when-let* ((buffer (dired-sidebar-buffer f)))
    (get-buffer-window buffer)))

(defun dired-sidebar-buffer (&optional f)
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
  (when (dired-sidebar-showing-sidebar-p)
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
  (dired-sidebar-when-let* ((sidebar (dired-sidebar-buffer)))
    (with-current-buffer sidebar
      (let ((auto-revert-verbose nil))
        (ignore auto-revert-verbose) ;; Make byte compiler happy.
        (revert-buffer)))))

(defun dired-sidebar-follow-file (&rest _)
  "Follow new file.

The root of the sidebar will be determined by `dired-sidebar-get-dir-to-show'
and the file followed is will be determined by `dired-sidebar-get-file-to-show',

both accounting for the currently selected window."
  (when (dired-sidebar-showing-sidebar-p)
    ;; Wrap in `with-selected-window' because we don't want to pop to
    ;; the sidebar buffer.
    ;; We also need to pick the correct selected-window so that
    ;; `dired-sidebar-get-dir-to-show' can get the correct root to change to.
    (with-selected-window (selected-window)
      (let ((root (dired-sidebar-get-dir-to-show)))
        (dired-sidebar-switch-to-dir root)
        (when dired-sidebar-follow-file-at-point-on-toggle-open
          (dired-sidebar-when-let* ((file (dired-sidebar-get-file-to-show)))
            (dired-sidebar-point-at-file file root)))))))

(defun dired-sidebar-default-alternate-select-window ()
  "Default function for `dired-sidebar-alternate-select-window-function'."
  (if (fboundp 'aw-select)
      (aw-select "Select Window")
    (next-window)))

(defun dired-sidebar-get-dir-to-show ()
  "Return the directory `dired-sidebar' should open to."
  (expand-file-name
   (cond
    ((and (derived-mode-p 'magit-mode)
          dired-sidebar-use-magit-integration
          (fboundp 'magit-toplevel))
     (magit-toplevel))
    ((and (eq major-mode 'term-mode)
          dired-sidebar-use-term-integration)
     (dired-sidebar-term-get-pwd))
    ((and (eq major-mode 'dired-mode))
     default-directory)
    ((and (eq major-mode 'ibuffer-mode)
          (fboundp 'ibuffer-current-buffer)
          (ibuffer-current-buffer))
     (let ((buffer-at-point (ibuffer-current-buffer)))
       (if (fboundp 'ibuffer-projectile-root)
           (dired-sidebar-if-let* ((ibuffer-projectile-root
                                    (ibuffer-projectile-root buffer-at-point)))
               (cdr ibuffer-projectile-root)
             (with-current-buffer buffer-at-point
               default-directory))
         (with-current-buffer buffer-at-point
           default-directory))))
    (:default
     (dired-sidebar-sidebar-root)))))

(defun dired-sidebar-get-file-to-show ()
  "Return the file `dired-sidebar' should open to.

This may return nil if there's no suitable file to show."
  (cond
   ((and dired-sidebar-use-magit-integration
         (derived-mode-p 'magit-mode)
         (fboundp 'magit-file-at-point)
         (magit-file-at-point))
    (expand-file-name (magit-file-at-point)))
   ((and (eq major-mode 'dired-mode))
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
  (condition-case nil
      (progn
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
    (error
     default-directory)))

(defun dired-sidebar-subtree-toggle ()
  "Wrapper over `dired-subtree-toggle' that accounts for `all-the-icons-dired'."
  (interactive)
  (dired-subtree-toggle)
  (dired-sidebar-redisplay-icons))

(defun dired-sidebar-redisplay-icons ()
  "Redisplay icon themes unless over TRAMP."
  (when (dired-sidebar-can-display-icons)
    (when (and (eq dired-sidebar-theme 'icons)
               (fboundp 'all-the-icons-dired--refresh))
      ;; Refresh `all-the-icons-dired'.
      (dired-sidebar-revert)
      (all-the-icons-dired--refresh))
    (when (dired-sidebar-using-tui-p)
      (dired-sidebar-tui-update-with-delay))))

(defun dired-sidebar-advice-hide-temporarily (f &rest args)
  "A function meant to be used with advice to temporarily hide itself.

This function hides the sidebar before executing F and then reshows itself
after."
  (if (not (dired-sidebar-showing-sidebar-p))
      (apply f args)
    (let ((sidebar (dired-sidebar-buffer)))
      (dired-sidebar-hide-sidebar)
      (apply f args)
      (dired-sidebar-show-sidebar sidebar))))

(defun dired-sidebar-can-display-icons ()
  "Return whether or not icons should be displayed."
  (and
   (or
    dired-sidebar-display-remote-icons
    (not (file-remote-p default-directory)))
   (cl-every (lambda (mode)
               ;; Check against the default value of `dired-mode-hook' here
               ;; since we made it buffer local earlier.
               (not (memq mode (default-value 'dired-mode-hook))))
             dired-sidebar-block-icon-display-modes)))

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
                  (if (eq dired-sidebar-theme 'vscode)
                      (progn
                        (require 'vscode-icon)
                        (when (fboundp 'vscode-icon-for-file)
                          (insert-image
                           (vscode-icon-for-file filename) " "))
                        (insert " "))
                    (if (file-directory-p filename)
                        (if (dired-subtree--is-expanded-p)
                            (insert (concat collapsible-icon " "))
                          (insert (concat expandable-icon " ")))
                      (insert (if (eq dired-sidebar-theme 'nerd) "  " ""))))))))
          (forward-line 1))))))

(defun dired-sidebar-tui-update-with-delay (&rest _)
  "Update tui interface after a delay."
  (run-with-idle-timer
   dired-sidebar-tui-update-delay nil
   #'dired-sidebar-tui-update))

(defun dired-sidebar-tui-update ()
  "Workhorse function to update tui interface."
  (dired-sidebar-when-let* ((buffer (dired-sidebar-buffer)))
    (with-current-buffer buffer
      (dired-sidebar-revert)
      (when dired-sidebar-recenter-cursor-on-tui-update
        (recenter)))))

(defun dired-sidebar-revert (&rest _)
  "Wrapper around `dired-revert' but saves window position."
  (dired-sidebar-when-let* ((window (get-buffer-window
                                     (dired-sidebar-buffer))))
    (with-selected-window window
      (let ((old-window-start (window-start)))
        (when (dired-sidebar-using-tui-p)
          (dired-sidebar-tui-reset-in-sidebar))
        (dired-revert)
        (set-window-start window old-window-start)))))

(defun dired-sidebar-tui-reset-in-sidebar (&rest _)
  "Runs `dired-sidebar-tui-dired-reset' in current `dired-sidebar' buffer."
  (dired-sidebar-when-let* ((buffer (dired-sidebar-buffer)))
    (with-current-buffer buffer
      (dired-sidebar-tui-dired-reset))))

(defun dired-sidebar-setup-tui ()
  "Sets up text user interface for `dired-sidebar'.

This is used in place of `all-the-icons' to add directory indicators.

e.g. + and -."
  (add-hook 'dired-after-readin-hook
            'dired-sidebar-tui-dired-display :append :local)
  (setq-local dired-subtree-line-prefix dired-sidebar-subtree-line-prefix)
  (dired-build-subdir-alist)
  (dired-sidebar-revert))

(defun dired-sidebar-using-tui-p ()
  "Return t if `dired-sidebar-theme' is using tui code path."
  (or
   (eq dired-sidebar-theme 'ascii)
   (eq dired-sidebar-theme 'nerd)
   (eq dired-sidebar-theme 'vscode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Text User Interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; `wdired' Hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32392

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; `wdired' Hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local dired-sidebar-wdired-tracking-major-mode nil
  "Track current `major-mode' when toggling to `wdired'.")

(defun dired-sidebar-wdired-change-to-dired-mode-advice (f &rest args)
  "Advice for `wdired-change-to-dired-mode'."
  (if (eq dired-sidebar-wdired-tracking-major-mode 'dired-sidebar-mode)
      (dired-sidebar-wdired-change-to-dired-mode)
    (apply f args)))

(defun dired-sidebar-wdired-change-to-dired-mode ()
  "Change the mode back to dired-sidebar.

This is an exact copy of `wdired-change-to-dired-mode' but changes the
`major-mode' to `dired-sidebar-mode' instead of `dired-mode'."
  (let ((inhibit-read-only t))
    (remove-text-properties
     (point-min) (point-max)
     '(front-sticky nil rear-nonsticky nil read-only nil keymap nil)))
  (use-local-map dired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only t)
  (setq major-mode 'dired-sidebar-mode)
  (setq mode-name "Dired-sidebar")
  (dired-advertise)
  (remove-hook 'kill-buffer-hook 'wdired-check-kill-buffer t)
  (set (make-local-variable 'revert-buffer-function) 'dired-sidebar-revert))

(defun dired-sidebar-wdired-change-to-wdired-mode-advice (f &rest args)
  "Forward to `wdired-change-to-wdired-mode'.

`wdired' expected the `major-mode' to be `dired-mode' first.

Track the current `major-mode' and revert to that upon exiting `wdired'."
  (setq dired-sidebar-wdired-tracking-major-mode major-mode)
  (if (eq major-mode 'dired-mode)
      (apply f args)
    (let ((major-mode 'dired-mode))
      (apply f args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; `wdired' Hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dired-sidebar)
;;; dired-sidebar.el ends here
