;;; dired-sidebar-test.el --- Tests for dired-sidebar -*- lexical-binding: t -*-

(require 'dired-sidebar)
(require 'ert)
(require 'f)
(eval-when-compile (require 'subr-x))

(defvar test-data-dir)
(defvar test-data-dir-basic)

;; make runs the test starting from a parent directory.
(setq test-data-dir (f-expand (if (string-match-p "/test/" default-directory)
                                  "./data"
                                "./test/data")))
(setq test-data-dir-basic (f-join test-data-dir "basic"))

;; https://github.com/rejeep/ert-runner.el/issues/24
(with-no-warnings
  (defun ert-runner-message (format &rest args)
    "..."
    (let ((message (apply #'format format args)))
      (if ert-runner-output-file
          (f-append-text message 'utf-8 ert-runner-output-file)
        (princ message t)))))

(ert-deftest dired-sidebar-blank-test ()
  "Blank test."
  (should (equal 0 0)))

(ert-deftest dired-sidebar-toggle-sidebar-opens-sidebar-when-hidden ()
  "Test `dired-sidebar-toggle-sidebar' opens sidebar if sidebar is not
already showing."
  (let* ((default-directory test-data-dir-basic)
         (A-buffer (find-file-noselect
                    (f-join default-directory "A") t)))
    (with-current-buffer A-buffer
      (call-interactively 'dired-sidebar-toggle-sidebar)
      (should (dired-sidebar-buffer)))
    (kill-buffer A-buffer)
    (dired-sidebar-hide-sidebar)))

(ert-deftest dired-sidebar-hide-sidebar-hides-sidebar ()
  "Test `dired-sidebar-hide-sidebar' actually hides sidebar."
  (let* ((default-directory test-data-dir-basic)
         (A-buffer (find-file-noselect
                    (f-join default-directory "A") t)))
    (with-current-buffer A-buffer
      (call-interactively 'dired-sidebar-toggle-sidebar)
      (should (equal (current-buffer)
                     (dired-sidebar-buffer))))
    (call-interactively 'dired-sidebar-hide-sidebar)
    (should (equal (dired-sidebar-buffer) nil))
    (kill-buffer A-buffer)))

(ert-deftest dired-sidebar-pop-to-sidebar-on-toggle-open ()
  "Test behavior around `dired-sidebar-pop-to-sidebar-on-toggle-open'."
  (let* ((default-directory test-data-dir-basic)
         (dired-sidebar-pop-to-sidebar-on-toggle-open t)
         (A-buffer (find-file-noselect
                    (f-join default-directory "A") t)))
    (with-current-buffer A-buffer
      (call-interactively 'dired-sidebar-toggle-sidebar)
      (should (equal (current-buffer)
                     (dired-sidebar-buffer))))
    (call-interactively 'dired-sidebar-hide-sidebar)
    (with-current-buffer A-buffer
      (let ((dired-sidebar-pop-to-sidebar-on-toggle-open nil))
        (call-interactively 'dired-sidebar-toggle-sidebar)
        (should (equal (current-buffer) A-buffer))))
    (kill-buffer A-buffer)
    (dired-sidebar-hide-sidebar)))

(ert-deftest dired-sidebar-hide-sidebar-if-hidden-does-nothing ()
  "Don't do anything if sidebar is already hidden when
`dired-sidebar-hide-sidebar' is called."
  (let ((default-directory test-data-dir-basic)
        (A-buffer (find-file-noselect
                   (f-join default-directory "A") t)))
    (with-current-buffer A-buffer
      (let ((current-window-count (length (window-list))))
        (call-interactively 'dired-sidebar-hide-sidebar)
        (should (equal (length (window-list)) current-window-count))))
    (kill-buffer A-buffer)))

(ert-deftest dired-sidebar-follows-file-on-open ()
  "Test behavior when setting
`dired-sidebar-follow-file-at-point-on-toggle-open'."
  (let* ((default-directory test-data-dir-basic)
         (dired-sidebar-follow-file-at-point-on-toggle-open t)
         (A-buffer (find-file-noselect
                    (f-join default-directory "A") t)))
    (with-current-buffer A-buffer
      (call-interactively 'dired-sidebar-toggle-sidebar)
      (should (string-suffix-p "A" (string-trim (thing-at-point 'line t))))
      (forward-line 1))

    (dired-sidebar-hide-sidebar)
    (with-current-buffer A-buffer
      (let ((dired-sidebar-follow-file-at-point-on-toggle-open nil))
        (call-interactively 'dired-sidebar-toggle-sidebar)
        (message (string-trim (thing-at-point 'line t)))
        (should (not (string-suffix-p "A" (string-trim (thing-at-point 'line t)))))))
    (kill-buffer A-buffer)
    (dired-sidebar-hide-sidebar)))

(ert-deftest dired-sidebar-sidebar-killed-sidebar-buffer-in-frame-returns-nil ()
  "Test that `dired-sidebar-buffer' returns nil if the buffer has been killed."
  (let* ((default-directory test-data-dir-basic)
         (A-buffer (find-file-noselect
                    (f-join default-directory "A") t)))
    (with-current-buffer A-buffer
      (call-interactively 'dired-sidebar-toggle-sidebar)
      (let ((sidebar (dired-sidebar-buffer)))
        (should sidebar)
        (kill-buffer sidebar)
        (should (not (dired-sidebar-buffer)))))
    (kill-buffer A-buffer)))

(ert-deftest dired-sidebar-get-file-to-show-returns-nil-for-unsaved-buffers ()
  "Test that `dired-sidebar-get-file-to-show' doesn't return a file when the
buffer hasn't been saved yet."
  (let* ((default-directory test-data-dir-basic)
         (unsaved-buffer
          (find-file-noselect
           (f-join default-directory "new-unsaved-file") t)))
    (with-current-buffer unsaved-buffer
      (should (not (dired-sidebar-get-file-to-show))))
    (kill-buffer unsaved-buffer)))

;;; dired-sidebar-test.el ends here
