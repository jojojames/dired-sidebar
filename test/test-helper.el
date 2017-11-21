;;; test-helper.el --- Helpers for dired-sidebar-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Created: 20 November 2017

;;; Commentary:

;; Utilities for running dired-sidebar tests.

;;; Code:
(require 'ert)

;; FIXME: Adding `f' as a dependency just for this line.
(require 'f)
(let ((dired-sidebar-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path dired-sidebar-dir))
(require 'dired-sidebar)

;;; test-helper.el ends here
