;;; test-helper.el --- Helpers for dired-sidebar-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Created: 20 November 2017

;;; Commentary:

;; Utilities for running dired-sidebar tests.

;;; Code:
(require 'ert)

(add-to-list 'load-path
             (expand-file-name
              ".." (file-name-directory
                    (or load-file-name buffer-file-name))))
(require 'dired-sidebar)

;;; test-helper.el ends here
