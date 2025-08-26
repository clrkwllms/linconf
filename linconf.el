;;; linconf.el --- Linux Kernel Configuration Editor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Generated with Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: tools, kernel, configuration
;; URL: 

;;; Commentary:

;; This package provides an Emacs interface for editing Linux kernel
;; configuration files (.config files). It offers syntax highlighting,
;; navigation, and editing capabilities for kernel configuration options.

;;; Code:

(defgroup linconf nil
  "Linux kernel configuration editor."
  :group 'tools
  :prefix "linconf-")

(defcustom linconf-config-file ".config"
  "Default kernel configuration file name."
  :type 'string
  :group 'linconf)

(defvar linconf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'linconf-toggle-option)
    (define-key map (kbd "C-c C-s") 'linconf-search-option)
    map)
  "Keymap for `linconf-mode'.")

(defvar linconf-font-lock-keywords
  '(("^# CONFIG_\\([A-Z0-9_]+\\) is not set" . font-lock-comment-face)
    ("^CONFIG_\\([A-Z0-9_]+\\)=" . font-lock-variable-name-face)
    ("=\\(y\\|n\\|m\\)" . font-lock-constant-face)
    ("=\\([0-9]+\\)" . font-lock-constant-face)
    ("=\"\\([^\"]*\\)\"" . font-lock-string-face)
    ("^#.*$" . font-lock-comment-face))
  "Font lock keywords for linconf mode.")

(defun linconf-toggle-option ()
  "Toggle a configuration option between enabled/disabled states."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Handle "# CONFIG_FOO is not set" -> "CONFIG_FOO=y"
     ((looking-at "^# CONFIG_\\([A-Z0-9_]+\\) is not set")
      (let ((option (match-string 1)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format "CONFIG_%s=y" option))))
     ;; Handle "CONFIG_FOO=y" -> "# CONFIG_FOO is not set"
     ((looking-at "^CONFIG_\\([A-Z0-9_]+\\)=y")
      (let ((option (match-string 1)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format "# CONFIG_%s is not set" option))))
     ;; Handle "CONFIG_FOO=m" -> "CONFIG_FOO=y"
     ((looking-at "^CONFIG_\\([A-Z0-9_]+\\)=m")
      (let ((option (match-string 1)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format "CONFIG_%s=y" option))))
     (t (message "No configuration option found on this line")))))

(defun linconf-search-option ()
  "Search for a configuration option."
  (interactive)
  (let ((option (read-string "Search for CONFIG option: ")))
    (goto-char (point-min))
    (if (re-search-forward (format "CONFIG_%s" option) nil t)
        (beginning-of-line)
      (message "Option CONFIG_%s not found" option))))

;;;###autoload
(define-derived-mode linconf-mode fundamental-mode "LinConf"
  "Major mode for editing Linux kernel configuration files."
  (setq font-lock-defaults '(linconf-font-lock-keywords))
  (setq comment-start "#")
  (setq comment-end "")
  (font-lock-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.config\\'" . linconf-mode))

(provide 'linconf)

;;; linconf.el ends here