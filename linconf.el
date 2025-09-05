;;; linconf.el --- Linux Kernel Configuration Editor -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Clark Williams <clrkwllms@kernel.org>

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

(defcustom linconf-kernel-source-path nil
  "Path to kernel source tree for Kconfig validation.
If nil, Kconfig validation will be disabled."
  :type '(choice (const :tag "No validation" nil)
                 (directory :tag "Kernel source directory"))
  :group 'linconf)

(defcustom linconf-kconfig-cache-file "~/.emacs.d/linconf-kconfig-cache.el"
  "File to cache parsed Kconfig data."
  :type 'file
  :group 'linconf)

(defvar linconf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'linconf-toggle-option)
    (define-key map (kbd "C-c C-s") 'linconf-search-option)
    (define-key map (kbd "C-c y") 'linconf-set-y)
    (define-key map (kbd "C-c m") 'linconf-set-m)
    (define-key map (kbd "C-c s") 'linconf-set-string)
    (define-key map (kbd "C-c n") 'linconf-set-number)
    (define-key map (kbd "C-c u") 'linconf-unset-option)
    map)
  "Keymap for `linconf-mode'.")

(defvar linconf-kconfig-options (make-hash-table :test 'equal)
  "Hash table storing Kconfig option definitions.
Keys are option names (without CONFIG_ prefix), values are plists with:
:type - option type (bool, tristate, string, int, hex)
:help - help text
:depends - dependency expression
:select - list of selected options
:default - default value
:range - for int/hex types, (min . max)
:choices - for choice groups, list of options")

(defvar linconf-kconfig-loaded nil
  "Non-nil if Kconfig data has been loaded.")

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

(defun linconf-get-option-name ()
  "Extract CONFIG option name from current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^# CONFIG_\\([A-Z0-9_]+\\) is not set")
      (match-string 1))
     ((looking-at "^CONFIG_\\([A-Z0-9_]+\\)=")
      (match-string 1))
     (t nil))))

(defun linconf-set-option (option value)
  "Set CONFIG option to value, replacing current line."
  (when option
    (delete-region (line-beginning-position) (line-end-position))
    (if (null value)
        (insert (format "# CONFIG_%s is not set" option))
      (insert (format "CONFIG_%s=%s" option value)))))

(defun linconf-set-y ()
  "Set configuration option to 'y' (built-in)."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (linconf-set-option option "y")
      (message "No configuration option found on this line"))))

(defun linconf-set-m ()
  "Set configuration option to 'm' (module)."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (linconf-set-option option "m")
      (message "No configuration option found on this line"))))

(defun linconf-set-string ()
  "Set configuration option to a string value."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (let ((value (read-string (format "Set CONFIG_%s to string: " option))))
          (linconf-set-option option (format "\"%s\"" value)))
      (message "No configuration option found on this line"))))

(defun linconf-set-number ()
  "Set configuration option to a numeric value."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (let ((value (read-number (format "Set CONFIG_%s to number: " option))))
          (linconf-set-option option (number-to-string value)))
      (message "No configuration option found on this line"))))

(defun linconf-unset-option ()
  "Unset configuration option (set to 'is not set')."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (linconf-set-option option nil)
      (message "No configuration option found on this line"))))

(defun linconf-parse-source-directives (file kernel-root)
  "Parse source directives from Kconfig FILE, returning list of referenced files.
KERNEL-ROOT is the kernel source tree root for resolving relative paths."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((sources '())
            (file-dir (file-name-directory file)))
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*source\\s-+\"\\([^\"]+\\)\"" nil t)
          (let* ((source-path (match-string 1))
                 (full-path (cond
                            ;; Absolute path
                            ((file-name-absolute-p source-path)
                             source-path)
                            ;; Relative to kernel root
                            ((string-match "^\\$" source-path)
                             (expand-file-name (substring source-path 1) kernel-root))
                            ;; Relative to current file
                            (t
                             (expand-file-name source-path file-dir)))))
            (when (file-readable-p full-path)
              (push full-path sources))))
        (nreverse sources))))

(defun linconf-collect-kconfig-files (kernel-root)
  "Collect all Kconfig files by following source directives from KERNEL-ROOT/Kconfig."
  (let ((visited (make-hash-table :test 'equal))
        (files '())
        (queue (list (expand-file-name "Kconfig" kernel-root))))
    (while queue
      (let ((current-file (pop queue)))
        (unless (gethash current-file visited)
          (puthash current-file t visited)
          (when (file-readable-p current-file)
            (push current-file files)
            (let ((sources (linconf-parse-source-directives current-file kernel-root)))
              (setq queue (append queue sources)))))))
    (nreverse files)))

(defun linconf-parse-kconfig-option (lines)
  "Parse a single config option from LINES, return (name . plist)."
  (let ((name nil)
        (type nil)
        (help nil)
        (depends nil)
        (select nil)
        (default nil)
        (range nil)
        (choices nil)
        (in-help nil))
    (dolist (line lines)
      (cond
       ((string-match "^config \\([A-Z0-9_]+\\)" line)
        (setq name (match-string 1 line)))
       ((string-match "^\\s-+\\(bool\\|tristate\\|string\\|int\\|hex\\)" line)
        (setq type (intern (match-string 1 line))))
       ((string-match "^\\s-+depends on \\(.+\\)" line)
        (setq depends (match-string 1 line)))
       ((string-match "^\\s-+select \\([A-Z0-9_]+\\)" line)
        (push (match-string 1 line) select))
       ((string-match "^\\s-+default \\(.+\\)" line)
        (setq default (match-string 1 line)))
       ((string-match "^\\s-+range \\([0-9]+\\) \\([0-9]+\\)" line)
        (setq range (cons (string-to-number (match-string 1 line))
                          (string-to-number (match-string 2 line)))))
       ((string-match "^\\s-+help" line)
        (setq in-help t))
       ((and in-help (string-match "^\\s-+\\(.+\\)" line))
        (setq help (if help
                       (concat help "\\n" (match-string 1 line))
                     (match-string 1 line))))
       ((string-match "^choice" line)
        (setq type 'choice))
       ((and (eq type 'choice) (string-match "^\\s-+config \\([A-Z0-9_]+\\)" line))
        (push (match-string 1 line) choices))))
    (when name
      (cons name (list :type type
                       :help help
                       :depends depends
                       :select (nreverse select)
                       :default default
                       :range range
                       :choices (nreverse choices))))))

(defun linconf-parse-kconfig-file (file)
  "Parse a single Kconfig file and return list of (name . plist) pairs."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (split-string (buffer-string) "\\n"))
            (options '())
            (current-config '())
            (in-config nil))
        (dolist (line lines)
          (cond
           ((string-match "^config " line)
            (when current-config
              (let ((option (linconf-parse-kconfig-option (nreverse current-config))))
                (when option
                  (push option options))))
            (setq current-config (list line)
                  in-config t))
           ((and in-config (string-match "^\\s-+" line))
            (push line current-config))
           ((and in-config (not (string-match "^\\s-*$" line)))
            (when current-config
              (let ((option (linconf-parse-kconfig-option (nreverse current-config))))
                (when option
                  (push option options))))
            (setq current-config nil
                  in-config nil))))
        (when current-config
          (let ((option (linconf-parse-kconfig-option (nreverse current-config))))
            (when option
              (push option options))))
        (nreverse options)))))

(defun linconf-load-kconfig-data ()
  "Load and parse all Kconfig files from kernel source tree."
  (interactive)
  (when linconf-kernel-source-path
    (unless (file-directory-p linconf-kernel-source-path)
      (error "Kernel source path does not exist: %s" linconf-kernel-source-path))
    (message "Loading Kconfig data from %s..." linconf-kernel-source-path)
    (clrhash linconf-kconfig-options)
    (let ((kconfig-files (linconf-collect-kconfig-files linconf-kernel-source-path))
          (total-options 0))
      (dolist (file kconfig-files)
        (let ((options (linconf-parse-kconfig-file file)))
          (dolist (option options)
            (puthash (car option) (cdr option) linconf-kconfig-options)
            (setq total-options (1+ total-options)))))
      (setq linconf-kconfig-loaded t)
      (message "Loaded %d options from %d Kconfig files"
               total-options (length kconfig-files)))))

;;;###autoload
(define-derived-mode linconf-mode fundamental-mode "linconf"
  "Major mode for editing Linux kernel configuration files."
  (setq font-lock-defaults '(linconf-font-lock-keywords))
  (setq comment-start "#")
  (setq comment-end "")
  (font-lock-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.config\\'" . linconf-mode))

(provide 'linconf)

;;; linconf.el ends here
