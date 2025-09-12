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
    (define-key map (kbd "C-c d") 'linconf-show-dependency-info)
    (define-key map (kbd "C-c C-d") 'linconf-simulate-config-change)
    (define-key map (kbd "C-c l") 'linconf-load-config-file)
    (define-key map (kbd "C-c w") 'linconf-save-config)
    (define-key map (kbd "C-c r") 'linconf-reload-config)
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

(defvar linconf-main-menu-title nil
  "Main menu title from mainmenu directive.")

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
        (progn
          (linconf-set-option option "y")
          ;; Update internal config state and apply select chains
          (linconf-set-config-with-chains option t))
      (message "No configuration option found on this line"))))

(defun linconf-set-m ()
  "Set configuration option to 'm' (module)."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (progn
          (linconf-set-option option "m")
          ;; Update internal config state and apply select chains
          (linconf-set-config-with-chains option 'module))
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
        (progn
          (linconf-set-option option nil)
          ;; Update internal config state (this may trigger warnings about reverse deps)
          (linconf-set-config-with-chains option nil))
      (message "No configuration option found on this line"))))

(defun linconf-get-main-menu-title ()
  "Get the main menu title if available."
  linconf-main-menu-title)

(defvar linconf-config-values (make-hash-table :test 'equal)
  "Hash table storing current configuration values for condition evaluation.")

(defun linconf-evaluate-condition (condition)
  "Evaluate a Kconfig condition expression.
CONDITION is a string like 'X86 && PCI' or '!ARM || (X86_64 && SMP)'.
Returns t if condition is true, nil if false, 'unknown if cannot determine."
  (when (and condition (> (length (string-trim condition)) 0))
    (let ((expr (string-trim condition)))
      ;; Handle simple cases first
      (cond
       ;; Single config option
       ((string-match "^!?[A-Z0-9_]+$" expr)
        (let* ((negated (string-prefix-p "!" expr))
               (option (if negated (substring expr 1) expr))
               (value (gethash option linconf-config-values 'unknown)))
          (cond
           ((eq value 'unknown) 'unknown)
           ((eq value t) (not negated))
           ((eq value nil) negated)
           (t (if negated nil t)))))
       ;; Complex expression - use recursive parser
       (t (linconf-parse-condition-expr expr))))))

(defun linconf-parse-condition-expr (expr)
  "Parse and evaluate complex condition expression EXPR."
  (let ((tokens (linconf-tokenize-condition expr)))
    (linconf-eval-condition-tokens tokens)))

(defun linconf-tokenize-condition (expr)
  "Tokenize condition expression EXPR into list of tokens."
  (let ((tokens '())
        (i 0)
        (len (length expr))
        current-token)
    (while (< i len)
      (let ((char (aref expr i)))
        (cond
         ;; Skip whitespace
         ((memq char '(?\s ?\t))
          (when current-token
            (push (nreverse current-token) tokens)
            (setq current-token nil))
          (setq i (1+ i)))
         ;; Handle operators
         ((memq char '(?& ?|))
          (when current-token
            (push (nreverse current-token) tokens)
            (setq current-token nil))
          (if (and (< (1+ i) len) (eq (aref expr (1+ i)) char))
              (progn
                (push (if (eq char ?&) "&&" "||") tokens)
                (setq i (+ i 2)))
            (push (string char) tokens)
            (setq i (1+ i))))
         ;; Handle parentheses and negation
         ((memq char '(?\( ?\) ?!))
          (when current-token
            (push (nreverse current-token) tokens)
            (setq current-token nil))
          (push (string char) tokens)
          (setq i (1+ i)))
         ;; Build tokens
         (t
          (push char current-token)
          (setq i (1+ i))))))
    (when current-token
      (push (nreverse current-token) tokens))
    (mapcar (lambda (token) (if (listp token) (apply #'string token) token)) 
            (nreverse tokens))))

(defun linconf-eval-condition-tokens (tokens)
  "Evaluate tokenized condition expression."
  ;; Simple evaluation - for now, return 'unknown for complex expressions
  ;; This is a placeholder for a full expression evaluator
  (if (= (length tokens) 1)
      (let ((token (car tokens)))
        (cond
         ((string-match "^!?[A-Z0-9_]+$" token)
          (linconf-evaluate-condition token))
         (t 'unknown)))
    'unknown))

(defun linconf-set-config-value (option value)
  "Set configuration OPTION to VALUE for condition evaluation."
  (puthash option value linconf-config-values))

(defun linconf-get-config-value (option)
  "Get current value of configuration OPTION."
  (gethash option linconf-config-values 'unknown))

(defvar linconf-select-chains (make-hash-table :test 'equal)
  "Hash table mapping config options to their select statements.
Key: option name, Value: list of (selected-option . condition) pairs.")

(defun linconf-add-select-statement (selector selected &optional condition)
  "Add a select statement: SELECTOR selects SELECTED [if CONDITION]."
  (let ((current-selects (gethash selector linconf-select-chains '())))
    (push (cons selected condition) current-selects)
    (puthash selector current-selects linconf-select-chains)))

(defun linconf-get-select-statements (option)
  "Get all options that OPTION selects."
  (gethash option linconf-select-chains '()))

(defun linconf-resolve-select-chains (option new-value &optional visited)
  "Resolve select chains when OPTION is set to NEW-VALUE.
VISITED is used to prevent infinite recursion.
Returns list of (option . value) pairs that should be set."
  (let ((visited (or visited (make-hash-table :test 'equal)))
        (changes '()))
    
    ;; Prevent infinite recursion  
    (unless (gethash option visited)
    (puthash option t visited)
    
    ;; Only process if the option is being enabled (y or m)
    (when (and new-value (not (eq new-value nil)))
      (let ((selects (linconf-get-select-statements option)))
        (dolist (select-pair selects)
          (let* ((selected-option (car select-pair))
                 (condition (cdr select-pair))
                 (should-select (if condition
                                  (let ((result (linconf-evaluate-condition condition)))
                                    (if (eq result 'unknown) t result))
                                t)))
            (when should-select
              ;; Add this selection to changes
              (push (cons selected-option t) changes)
              
              ;; Recursively resolve any chains from the selected option
              (let ((sub-changes (linconf-resolve-select-chains selected-option t visited)))
                (setq changes (append sub-changes changes)))))))
    
    changes))

(defun linconf-apply-select-chains (option new-value)
  "Apply select chain resolution when setting OPTION to NEW-VALUE."
  (let ((changes (linconf-resolve-select-chains option new-value)))
    (dolist (change changes)
      (let ((selected-option (car change))
            (selected-value (cdr change)))
        (when (not (eq (linconf-get-config-value selected-option) selected-value))
          (message "Auto-selecting %s=%s (required by %s)" 
                   selected-option 
                   (if selected-value "y" "n") 
                   option)
          (linconf-set-config-value selected-option selected-value))))))

(defun linconf-find-reverse-selects (option)
  "Find all options that select the given OPTION."
  (let ((reverse-selects '()))
    (maphash (lambda (selector select-list)
               (dolist (select-pair select-list)
                 (when (equal (car select-pair) option)
                   (push (cons selector (cdr select-pair)) reverse-selects))))
             linconf-select-chains)
    reverse-selects))

(defun linconf-set-config-with-chains (option value)
  "Set configuration OPTION to VALUE and resolve select chains."
  (let ((old-value (linconf-get-config-value option)))
    (unless (eq old-value value)
      ;; Set the new value
      (linconf-set-config-value option value)
      
      ;; Apply select chains if enabling
      (when value
        (linconf-apply-select-chains option value))
      
      ;; Check reverse dependencies if disabling
      (when (and (not value) (not (eq old-value 'unknown)))
        (let ((reverse-selects (linconf-find-reverse-selects option)))
          (when reverse-selects
            (message "Warning: %s is required by: %s" 
                     option
                     (mapconcat (lambda (pair) (car pair)) reverse-selects ", "))))))))

(defun linconf-get-dependency-info (option)
  "Get comprehensive dependency information for OPTION.
Returns plist with :depends, :selects, :selected-by, :conflicts."
  (let* ((option-info (gethash option linconf-kconfig-options))
         (depends (when option-info (plist-get option-info :depends)))
         (selects (mapcar #'car (linconf-get-select-statements option)))
         (selected-by (mapcar #'car (linconf-find-reverse-selects option)))
         (conflicts '())) ; TODO: Implement conflict detection
    
    (list :depends depends
          :selects selects  
          :selected-by selected-by
          :conflicts conflicts)))

(defun linconf-show-dependency-info (option)
  "Show dependency information for OPTION in a user-friendly format."
  (interactive (list (read-string "Config option: " (linconf-get-option-name))))
  (let ((info (linconf-get-dependency-info option)))
    (message "Dependencies for %s:" option)
    (when (plist-get info :depends)
      (message "  Depends on: %s" (plist-get info :depends)))
    (when (plist-get info :selects)
      (message "  Selects: %s" (mapconcat #'identity (plist-get info :selects) ", ")))
    (when (plist-get info :selected-by)
      (message "  Selected by: %s" (mapconcat #'identity (plist-get info :selected-by) ", ")))
    (unless (or (plist-get info :depends) (plist-get info :selects) (plist-get info :selected-by))
      (message "  No dependencies found"))))

(defun linconf-simulate-config-change (option value)
  "Simulate setting OPTION to VALUE and show what would be auto-selected.
Returns list of changes that would be made."
  (let ((changes (linconf-resolve-select-chains option value)))
    (when changes
      (message "Setting %s=%s would auto-select:" option (if value "y" "n"))
      (dolist (change changes)
        (message "  %s=%s" (car change) (if (cdr change) "y" "n"))))
    changes))

;; .config file handling functions

(defun linconf-parse-config-file (config-file)
  "Parse a .config file and return hash table of option values.
Returns hash table with option names as keys and values as:
- t for =y (enabled)  
- 'module for =m (module)
- nil for 'is not set' (disabled)
- string for quoted values
- number for numeric values"
  (let ((config-values (make-hash-table :test 'equal)))
    (when (file-readable-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties 
                      (line-beginning-position) 
                      (line-end-position))))
            (cond
             ;; Disabled option: # CONFIG_OPTION is not set
             ((string-match "^# CONFIG_\\([A-Z0-9_]+\\) is not set" line)
              (puthash (match-string 1 line) nil config-values))
             
             ;; Enabled boolean: CONFIG_OPTION=y
             ((string-match "^CONFIG_\\([A-Z0-9_]+\\)=y$" line)
              (puthash (match-string 1 line) t config-values))
             
             ;; Module: CONFIG_OPTION=m  
             ((string-match "^CONFIG_\\([A-Z0-9_]+\\)=m$" line)
              (puthash (match-string 1 line) 'module config-values))
             
             ;; String value: CONFIG_OPTION="value"
             ((string-match "^CONFIG_\\([A-Z0-9_]+\\)=\"\\([^\"]*\\)\"$" line)
              (puthash (match-string 1 line) (match-string 2 line) config-values))
             
             ;; Numeric value: CONFIG_OPTION=123
             ((string-match "^CONFIG_\\([A-Z0-9_]+\\)=\\([0-9]+\\)$" line)
              (puthash (match-string 1 line) 
                      (string-to-number (match-string 2 line)) 
                      config-values))
             
             ;; Hex value: CONFIG_OPTION=0x123
             ((string-match "^CONFIG_\\([A-Z0-9_]+\\)=0x\\([0-9a-fA-F]+\\)$" line)
              (puthash (match-string 1 line)
                      (string-to-number (match-string 2 line) 16)
                      config-values))))
          (forward-line 1))))
    config-values))

(defun linconf-load-config-file (&optional config-file)
  "Load .config file and populate internal config values.
CONFIG-FILE defaults to linconf-config-file in the kernel source path."
  (interactive)
  (let* ((kernel-root (or linconf-kernel-source-path default-directory))
         (config-path (or config-file 
                         (expand-file-name linconf-config-file kernel-root))))
    (if (file-readable-p config-path)
        (progn
          (message "Loading config from %s..." config-path)
          (let ((config-hash (linconf-parse-config-file config-path))
                (loaded-count 0))
            ;; Clear existing values
            (clrhash linconf-config-values)
            
            ;; Copy parsed values to internal hash table
            (maphash (lambda (key value)
                       (puthash key value linconf-config-values)
                       (setq loaded-count (1+ loaded-count)))
                     config-hash)
            
            (message "Loaded %d configuration values from %s" 
                     loaded-count config-path)
            config-path))
      (message "Config file not found: %s" config-path))))

(defun linconf-config-value-to-string (value)
  "Convert internal config VALUE to .config file format."
  (cond
   ((eq value t) "y")
   ((eq value 'module) "m") 
   ((eq value nil) nil) ; Will be handled as "is not set"
   ((stringp value) (format "\"%s\"" value))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun linconf-write-config-file (&optional config-file)
  "Write current config values to .config file.
CONFIG-FILE defaults to linconf-config-file in the kernel source path."
  (interactive)
  (let* ((kernel-root (or linconf-kernel-source-path default-directory))
         (config-path (or config-file 
                         (expand-file-name linconf-config-file kernel-root)))
         (written-count 0)
         (arch (linconf-detect-architecture kernel-root))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    
    (message "Writing config to %s..." config-path)
    
    (with-temp-buffer
      ;; Write header with architecture and timestamp
      (insert (format "# %s\n" arch))
      (insert (format "# Automatically generated file; DO NOT EDIT.\n"))
      (insert (format "# Generated on %s by linconf.el\n" timestamp))
      (insert "#\n")
      
      ;; Collect all known options from both Kconfig definitions and current values
      (let ((all-options (make-hash-table :test 'equal)))
        
        ;; Add all known Kconfig options (even if not currently set)
        (maphash (lambda (option-name option-info)
                   (puthash option-name 'from-kconfig all-options))
                 linconf-kconfig-options)
        
        ;; Add all current config values
        (maphash (lambda (option-name value)
                   (puthash option-name value all-options))
                 linconf-config-values)
        
        ;; Sort options alphabetically
        (let ((sorted-options (sort (hash-table-keys all-options) #'string<)))
          
          (dolist (option sorted-options)
            (let* ((current-value (gethash option linconf-config-values 'unknown))
                   (kconfig-info (gethash option linconf-kconfig-options))
                   (option-type (when kconfig-info (plist-get kconfig-info :type))))
              
              ;; Only write options that have been explicitly set or are known from Kconfig
              (when (and (not (eq current-value 'unknown)) (not (eq current-value 'from-kconfig)))
                (let ((value-string (linconf-config-value-to-string current-value)))
                  (if value-string
                      ;; Option is enabled/set
                      (progn
                        (insert (format "CONFIG_%s=%s\n" option value-string))
                        (setq written-count (1+ written-count)))
                    ;; Option is disabled  
                    (when (or (eq option-type 'bool) 
                             (eq option-type 'tristate)
                             (eq option-type 'menuconfig))
                      (insert (format "# CONFIG_%s is not set\n" option))
                      (setq written-count (1+ written-count)))))))))
      
      ;; Write to file
      (write-region (point-min) (point-max) config-path)
      (message "Wrote %d configuration options to %s" written-count config-path)
      config-path)))

(defun linconf-save-config ()
  "Save current configuration to .config file.
Interactive wrapper around linconf-write-config-file."
  (interactive)
  (if linconf-kernel-source-path
      (linconf-write-config-file)
    (let ((config-file (read-file-name "Save config to: " default-directory ".config")))
      (linconf-write-config-file config-file))))

(defun linconf-sync-config-with-kconfig ()
  "Sync current .config values with Kconfig option definitions.
Sets sensible defaults for options that are defined in Kconfig but not in .config."
  (let ((updated-count 0))
    (maphash (lambda (option-name option-info)
               (let ((current-value (gethash option-name linconf-config-values 'unknown))
                     (option-type (plist-get option-info :type))
                     (option-default (plist-get option-info :default)))
                 
                 ;; Set default values for options not in .config
                 (when (eq current-value 'unknown)
                   (let ((default-value 
                          (cond
                           ;; Use explicit default if available
                           ((and option-default (stringp option-default))
                            (cond
                             ((string= option-default "y") t)
                             ((string= option-default "m") 'module)
                             ((string= option-default "n") nil)
                             ((string-match "^\"\\(.*\\)\"$" option-default) (match-string 1 option-default))
                             ((string-match "^[0-9]+$" option-default) (string-to-number option-default))
                             (t option-default)))
                           ;; Default values by type
                           ((memq option-type '(bool menuconfig)) nil) ; Default to disabled
                           ((eq option-type 'tristate) nil) ; Default to disabled
                           ((eq option-type 'string) "")
                           ((memq option-type '(int hex)) 0)
                           (t 'unknown))))
                     
                     (when (not (eq default-value 'unknown))
                       (puthash option-name default-value linconf-config-values)
                       (setq updated-count (1+ updated-count))))))
             linconf-kconfig-options)
    
    (when (> updated-count 0)
      (message "Set defaults for %d undefined config options" updated-count))
    updated-count)))

(defun linconf-reload-config ()
  "Reload both Kconfig definitions and .config values."
  (interactive)
  (when (and linconf-kernel-source-path (file-directory-p linconf-kernel-source-path))
    ;; Reload Kconfig definitions
    (linconf-load-kconfig-data)
    
    ;; Load current .config if it exists
    (let ((config-path (expand-file-name linconf-config-file linconf-kernel-source-path)))
      (if (file-readable-p config-path)
          (progn
            (linconf-load-config-file config-path)
            ;; Sync with Kconfig to set defaults for missing options
            (linconf-sync-config-with-kconfig))
        (message "No .config file found at %s" config-path)))))

(defun linconf-detect-architecture (kernel-root)
  "Detect the target architecture from kernel source tree."
  ;; Try to detect architecture from common indicators
  (let ((arch-path (expand-file-name "arch" kernel-root)))
    (if (file-directory-p arch-path)
        (let ((arch-dirs (directory-files arch-path nil "^[^.]")))
          (cond
           ((member "x86" arch-dirs) "x86")
           ((member "arm64" arch-dirs) "arm64")
           ((member "arm" arch-dirs) "arm")
           ((member "powerpc" arch-dirs) "powerpc")
           ((member "riscv" arch-dirs) "riscv")
           ((member "s390" arch-dirs) "s390")
           (t "x86"))) ;; Default fallback
      "x86"))) ;; Default when no arch directory exists

(defun linconf-get-kernel-build-vars (kernel-root)
  "Get kernel build variables from the source tree context."
  (let ((arch (linconf-detect-architecture kernel-root)))
    `(("SRCARCH" . ,arch)
      ("ARCH" . ,arch) 
      ("HEADER_ARCH" . ,(cond
                         ((equal arch "x86") "x86")
                         ((equal arch "arm64") "arm64")
                         ((equal arch "arm") "arm")
                         ((equal arch "powerpc") "powerpc")
                         ((equal arch "riscv") "riscv")
                         ((equal arch "s390") "s390")
                         (t arch)))
      ("KERNELVERSION" . "6.x")
      ("srctree" . "."))))

(defun linconf-expand-kconfig-variables (path kernel-root)
  "Expand Kconfig variables in PATH using KERNEL-ROOT context.
Supports $(VAR), ${VAR}, and $VAR patterns."
  (let ((expanded-path path)
        (build-vars (linconf-get-kernel-build-vars kernel-root)))
    
    ;; Handle $(VAR) patterns - case insensitive
    (dolist (var-pair build-vars)
      (let ((var-name (car var-pair))
            (var-value (cdr var-pair)))
        (setq expanded-path (replace-regexp-in-string 
                            (format "\\$(%s)" (regexp-quote var-name))
                            var-value expanded-path t))
        (setq expanded-path (replace-regexp-in-string 
                            (format "\\${%s}" (regexp-quote var-name))
                            var-value expanded-path t))
        (setq expanded-path (replace-regexp-in-string 
                            (format "\\$%s\\b" (regexp-quote var-name))
                            var-value expanded-path t))))
    
    ;; Handle some build-time expressions
    (setq expanded-path (replace-regexp-in-string "\\$(srctree)/" "" expanded-path))
    (setq expanded-path (replace-regexp-in-string "\\${srctree}/" "" expanded-path))
    
    expanded-path))

(defun linconf-expand-glob-pattern (pattern kernel-root)
  "Expand glob PATTERN relative to KERNEL-ROOT, returning list of matching files.
Supports *, ?, [abc], and ** patterns."
  (let ((expanded-pattern (linconf-expand-kconfig-variables pattern kernel-root)))
    (if (string-match-p "[*?\\[]" expanded-pattern)
        ;; Has glob characters - expand them
        (let ((full-pattern (expand-file-name expanded-pattern kernel-root)))
          ;; Handle ** recursive patterns by converting to shell glob
          (if (string-match-p "\\*\\*" full-pattern)
              ;; Use find for ** patterns since file-expand-wildcards doesn't support them
              (let* ((parts (split-string full-pattern "/\\*\\*/"))
                     (base-dir (car parts))
                     (file-pattern (if (> (length parts) 1) (cadr parts) "Kconfig")))
                (when (and (file-directory-p base-dir) (> (length file-pattern) 0))
                  (let ((find-cmd (format "find '%s' -name '%s' -type f 2>/dev/null" 
                                          base-dir file-pattern)))
                    (let ((result (shell-command-to-string find-cmd)))
                      (when (> (length (string-trim result)) 0)
                        (split-string (string-trim result) "\n"))))))
            ;; Regular glob patterns
            (file-expand-wildcards full-pattern)))
      ;; No glob characters - return single file if it exists
      (let ((full-path (expand-file-name expanded-pattern kernel-root)))
        (when (file-readable-p full-path)
          (list full-path))))))

(defun linconf-parse-source-directives (file kernel-root)
  "Parse source directives from Kconfig FILE, returning list of referenced files.
KERNEL-ROOT is the kernel source tree root for resolving relative paths.
Supports glob patterns and variable substitution."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((sources '())
            (file-dir (file-name-directory file))
            (in-if-block nil)
            (if-condition nil))
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ;; Handle if/endif blocks for conditional sourcing
           ((looking-at "^\\s-*if\\s-+\\(.+\\)")
            (setq in-if-block t
                  if-condition (match-string 1))
            (forward-line 1))
           ((looking-at "^\\s-*endif")
            (setq in-if-block nil
                  if-condition nil)
            (forward-line 1))
           ;; Handle source directives (with optional conditions)
           ((looking-at "^\\s-*source\\s-+\"\\([^\"]+\\)\"\\(\\s-+if\\s-+\\(.+\\)\\)?")
            (let* ((source-path (match-string 1))
                   (condition (match-string 3))
                   (should-include (if condition
                                     (let ((result (linconf-evaluate-condition condition)))
                                       (if (eq result 'unknown) t result)) ; Include unknown conditions
                                   t)))
              (when should-include
                (let ((expanded-files
                       (cond
                        ;; Absolute path
                        ((file-name-absolute-p source-path)
                         (if (string-match-p "[*?]" source-path)
                             (file-expand-wildcards source-path)
                           (when (file-readable-p source-path)
                             (list source-path))))
                        ;; Relative to kernel root (most common case)
                        (t
                         (linconf-expand-glob-pattern source-path kernel-root)))))
                  ;; Add all expanded files
                  (dolist (expanded-file expanded-files)
                    (when (and expanded-file (file-readable-p expanded-file))
                      (push expanded-file sources)))))
              (forward-line 1)))
           (t
            (forward-line 1))))
        (nreverse sources)))))

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

(defun linconf-parse-kconfig-option (lines entry-type)
  "Parse a single config option from LINES, return (name . plist).
ENTRY-TYPE can be 'config, 'menuconfig, 'choice, or 'comment."
  (let ((name nil)
        (type nil)
        (help nil)
        (depends nil)
        (select nil)
        (default nil)
        (range nil)
        (choices nil)
        (in-help nil)
        (is-menuconfig (eq entry-type 'menuconfig))
        (is-comment (eq entry-type 'comment))
        (comment-text nil))
    (dolist (line lines)
      (cond
       ;; Handle comment declaration
       ((string-match "^comment\\s-+\"\\([^\"]+\\)\"" line)
        (setq name (format "COMMENT_%s" (replace-regexp-in-string "[^A-Z0-9_]" "_" (upcase (match-string 1 line)))))
        (setq comment-text (match-string 1 line))
        (setq type 'comment))
       ;; Handle config or menuconfig declaration
       ((string-match "^\\(menu\\)?config \\([A-Z0-9_]+\\)" line)
        (setq name (match-string 2 line)))
       ;; Handle type declarations
       ((string-match "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\(?: \"\\([^\"]*\\)\"\\)?" line)
        (setq type (intern (match-string 1 line))))
       ;; Handle def_bool and def_tristate (type + default combined)
       ((string-match "^[ \t]+def_\\(bool\\|tristate\\)\\s-+\\(.+\\)" line)
        (setq type (intern (match-string 1 line)))
        (setq default (match-string 2 line)))
       ;; Handle dependencies
       ((string-match "^[ \t]+depends on \\(.+\\)" line)
        (setq depends (match-string 1 line)))
       ;; Handle selections (with optional conditions)
       ((string-match "^[ \t]+select \\([A-Z0-9_]+\\)\\(?: if \\(.+\\)\\)?" line)
        (let ((selected-option (match-string 1 line))
              (select-condition (match-string 2 line)))
          (push (if select-condition
                    (cons selected-option select-condition)
                  selected-option)
                select)))
       ;; Handle defaults
       ((string-match "^[ \t]+default \\(.+\\)" line)
        (setq default (match-string 1 line)))
       ;; Handle ranges
       ((string-match "^[ \t]+range \\([0-9]+\\) \\([0-9]+\\)" line)
        (setq range (cons (string-to-number (match-string 1 line))
                          (string-to-number (match-string 2 line)))))
       ;; Handle help text
       ((string-match "^[ \t]+help" line)
        (setq in-help t))
       ((and in-help (string-match "^[ \t]+\\(.+\\)" line))
        (setq help (if help
                       (concat help "\\n" (match-string 1 line))
                     (match-string 1 line))))
       ;; Handle choice options (for choice blocks)
       ((and (eq entry-type 'choice) (string-match "^[ \t]+config \\([A-Z0-9_]+\\)" line))
        (push (match-string 1 line) choices))))
    (when name
      (cons name (list :type (cond 
                             (is-comment 'comment)
                             (is-menuconfig 'menuconfig) 
                             (t type))
                       :help help
                       :depends depends
                       :select (nreverse select)
                       :default default
                       :range range
                       :choices (nreverse choices)
                       :menuconfig is-menuconfig
                       :comment is-comment
                       :comment-text comment-text)))))

(defun linconf-preprocess-continuations (content)
  "Preprocess CONTENT to handle line continuations (backslash at end of line)."
  (let ((lines (split-string content "\n"))
        (processed-lines '())
        (current-line ""))
    (dolist (line lines)
      (if (string-match "\\\\\\s-*$" line)
          ;; Line ends with backslash - continue on next line
          (setq current-line (concat current-line (replace-regexp-in-string "\\\\\\s-*$" " " line)))
        ;; Regular line or end of continuation
        (setq current-line (concat current-line line))
        (push current-line processed-lines)
        (setq current-line "")))
    ;; Handle any remaining continuation at end of file
    (when (> (length current-line) 0)
      (push current-line processed-lines))
    (nreverse processed-lines)))

(defun linconf-parse-kconfig-file (file)
  "Parse a single Kconfig file and return list of (name . plist) pairs.
Handles config, menuconfig, choice/endchoice, and menu/endmenu blocks."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (linconf-preprocess-continuations (buffer-string)))
            (options '())
            (current-config '())
            (current-choice '())
            (choice-options '())
            (in-config nil)
            (in-choice nil)
            (in-menu nil)
            (menu-stack '())
            (config-type 'config))
        
        (dolist (line lines)
          (let ((trimmed-line (string-trim line)))
            (cond
             ;; Handle mainmenu
             ((string-match "^mainmenu\\s-+\"\\([^\"]+\\)\"" line)
              (setq linconf-main-menu-title 
                    (linconf-expand-kconfig-variables (match-string 1 line) 
                                                     (file-name-directory file))))
             
             ;; Handle menu start
             ((string-match "^menu\\s-+\"\\([^\"]+\\)\"" line)
              (push (match-string 1 line) menu-stack)
              (setq in-menu t))
             
             ;; Handle menu end
             ((string-match "^endmenu" line)
              (when menu-stack
                (pop menu-stack))
              (setq in-menu (> (length menu-stack) 0)))
             
             ;; Handle choice start
             ((string-match "^choice" line)
              (when current-config
                ;; Finish previous config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                  (when option (push option options))))
              (setq current-choice (list line)
                    choice-options '()
                    in-choice t
                    current-config nil
                    in-config nil))
             
             ;; Handle choice end
             ((string-match "^endchoice" line)
              (when current-choice
                ;; Create a synthetic choice option
                (let* ((choice-name (format "CHOICE_%d" (random 10000)))
                       (choice-option (cons choice-name 
                                           (list :type 'choice
                                                 :choices choice-options
                                                 :help "Choice group"))))
                  (push choice-option options)))
              (setq in-choice nil
                    current-choice nil
                    choice-options '()))
             
             ;; Handle comment entries
             ((string-match "^comment\\s-+\"\\([^\"]+\\)\"" line)
              (when current-config
                ;; Finish previous config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                  (when option (push option options))))
              (setq current-config (list line)
                    in-config t
                    config-type 'comment))
             
             ;; Handle config entries
             ((string-match "^config[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((config-name (match-string 1 line))) ; Save match immediately
                (when current-config
                  ;; Finish previous config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                    (when option (push option options))))
                (setq current-config (list line)
                      in-config t
                      config-type 'config)
                (when in-choice
                  (push config-name choice-options))))
             
             ;; Handle menuconfig entries
             ((string-match "^menuconfig[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((menuconfig-name (match-string 1 line))) ; Save match immediately
                (when current-config
                  ;; Finish previous config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                    (when option (push option options))))
                (setq current-config (list line)
                      in-config t
                      config-type 'menuconfig)))
             
             ;; Handle indented lines (part of current config/choice)
             ((and (or in-config in-choice) (string-match "^[ \t]+" line))
              (when current-config
                (push line current-config)))
             
             ;; Handle non-indented, non-empty lines that end current config
             ((and in-config (not (string-match "^[ \t]*$" line))
                   (not (string-match "^\\(config\\|menuconfig\\|choice\\|endchoice\\|menu\\|endmenu\\|source\\|if\\|endif\\)" line)))
              (when current-config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                  (when option (push option options))))
              (setq current-config nil
                    in-config nil)))))
        
        ;; Handle any remaining config at end of file
        (when current-config
          (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
            (when option (push option options))))
        
        ;; Handle any remaining choice at end of file
        (when current-choice
          (let* ((choice-name (format "CHOICE_%d" (random 10000)))
                 (choice-option (cons choice-name 
                                     (list :type 'choice
                                           :choices choice-options
                                           :help "Choice group"))))
            (push choice-option options)))
        
        (nreverse options)))))

(defun linconf-load-kconfig-data ()
  "Load and parse all Kconfig files from kernel source tree."
  (interactive)
  (when linconf-kernel-source-path
    (unless (file-directory-p linconf-kernel-source-path)
      (error "Kernel source path does not exist: %s" linconf-kernel-source-path))
    (message "Loading Kconfig data from %s..." linconf-kernel-source-path)
    (clrhash linconf-kconfig-options)
    (clrhash linconf-config-values)
    (clrhash linconf-select-chains)
    
    ;; Initialize some basic config values based on detected architecture
    (let ((arch (linconf-detect-architecture linconf-kernel-source-path)))
      (pcase arch
        ("x86" (linconf-set-config-value "X86" t)
               (linconf-set-config-value "X86_64" t)
               (linconf-set-config-value "64BIT" t))
        ("arm64" (linconf-set-config-value "ARM64" t)
                 (linconf-set-config-value "64BIT" t))
        ("arm" (linconf-set-config-value "ARM" t))
        (_ nil)))
    
    ;; Set some common defaults
    (linconf-set-config-value "EXPERT" nil) ; Most users aren't experts
    (linconf-set-config-value "COMPILE_TEST" nil) ; Usually disabled
    (let ((kconfig-files (linconf-collect-kconfig-files linconf-kernel-source-path))
          (total-options 0))
      (dolist (file kconfig-files)
        (let ((options (linconf-parse-kconfig-file file)))
          (when (> (length options) 0)
            (message "Found %d options in %s" (length options) file))
          (dolist (option options)
            (puthash (car option) (cdr option) linconf-kconfig-options)
            (setq total-options (1+ total-options))
            
            ;; Process select statements for this option
            (let ((option-name (car option))
                  (option-plist (cdr option))
                  (select-statements (plist-get (cdr option) :select)))
              (dolist (select-stmt select-statements)
                (if (consp select-stmt)
                    ;; Conditional select: (selected-option . condition)
                    (linconf-add-select-statement option-name (car select-stmt) (cdr select-stmt))
                  ;; Simple select: selected-option
                  (linconf-add-select-statement option-name select-stmt)))))))
      (setq linconf-kconfig-loaded t)
      
      ;; Try to load existing .config file if it exists
      (let ((config-file (expand-file-name ".config" linconf-kernel-source-path)))
        (when (file-exists-p config-file)
          (message "Loading existing .config file...")
          (linconf-load-config-file config-file)
          (message "Loaded .config with %d configured options" 
                   (hash-table-count linconf-config-values))))
      
      (message "Loaded %d options from %d Kconfig files"
               total-options (length kconfig-files))))))

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
