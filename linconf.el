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
    (define-key map (kbd "C-c v") 'linconf-validate-current-option)
    (define-key map (kbd "C-c C-v") 'linconf-validate-all-options)
    (define-key map (kbd "C-c D") 'linconf-validate-all-dependencies)
    (define-key map (kbd "C-c C-f") 'linconf-fix-dependencies)
    (define-key map (kbd "C-c h") 'linconf-set-hex)
    (define-key map (kbd "C-c ?") 'linconf-show-help-text)
    (define-key map (kbd "C-c f") 'linconf-show-source-file)
    (define-key map (kbd "C-c H") 'linconf-highlight-invalid-configurations)
    (define-key map (kbd "C-c C-h") 'linconf-clear-highlighting)
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
:choices - for choice groups, list of options
:source-file - path to Kconfig file containing this option")

(defvar linconf-kconfig-loaded nil
  "Non-nil if Kconfig data has been loaded.")

(defvar linconf-main-menu-title nil
  "Main menu title from mainmenu directive.")

(defvar linconf-detected-architecture nil
  "Architecture detected from config file comment or kernel source.
Set automatically when config files are loaded.")

(defface linconf-invalid-face
  '((t (:background "red" :foreground "white")))
  "Face for invalid configuration lines."
  :group 'linconf)

(defface linconf-warning-face
  '((t (:background "yellow" :foreground "black")))
  "Face for configuration warnings."
  :group 'linconf)

(defvar linconf-font-lock-keywords
  '(("^# CONFIG_\\([A-Z0-9_]+\\) is not set" . font-lock-comment-face)
    ("^CONFIG_\\([A-Z0-9_]+\\)=" . font-lock-variable-name-face)
    ("=\\(y\\|n\\|m\\)" . font-lock-constant-face)
    ("=\\([0-9]+\\)" . font-lock-constant-face)
    ("=\"\\([^\"]*\\)\"" . font-lock-string-face)
    ("^#.*$" . font-lock-comment-face))
  "Font lock keywords for linconf mode.")

(defun linconf-toggle-option ()
  "Toggle a configuration option between enabled/disabled states with type awareness."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Handle "# CONFIG_FOO is not set" -> appropriate enabled state
     ((looking-at "^# CONFIG_\\([A-Z0-9_]+\\) is not set")
      (let* ((option (match-string 1))
             (kconfig-info (gethash option linconf-kconfig-options))
             (option-type (when kconfig-info (plist-get kconfig-info :type))))
        (cond
         ;; For tristate, enable as module by default
         ((eq option-type 'tristate)
          (linconf-set-option option "m" t))
         ;; For all others, enable as built-in
         (t
          (linconf-set-option option "y" t)))))
     ;; Handle "CONFIG_FOO=y" -> "# CONFIG_FOO is not set"
     ((looking-at "^CONFIG_\\([A-Z0-9_]+\\)=y")
      (let ((option (match-string 1)))
        (linconf-set-option option nil t)))
     ;; Handle "CONFIG_FOO=m" -> "CONFIG_FOO=y"
     ((looking-at "^CONFIG_\\([A-Z0-9_]+\\)=m")
      (let ((option (match-string 1)))
        (linconf-set-option option "y" t)))
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

(defun linconf-set-option (option value &optional force)
  "Set CONFIG option to value, replacing current line with validation.
If FORCE is t, skip all validation. Returns t on success, nil on validation failure."
  (when option
    (linconf-ensure-kconfig-loaded)
    (let ((validation-result (if force
                                (cons t nil)
                              (linconf-validate-option-value option value)))
          (dependency-result (if (or force (null value))
                                (cons t nil)
                              (linconf-validate-dependencies option))))

      ;; Check both type and dependency validation
      (if (and (car validation-result) (car dependency-result))
          (progn
            ;; All validation passed - set the option
            (delete-region (line-beginning-position) (line-end-position))
            (if (null value)
                (insert (format "# CONFIG_%s is not set" option))
              (insert (format "CONFIG_%s=%s" option value)))

            ;; Show warnings if there were any
            (when (cdr validation-result)
              (message "%s" (cdr validation-result)))
            (when (cdr dependency-result)
              (message "%s" (cdr dependency-result)))
            t)

        ;; Validation failed - show errors and don't change anything
        (let ((errors '()))
          (unless (car validation-result)
            (push (format "Type validation: %s" (cdr validation-result)) errors))
          (unless (car dependency-result)
            (push (format "Dependency validation: %s" (cdr dependency-result)) errors))
          (message "Validation failed for %s: %s" option (mapconcat #'identity errors "; "))
          nil)))))

(defun linconf-set-y ()
  "Set configuration option to 'y' (built-in)."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (when (linconf-set-option option "y")
          ;; Update internal config state and apply select chains only on success
          (linconf-set-config-with-chains option t))
      (message "No configuration option found on this line"))))

(defun linconf-set-m ()
  "Set configuration option to 'm' (module)."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (when (linconf-set-option option "m")
          ;; Update internal config state and apply select chains only on success
          (linconf-set-config-with-chains option 'module))
      (message "No configuration option found on this line"))))

(defun linconf-set-string ()
  "Set configuration option to a string value with validation."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (let ((value (read-string (format "Set CONFIG_%s to string: " option))))
          (linconf-set-option option (format "\"%s\"" value)))
      (message "No configuration option found on this line"))))

(defun linconf-set-number ()
  "Set configuration option to a numeric value with validation."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (let ((value (read-number (format "Set CONFIG_%s to number: " option))))
          (linconf-set-option option (number-to-string value)))
      (message "No configuration option found on this line"))))

(defun linconf-unset-option ()
  "Unset configuration option (set to 'is not set') with validation."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (when (linconf-set-option option nil)
          ;; Update internal config state only on success
          (linconf-set-config-with-chains option nil))
      (message "No configuration option found on this line"))))

(defun linconf-ensure-kconfig-loaded ()
  "Ensure Kconfig data is loaded if kernel source path is available.
Returns t if data is available, nil if no kernel source path is configured."
  (when (and linconf-kernel-source-path
             (file-directory-p linconf-kernel-source-path)
             (= (hash-table-count linconf-kconfig-options) 0))
    (message "Loading Kconfig data automatically...")
    (linconf-load-kconfig-data linconf-detected-architecture))
  (and linconf-kernel-source-path
       (> (hash-table-count linconf-kconfig-options) 0)))

(defun linconf-validate-option-value (option value)
  "Validate VALUE for configuration OPTION based on its Kconfig type.
Returns (valid . error-message) where valid is t/nil and error-message explains validation failure."
  (linconf-ensure-kconfig-loaded)
  (let* ((kconfig-info (gethash option linconf-kconfig-options))
         (option-type (when kconfig-info (plist-get kconfig-info :type)))
         (option-range (when kconfig-info (plist-get kconfig-info :range))))

    (if (not kconfig-info)
        ;; No Kconfig info - check for vendor-specific options
        (let ((vendor-info (linconf-is-vendor-specific-option option)))
          (if vendor-info
              ;; Create phantom entry for vendor-specific option and validate normally
              (progn
                (linconf-create-phantom-vendor-option option vendor-info)
                ;; Recursively validate now that we have the option info
                (linconf-validate-option-value option value))
            ;; Not vendor-specific - treat as missing
            (cons t (if linconf-kernel-source-path
                        (format "Warning: No Kconfig definition found for %s" option)
                      (format "Warning: %s not validated (no kernel source path set)" option)))))

      ;; Validate based on option type
      (cond
       ((eq option-type 'bool)
        (linconf-validate-bool-value value))
       ((eq option-type 'tristate)
        (linconf-validate-tristate-value value))
       ((eq option-type 'string)
        (linconf-validate-string-value value))
       ((eq option-type 'int)
        (linconf-validate-int-value value option-range))
       ((eq option-type 'hex)
        (linconf-validate-hex-value value option-range))
       (t
        (cons t nil)))))) ; Unknown type - allow

(defun linconf-validate-bool-value (value)
  "Validate VALUE for bool option. Returns (valid . error-message)."
  (cond
   ((member value '("y" "n" nil)) (cons t nil))
   (t (cons nil "Boolean options only accept 'y' or 'n' (or unset)"))))

(defun linconf-validate-tristate-value (value)
  "Validate VALUE for tristate option. Returns (valid . error-message)."
  (cond
   ((member value '("y" "m" "n" nil)) (cons t nil))
   (t (cons nil "Tristate options only accept 'y', 'm', 'n' (or unset)"))))

(defun linconf-validate-string-value (value)
  "Validate VALUE for string option. Returns (valid . error-message)."
  (cond
   ((null value) (cons t nil)) ; Unset string is valid
   ((and (stringp value)
         (or (not (string-match "\"" value))  ; No quotes (will be added)
             (and (string-prefix-p "\"" value) ; Or properly quoted
                  (string-suffix-p "\"" value)
                  (= (length (split-string value "\"")) 3)))) ; Exactly 2 quotes
    (cons t nil))
   (t (cons nil "String values should not contain unescaped quotes"))))

(defun linconf-validate-int-value (value range)
  "Validate VALUE for int option with optional RANGE. Returns (valid . error-message)."
  (cond
   ((null value) (cons t nil)) ; Unset int is valid
   ((not (string-match "^[0-9]+$" value))
    (cons nil "Integer values must be numeric"))
   (range
    (let ((num-value (string-to-number value))
          (min-val (car range))
          (max-val (cdr range)))
      (if (and (>= num-value min-val) (<= num-value max-val))
          (cons t nil)
        (cons nil (format "Integer value %s outside valid range [%d-%d]"
                         value min-val max-val)))))
   (t (cons t nil))))

(defun linconf-validate-hex-value (value range)
  "Validate VALUE for hex option with optional RANGE. Returns (valid . error-message)."
  (cond
   ((null value) (cons t nil)) ; Unset hex is valid
   ((not (string-match "^\\(0x\\)?[0-9a-fA-F]+$" value))
    (cons nil "Hex values must be in format '0x123' or '123' (hex digits only)"))
   (range
    (let* ((clean-value (if (string-prefix-p "0x" value)
                           (substring value 2) value))
           (num-value (string-to-number clean-value 16))
           (min-val (car range))
           (max-val (cdr range)))
      (if (and (>= num-value min-val) (<= num-value max-val))
          (cons t nil)
        (cons nil (format "Hex value %s outside valid range [0x%x-0x%x]"
                         value min-val max-val)))))
   (t (cons t nil))))

(defun linconf-validate-current-option ()
  "Validate the option on the current line and show result."
  (interactive)
  (linconf-ensure-kconfig-loaded)
  (let ((option (linconf-get-option-name)))
    (if option
        (save-excursion
          (beginning-of-line)
          (let ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
                (value nil))
            ;; Extract current value
            (cond
             ((string-match "^# CONFIG_[A-Z0-9_]+ is not set" line)
              (setq value nil))
             ((string-match "^CONFIG_[A-Z0-9_]+=\\(.+\\)" line)
              (setq value (match-string 1 line))))

            ;; Validate and report
            (let ((result (linconf-validate-option-value option value)))
              (if (car result)
                  (message "✓ %s: Valid%s" option
                          (if (cdr result) (format " (%s)" (cdr result)) ""))
                (message "✗ %s: %s" option (cdr result))))))
      (message "No configuration option found on this line"))))

(defun linconf-validate-all-options (&optional highlight)
  "Validate all options in the current buffer and report errors.
If HIGHLIGHT is non-nil, also highlight invalid configurations."
  (interactive "P")
  (linconf-ensure-kconfig-loaded)
  (when highlight
    (linconf-clear-highlighting))
  (let ((errors '())
        (warnings '())
        (valid-count 0)
        (config-file-name buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (when (string-match "^\\(?:# \\)?CONFIG_\\([A-Z0-9_]+\\)" line)
            (let* ((option (match-string 1 line))
                   (value (cond
                          ((string-match "^# CONFIG_[A-Z0-9_]+ is not set" line) nil)
                          ((string-match "^CONFIG_[A-Z0-9_]+=\\(.+\\)" line)
                           (match-string 1 line))))
                   (result (linconf-validate-option-value option value)))
              (if (car result)
                  (progn
                    (setq valid-count (1+ valid-count))
                    (when (cdr result)
                      (push (format "Line %d: %s" (line-number-at-pos) (cdr result)) warnings)
                      (when highlight
                        (linconf-highlight-line (line-number-at-pos) 'linconf-warning-face
                                              (format "Warning: %s" (cdr result))))))
                (push (format "Line %d: %s: %s"
                             (line-number-at-pos) option (cdr result)) errors)
                (when highlight
                  (linconf-highlight-line (line-number-at-pos) 'linconf-invalid-face
                                        (format "Invalid: %s" (cdr result)))))))
        (forward-line 1))))

    ;; Report results
    (with-current-buffer (get-buffer-create "*LinConf Validation*")
      (erase-buffer)
      (insert (format "LinConf Configuration Validation Report\n"))
      (insert (format "======================================\n\n"))
      (when config-file-name
        (insert (format "Config file: %s\n" config-file-name)))
      (when linconf-detected-architecture
        (insert (format "Architecture: %s\n" linconf-detected-architecture)))

      ;; Add kernel version compatibility information
      (when (and config-file-name linconf-kernel-source-path)
        (let ((config-version (linconf-detect-config-kernel-version config-file-name))
              (source-version (linconf-detect-source-kernel-version linconf-kernel-source-path)))
          (when config-version
            (insert (format "Config kernel version: %s\n" config-version)))
          (when source-version
            (insert (format "Source kernel version: %s\n" source-version)))
          (when (and config-version source-version)
            (let ((comparison (linconf-compare-kernel-versions source-version config-version)))
              (cond
               ((< comparison 0)
                (insert (format "⚠ VERSION MISMATCH: Source (%s) older than config (%s)\n" source-version config-version))
                (insert (format "   Many missing option warnings are expected due to version differences.\n")))
               ((> comparison 0)
                (insert (format "ℹ INFO: Source (%s) newer than config (%s) - validation should be complete.\n" source-version config-version)))
               (t
                (insert (format "✓ Kernel versions match - validation is fully accurate.\n"))))))))

      (insert (format "Valid options: %d\n" valid-count))
      (insert (format "Errors: %d\n" (length errors)))
      (insert (format "Warnings: %d\n\n" (length warnings)))

      (when errors
        (insert "ERRORS:\n")
        (dolist (error errors)
          (insert (format "  • %s\n" error)))
        (insert "\n"))

      (when warnings
        (insert "WARNINGS:\n")
        (dolist (warning warnings)
          (insert (format "  • %s\n" warning)))
        (insert "\n"))

      (when (and (= (length errors) 0) (= (length warnings) 0))
        (insert "✓ All configuration options are valid!\n"))

      (goto-char (point-min)))

    (display-buffer "*LinConf Validation*")
    (message "Validation complete: %d valid, %d errors, %d warnings"
             valid-count (length errors) (length warnings))))

(defun linconf-set-hex ()
  "Set configuration option to a hex value with validation."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (let* ((kconfig-info (gethash option linconf-kconfig-options))
               (option-range (when kconfig-info (plist-get kconfig-info :range)))
               (prompt (if option-range
                          (format "Set CONFIG_%s to hex (range 0x%x-0x%x): "
                                 option (car option-range) (cdr option-range))
                        (format "Set CONFIG_%s to hex value: " option)))
               (value (read-string prompt)))
          ;; Ensure 0x prefix
          (unless (string-prefix-p "0x" value)
            (setq value (concat "0x" value)))
          (linconf-set-option option value))
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

(defvar linconf-token-pos 0
  "Current position in token stream during expression evaluation.")

(defvar linconf-token-stream nil
  "Current token stream being evaluated.")

(defun linconf-eval-condition-tokens (tokens)
  "Evaluate tokenized condition expression using recursive descent parser."
  (let ((linconf-token-pos 0)
        (linconf-token-stream tokens))
    (linconf-eval-or-expr)))

(defun linconf-current-token ()
  "Get current token without advancing position."
  (when (< linconf-token-pos (length linconf-token-stream))
    (nth linconf-token-pos linconf-token-stream)))

(defun linconf-advance-token ()
  "Advance to next token and return current one."
  (let ((token (linconf-current-token)))
    (when token (setq linconf-token-pos (1+ linconf-token-pos)))
    token))

(defun linconf-eval-or-expr ()
  "Evaluate OR expression (lowest precedence)."
  (let ((result (linconf-eval-and-expr)))
    (while (equal (linconf-current-token) "||")
      (linconf-advance-token)
      (let ((right (linconf-eval-and-expr)))
        (setq result (linconf-logical-or result right))))
    result))

(defun linconf-eval-and-expr ()
  "Evaluate AND expression (higher precedence than OR)."
  (let ((result (linconf-eval-not-expr)))
    (while (equal (linconf-current-token) "&&")
      (linconf-advance-token)
      (let ((right (linconf-eval-not-expr)))
        (setq result (linconf-logical-and result right))))
    result))

(defun linconf-eval-not-expr ()
  "Evaluate NOT expression and primary expressions."
  (cond
   ((equal (linconf-current-token) "!")
    (linconf-advance-token)
    (linconf-logical-not (linconf-eval-primary-expr)))
   (t (linconf-eval-primary-expr))))

(defun linconf-eval-primary-expr ()
  "Evaluate primary expression (config names, parentheses)."
  (let ((token (linconf-current-token)))
    (cond
     ((equal token "(")
      (linconf-advance-token)
      (let ((result (linconf-eval-or-expr)))
        (unless (equal (linconf-advance-token) ")")
          (error "Missing closing parenthesis in condition"))
        result))
     ((and token (string-match "^[A-Z0-9_]+$" token))
      (linconf-advance-token)
      (let ((value (gethash token linconf-config-values 'unknown)))
        (cond
         ((eq value 'unknown) 'unknown)
         ((or (eq value t) (eq value 'y) (eq value 'm)) t)
         (t nil))))
     (t (error "Invalid token in condition: %s" token)))))

(defun linconf-logical-or (left right)
  "Logical OR with three-valued logic (t, nil, unknown)."
  (cond
   ((eq left t) t)
   ((eq right t) t)
   ((and (eq left nil) (eq right nil)) nil)
   (t 'unknown)))

(defun linconf-logical-and (left right)
  "Logical AND with three-valued logic (t, nil, unknown)."
  (cond
   ((eq left nil) nil)
   ((eq right nil) nil)
   ((and (eq left t) (eq right t)) t)
   (t 'unknown)))

(defun linconf-logical-not (value)
  "Logical NOT with three-valued logic (t, nil, unknown)."
  (cond
   ((eq value t) nil)
   ((eq value nil) t)
   (t 'unknown)))

(defun linconf-set-config-value (option value)
  "Set configuration OPTION to VALUE for condition evaluation."
  (puthash option value linconf-config-values))

(defun linconf-get-config-value (option)
  "Get current value of configuration OPTION."
  (gethash option linconf-config-values 'unknown))

(defun linconf-validate-dependencies (option)
  "Validate that all dependencies for OPTION are satisfied.
Returns (valid . error-message) where valid is t/nil."
  (let* ((kconfig-info (gethash option linconf-kconfig-options))
         (depends (when kconfig-info (plist-get kconfig-info :depends))))
    (if (not depends)
        (cons t nil)
      (let ((result (linconf-evaluate-condition depends)))
        (cond
         ((eq result t) (cons t nil))
         ((eq result nil) (cons nil (format "Dependencies not satisfied: %s" depends)))
         (t (cons t (format "Warning: Cannot verify dependencies: %s" depends))))))))

(defun linconf-get-unsatisfied-dependencies (option)
  "Get list of unsatisfied dependencies for OPTION.
Returns list of dependency expressions that evaluate to false."
  (let* ((kconfig-info (gethash option linconf-kconfig-options))
         (depends (when kconfig-info (plist-get kconfig-info :depends))))
    (when depends
      (let ((result (linconf-evaluate-condition depends)))
        (when (eq result nil)
          (list depends))))))

(defun linconf-get-dependency-chain (option &optional visited)
  "Get full dependency chain for OPTION, detecting cycles.
Returns list of options in dependency order, or nil if circular dependency found."
  (let ((visited (or visited '())))
    (if (member option visited)
        nil
      (let* ((kconfig-info (gethash option linconf-kconfig-options))
             (depends (when kconfig-info (plist-get kconfig-info :depends)))
             (new-visited (cons option visited))
             (chain (list option)))
        (when depends
          (let ((dep-options (linconf-extract-options-from-condition depends)))
            (dolist (dep-option dep-options)
              (let ((sub-chain (linconf-get-dependency-chain dep-option new-visited)))
                (if sub-chain
                    (setq chain (append sub-chain chain))
                  (setq chain nil)
                  (return))))))
        chain))))

(defun linconf-extract-options-from-condition (condition)
  "Extract all config option names from CONDITION expression."
  (let ((options '()))
    (when (stringp condition)
      (let ((tokens (linconf-tokenize-condition condition)))
        (dolist (token tokens)
          (when (string-match "^[A-Z0-9_]+$" token)
            (push token options)))))
    (delete-dups options)))

(defun linconf-detect-circular-dependencies ()
  "Detect circular dependencies in all Kconfig options.
Returns list of (option . cycle-path) for options with circular dependencies."
  (let ((circular-deps '())
        (all-options '()))
    (maphash (lambda (option _info) (push option all-options)) linconf-kconfig-options)
    (dolist (option all-options)
      (let ((cycle-path (linconf-find-dependency-cycle option)))
        (when cycle-path
          (push (cons option cycle-path) circular-deps))))
    circular-deps))

(defun linconf-find-dependency-cycle (option &optional path)
  "Find dependency cycle starting from OPTION.
Returns cycle path if found, nil otherwise."
  (let ((path (or path '())))
    (if (member option path)
        (append path (list option))
      (let* ((new-path (cons option path))
             (kconfig-info (gethash option linconf-kconfig-options))
             (depends (when kconfig-info (plist-get kconfig-info :depends))))
        (when depends
          (let ((dep-options (linconf-extract-options-from-condition depends)))
            (catch 'cycle-found
              (dolist (dep-option dep-options)
                (let ((cycle (linconf-find-dependency-cycle dep-option new-path)))
                  (when cycle
                    (throw 'cycle-found cycle))))
              nil)))))))

(defun linconf-build-dependency-graph ()
  "Build dependency graph for all Kconfig options.
Returns hash table with option -> list-of-dependencies mapping."
  (let ((graph (make-hash-table :test 'equal)))
    (maphash (lambda (option info)
               (let ((depends (plist-get info :depends)))
                 (when depends
                   (let ((dep-options (linconf-extract-options-from-condition depends)))
                     (puthash option dep-options graph)))))
             linconf-kconfig-options)
    graph))

(defun linconf-find-dependency-path (from to &optional visited)
  "Find dependency path from FROM to TO option.
Returns list of options forming the path, or nil if no path exists."
  (let ((visited (or visited '())))
    (cond
     ((equal from to) (list to))
     ((member from visited) nil)
     (t (let* ((new-visited (cons from visited))
               (kconfig-info (gethash from linconf-kconfig-options))
               (depends (when kconfig-info (plist-get kconfig-info :depends))))
          (when depends
            (let ((dep-options (linconf-extract-options-from-condition depends)))
              (catch 'path-found
                (dolist (dep-option dep-options)
                  (let ((sub-path (linconf-find-dependency-path dep-option to new-visited)))
                    (when sub-path
                      (throw 'path-found (cons from sub-path)))))
                nil))))))))

(defun linconf-get-select-statements (option)
  "Get all options that OPTION selects.
Returns list of (selected-option . condition) pairs from embedded :selects chain."
  (let ((option-info (gethash option linconf-kconfig-options)))
    (if option-info
        (plist-get option-info :selects)
      '())))

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
                (setq changes (append sub-changes changes)))))))))
    
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
  "Find all options that select the given OPTION.
Returns list of (selector-option . condition) pairs from embedded :selected-by chain."
  (let ((option-info (gethash option linconf-kconfig-options)))
    (if option-info
        (plist-get option-info :selected-by)
      '())))

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
Returns plist with :depends, :depends-on, :selects, :selected-by, :conflicts.
All data is retrieved directly from embedded chains in linconf-kconfig-options."
  (let* ((option-info (gethash option linconf-kconfig-options))
         (depends (when option-info (plist-get option-info :depends)))
         (depends-on (when option-info (plist-get option-info :depends-on)))
         (selects-chain (when option-info (plist-get option-info :selects)))
         (selected-by-chain (when option-info (plist-get option-info :selected-by)))
         (selects (mapcar #'car selects-chain))
         (selected-by (mapcar #'car selected-by-chain))
         (conflicts '())) ; TODO: Implement conflict detection

    (list :depends depends
          :depends-on depends-on
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

(defun linconf-relative-source-path (source-file)
  "Return SOURCE-FILE path relative to kernel source root for display.
If SOURCE-FILE is absolute and within kernel source tree, return relative path.
If SOURCE-FILE is already relative, return as-is.
Otherwise return the filename only."
  (when source-file
    (cond
     ;; If already relative path, return as-is
     ((not (file-name-absolute-p source-file)) source-file)
     ;; If absolute and within kernel source tree, make relative
     ((and linconf-kernel-source-path
           (string-prefix-p (expand-file-name linconf-kernel-source-path) source-file))
      (file-relative-name source-file linconf-kernel-source-path))
     ;; Otherwise return just filename for readability
     (t (file-name-nondirectory source-file)))))

(defun linconf-show-help-text ()
  "Show help text for the config option on the current line."
  (interactive)
  (let ((option (linconf-get-option-name)))
    (if option
        (let ((kconfig-info (gethash option linconf-kconfig-options)))
          (if kconfig-info
              (let ((help-text (plist-get kconfig-info :help))
                    (option-type (plist-get kconfig-info :type))
                    (range (plist-get kconfig-info :range))
                    (choices (plist-get kconfig-info :choices))
                    (default (plist-get kconfig-info :default))
                    (source-file (plist-get kconfig-info :source-file)))
                (with-output-to-temp-buffer "*LinConf Help*"
                  (princ (format "CONFIG_%s\n" option))
                  (princ (format "Type: %s\n" (or option-type "unknown")))
                  (when source-file
                    (let ((relative-path (linconf-relative-source-path source-file)))
                      (princ (format "Source: %s\n" relative-path))))
                  (when default
                    (princ (format "Default: %s\n" default)))
                  (when range
                    (princ (format "Range: %s to %s\n" (car range) (cdr range))))
                  (when choices
                    (princ (format "Choices: %s\n" (mapconcat #'identity choices ", "))))
                  (princ "\n")
                  (if help-text
                      (princ help-text)
                    (princ "No help text available for this option."))))
            (message "No Kconfig definition found for CONFIG_%s" option)))
      (message "No configuration option found on this line"))))

(defun linconf-show-source-file ()
  "Display the source file path for the Kconfig option at point."
  (interactive)
  (linconf-ensure-kconfig-loaded)
  (let ((option (linconf-get-option-name)))
    (if option
        (let* ((option-plist (gethash option linconf-kconfig-options))
               (source-file (when option-plist (plist-get option-plist :source-file))))
          (if source-file
              (let ((display-path (linconf-relative-source-path source-file)))
                (message "CONFIG_%s defined in: %s" option display-path))
            (message "CONFIG_%s: No source file information available" option)))
      (message "No configuration option found on this line"))))

(defvar linconf-invalid-overlays '()
  "List of overlays used to highlight invalid configurations.")

(defun linconf-clear-highlighting ()
  "Clear all invalid configuration highlighting."
  (interactive)
  (dolist (overlay linconf-invalid-overlays)
    (delete-overlay overlay))
  (setq linconf-invalid-overlays '()))

(defun linconf-highlight-line (line-number face &optional message)
  "Highlight LINE-NUMBER with FACE and optional MESSAGE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'face face)
        (when message
          (overlay-put overlay 'help-echo message))
        (push overlay linconf-invalid-overlays)))))

(defun linconf-highlight-invalid-configurations ()
  "Highlight all invalid configurations in the current buffer."
  (interactive)
  (linconf-ensure-kconfig-loaded)
  (linconf-clear-highlighting)
  (save-excursion
    (goto-char (point-min))
    (let ((line-number 1))
      (while (not (eobp))
        (beginning-of-line)
        (when (looking-at "^CONFIG_\\([A-Z0-9_]+\\)=\\(.*\\)")
          (let* ((option (match-string 1))
                 (value (match-string 2))
                 (validation-result (linconf-validate-option-value option value)))
            (unless (car validation-result)
              (linconf-highlight-line line-number 'linconf-invalid-face
                                    (format "Invalid: %s" (cdr validation-result))))))
        (forward-line 1)
        (setq line-number (1+ line-number))))))

(defun linconf-simulate-config-change (option value)
  "Simulate setting OPTION to VALUE and show what would be auto-selected.
Returns list of changes that would be made."
  (let ((changes (linconf-resolve-select-chains option value)))
    (when changes
      (message "Setting %s=%s would auto-select:" option (if value "y" "n"))
      (dolist (change changes)
        (message "  %s=%s" (car change) (if (cdr change) "y" "n"))))
    changes))

(defun linconf-validate-all-dependencies ()
  "Validate dependencies for all options in the current buffer."
  (interactive)
  (linconf-ensure-kconfig-loaded)
  (let ((errors '())
        (warnings '())
        (valid-count 0)
        (circular-deps (linconf-detect-circular-dependencies)))

    ;; Check for circular dependencies first
    (when circular-deps
      (dolist (circular circular-deps)
        (push (format "Circular dependency: %s -> %s"
                     (car circular)
                     (mapconcat #'identity (cdr circular) " -> ")) errors)))

    ;; Validate dependencies for all options in buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (when (string-match "^CONFIG_\\([A-Z0-9_]+\\)=" line)
            (let* ((option (match-string 1 line))
                   (result (linconf-validate-dependencies option)))
              (if (car result)
                  (progn
                    (setq valid-count (1+ valid-count))
                    (when (cdr result)
                      (push (format "Line %d: %s" (line-number-at-pos) (cdr result)) warnings)))
                (push (format "Line %d: %s: %s"
                             (line-number-at-pos) option (cdr result)) errors)))))
        (forward-line 1)))

    ;; Report results
    (with-current-buffer (get-buffer-create "*LinConf Dependency Validation*")
      (erase-buffer)
      (insert (format "Dependency Validation Results\n"))
      (insert (format "============================\n\n"))
      (insert (format "Valid dependencies: %d\n" valid-count))
      (insert (format "Warnings: %d\n" (length warnings)))
      (insert (format "Errors: %d\n\n" (length errors)))

      (when errors
        (insert "ERRORS:\n")
        (dolist (error errors)
          (insert (format "  %s\n" error)))
        (insert "\n"))

      (when warnings
        (insert "WARNINGS:\n")
        (dolist (warning warnings)
          (insert (format "  %s\n" warning)))
        (insert "\n"))

      (when (and (= (length errors) 0) (= (length warnings) 0))
        (insert "All dependencies are satisfied!\n"))

      (display-buffer (current-buffer)))))

(defun linconf-fix-dependencies ()
  "Attempt to automatically fix dependency issues in the current buffer."
  (interactive)
  (linconf-ensure-kconfig-loaded)
  (let ((fixed-count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
          (when (string-match "^CONFIG_\\([A-Z0-9_]+\\)=" line)
            (let* ((option (match-string 1 line))
                   (unsatisfied (linconf-get-unsatisfied-dependencies option)))
              (when unsatisfied
                (dolist (dep-expr unsatisfied)
                  (let ((dep-options (linconf-extract-options-from-condition dep-expr)))
                    (dolist (dep-option dep-options)
                      (when (eq (linconf-get-config-value dep-option) 'unknown)
                        (when (y-or-n-p (format "Enable %s to satisfy dependency for %s? " dep-option option))
                          (linconf-set-config-value dep-option t)
                          (setq fixed-count (1+ fixed-count)))))))))
        (forward-line 1)))
    (if (> fixed-count 0)
        (message "Fixed %d dependency issues. Re-run validation to verify." fixed-count)
      (message "No automatically fixable dependency issues found."))))))

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
      ;; Detect and set architecture from config file comment
      (let ((detected-arch (linconf-detect-architecture-from-config config-file)))
        (when detected-arch
          (setq linconf-detected-architecture detected-arch)
          (linconf-set-architecture-variables detected-arch)
          ;; Update mode line if we're in a linconf buffer
          (when (derived-mode-p 'linconf-mode)
            (force-mode-line-update))
          (message "Detected architecture from config: %s" detected-arch)))

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
      config-path))))

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

(defun linconf-detect-architecture-from-config (config-file)
  "Detect architecture from config file first line comment.
Returns the architecture string if detected, nil otherwise.
Supports patterns like '# x86_64', '# arm64', '# riscv', etc."
  (when (file-readable-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file nil 0 100) ; Read just first 100 chars
      (goto-char (point-min))
      (when (looking-at "^#[ \t]*\\([a-zA-Z0-9_]+\\)")
        (let ((arch-comment (match-string 1)))
          (cond
           ;; Map config comment architectures to standard names
           ((string-match "x86_64\\|x86" arch-comment) "x86")
           ((string-match "arm64\\|aarch64" arch-comment) "arm64")
           ((string-match "arm" arch-comment) "arm")
           ((string-match "riscv" arch-comment) "riscv")
           ((string-match "s390" arch-comment) "s390")
           ((string-match "powerpc\\|ppc" arch-comment) "powerpc")
           ((string-match "mips" arch-comment) "mips")
           ;; Return the comment as-is for other architectures
           (t arch-comment)))))))

(defun linconf-set-architecture-variables (arch)
  "Set architecture-specific config variables based on ARCH.
These variables are commonly used in Kconfig dependency expressions."
  (when arch
    (pcase arch
      ("x86" (linconf-set-config-value "X86" t)
             (linconf-set-config-value "X86_64" t)
             (linconf-set-config-value "64BIT" t))
      ("arm64" (linconf-set-config-value "ARM64" t)
               (linconf-set-config-value "64BIT" t))
      ("arm" (linconf-set-config-value "ARM" t))
      ("riscv" (linconf-set-config-value "RISCV" t)
               (linconf-set-config-value "64BIT" t))
      ("s390" (linconf-set-config-value "S390" t)
              (linconf-set-config-value "64BIT" t))
      ("powerpc" (linconf-set-config-value "PPC" t)
                 (linconf-set-config-value "PPC64" t)
                 (linconf-set-config-value "64BIT" t))
      ("mips" (linconf-set-config-value "MIPS" t))
      (_ nil))))

(defun linconf-mode-line-architecture ()
  "Return architecture string for mode line display.
Returns formatted string like '[x86_64]' or empty string if no arch detected."
  (if linconf-detected-architecture
      (format " [%s]" linconf-detected-architecture)
    ""))

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

(defun linconf-parse-kconfig-option (lines &optional entry-type source-file)
  "Parse a single config option from LINES, return (name . plist).
ENTRY-TYPE can be 'config, 'menuconfig, 'choice, or 'comment (defaults to 'config).
SOURCE-FILE is the path to the Kconfig file containing this option."
  (let* ((entry-type (or entry-type 'config))
         (name nil)
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
       ((string-match "^[ \t]*\\(menu\\)?config[ \t]+\\([A-Z0-9_]+\\)" line)
        (setq name (match-string 2 line)))
       ;; Handle type declarations (but not in help text)
       ((and (not in-help) (string-match "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\b\\(?: \"\\([^\"]*\\)\"\\)?\\(?: if .*\\)?" line))
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
                       :comment-text comment-text
                       :source-file source-file)))))

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
            (config-type 'config)
            (in-if-block nil)
            (if-conditions '()))
        
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

             ;; Handle if blocks (conditional configuration)
             ((string-match "^if\\s-+\\(.+\\)" line)
              (let ((condition (match-string 1 line)))
                (push condition if-conditions)
                (setq in-if-block t)))

             ;; Handle endif blocks
             ((string-match "^endif" line)
              (when if-conditions
                (pop if-conditions))
              (setq in-if-block (> (length if-conditions) 0)))

             ;; Handle choice start
             ((string-match "^choice" line)
              (when current-config
                ;; Finish previous config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
                  (when option (push option options))))
              (setq current-choice (list line)
                    choice-options '()
                    in-choice t
                    current-config nil
                    in-config nil))
             
             ;; Handle choice end
             ((string-match "^endchoice" line)
              (when current-choice
                ;; Process any remaining config before ending choice
                (when current-config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
                    (when option (push option options))))
                ;; Create phantom entries for individual choice options
                (dolist (choice-opt choice-options)
                  (let ((phantom-option (cons choice-opt
                                             (list :type 'bool
                                                   :help (format "Choice option in group")
                                                   :phantom t))))
                    (push phantom-option options)))
                ;; Create a synthetic choice option
                (let* ((choice-name (format "CHOICE_%d" (random 10000)))
                       (choice-option (cons choice-name
                                           (list :type 'choice
                                                 :choices choice-options
                                                 :help "Choice group"))))
                  (push choice-option options)))
              (setq in-choice nil
                    current-choice nil
                    choice-options '()
                    current-config nil
                    in-config nil))
             
             ;; Handle comment entries
             ((string-match "^comment\\s-+\"\\([^\"]+\\)\"" line)
              (when current-config
                ;; Finish previous config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
                  (when option (push option options))))
              (setq current-config (list line)
                    in-config t
                    config-type 'comment))
             
             ;; Handle config entries (top-level and in choices)
             ((string-match "^[ \t]*config[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((config-name (match-string 1 line))) ; Save match immediately
                (when current-config
                  ;; Finish previous config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
                    (when option (push option options))))
                (setq current-config (list line)
                      in-config t
                      config-type 'config)
                (when in-choice
                  (push config-name choice-options))))
             
             ;; Handle menuconfig entries (top-level and in choices)
             ((string-match "^[ \t]*menuconfig[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((menuconfig-name (match-string 1 line))) ; Save match immediately
                (when current-config
                  ;; Finish previous config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
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
                   (not (string-match "^\\(config\\|menuconfig\\|choice\\|endchoice\\|menu\\|endmenu\\|source\\)" line)))
              (when current-config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
                  (when option (push option options))))
              (setq current-config nil
                    in-config nil)))))
        
        ;; Handle any remaining config at end of file
        (when current-config
          (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type file)))
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

(defun linconf-build-selection-chains ()
  "Build forward (:selects) and reverse (:selected-by) selection chains.
Also extract :depends-on lists from :depends expressions.
This consolidates selection chain data directly into linconf-kconfig-options,
eliminating the need for a separate linconf-select-chains hash table."
  (let ((all-options '()))
    ;; First pass: collect all option names
    (maphash (lambda (name _plist) (push name all-options))
             linconf-kconfig-options)

    ;; Second pass: process each option
    (dolist (option-name all-options)
      (let* ((option-plist (gethash option-name linconf-kconfig-options))
             (select-list (plist-get option-plist :select))
             (depends-expr (plist-get option-plist :depends))
             (selects-chain '())
             (depends-on-list '()))

        ;; Build :selects forward chain from :select data
        ;; Format: list of (selected-option . condition) pairs
        (when select-list
          (dolist (select-stmt select-list)
            (if (consp select-stmt)
                ;; Already in (option . condition) format
                (push select-stmt selects-chain)
              ;; Simple select without condition
              (push (cons select-stmt nil) selects-chain))))

        ;; Extract :depends-on list from :depends expression
        (when depends-expr
          (setq depends-on-list (linconf-extract-options-from-condition depends-expr)))

        ;; Update the option with forward chains
        (puthash option-name
                 (plist-put (plist-put option-plist
                                      :selects (nreverse selects-chain))
                           :depends-on depends-on-list)
                 linconf-kconfig-options)))

    ;; Third pass: build :selected-by reverse chains
    (dolist (selector-name all-options)
      (let* ((selector-plist (gethash selector-name linconf-kconfig-options))
             (selects-chain (plist-get selector-plist :selects)))
        ;; For each option this selector selects, add reverse reference
        (dolist (select-pair selects-chain)
          (let* ((selected-name (car select-pair))
                 (condition (cdr select-pair))
                 (selected-plist (gethash selected-name linconf-kconfig-options)))
            (when selected-plist
              (let ((current-selected-by (plist-get selected-plist :selected-by)))
                ;; Add this selector to the selected option's :selected-by list
                (puthash selected-name
                        (plist-put selected-plist
                                  :selected-by
                                  (cons (cons selector-name condition)
                                        current-selected-by))
                        linconf-kconfig-options)))))))))

(defun linconf-apply-vendor-type-corrections ()
  "Apply vendor-specific type corrections for known kernel patches.
This handles cases where distributions (like RHEL) patch the mainline kernel
to change option types (e.g., bool to tristate)."
  (let ((corrections '(("TEST_MISC_MINOR" tristate "RHEL patches TEST_MISC_MINOR to tristate")))
        (applied-count 0))
    (dolist (correction corrections)
      (let* ((option (nth 0 correction))
             (new-type (nth 1 correction))
             (reason (nth 2 correction))
             (option-plist (gethash option linconf-kconfig-options)))
        (when option-plist
          (let ((old-type (plist-get option-plist :type)))
            (when (and old-type (not (eq old-type new-type)))
              (puthash option
                      (plist-put option-plist :type new-type)
                      linconf-kconfig-options)
              (setq applied-count (1+ applied-count))
              (message "Applied type correction: %s %s -> %s (%s)"
                      option old-type new-type reason))))))
    (when (> applied-count 0)
      (message "Applied %d vendor-specific type corrections" applied-count))))

(defun linconf-load-kconfig-data (&optional target-arch)
  "Load and parse all Kconfig files from kernel source tree.
TARGET-ARCH can be specified to load architecture-specific Kconfig files.
If nil, detects architecture from kernel source tree."
  (interactive)
  (when linconf-kernel-source-path
    (unless (file-directory-p linconf-kernel-source-path)
      (error "Kernel source path does not exist: %s" linconf-kernel-source-path))
    (message "Loading Kconfig data from %s..." linconf-kernel-source-path)
    (clrhash linconf-kconfig-options)
    (clrhash linconf-config-values)

    ;; Determine target architecture - use provided arch or detect from source
    (let ((arch (or target-arch
                    linconf-detected-architecture
                    (linconf-detect-architecture linconf-kernel-source-path))))

      ;; Initialize some basic config values based on detected architecture
      (pcase arch
        ("x86" (linconf-set-config-value "X86" t)
               (linconf-set-config-value "X86_64" t)
               (linconf-set-config-value "64BIT" t))
        ("arm64" (linconf-set-config-value "ARM64" t)
                 (linconf-set-config-value "64BIT" t))
        ("arm" (linconf-set-config-value "ARM" t))
        ("riscv" (linconf-set-config-value "RISCV" t)
                 (linconf-set-config-value "64BIT" t))
        ("s390" (linconf-set-config-value "S390" t)
                (linconf-set-config-value "64BIT" t))
        ("powerpc" (linconf-set-config-value "PPC" t)
                   (linconf-set-config-value "PPC64" t)
                   (linconf-set-config-value "64BIT" t))
        (_ nil))

      ;; Set some common defaults
      (linconf-set-config-value "EXPERT" nil) ; Most users aren't experts
      (linconf-set-config-value "COMPILE_TEST" nil) ; Usually disabled

      ;; Collect main Kconfig files
      (let ((kconfig-files (linconf-collect-kconfig-files linconf-kernel-source-path))
            (total-options 0))

        ;; Add architecture-specific Kconfig file if it exists
        (when arch
          (let ((arch-kconfig (expand-file-name (format "arch/%s/Kconfig" arch)
                                               linconf-kernel-source-path)))
            (when (file-readable-p arch-kconfig)
              (push arch-kconfig kconfig-files)
              (message "Including architecture-specific Kconfig: arch/%s/Kconfig" arch))))

        (dolist (file kconfig-files)
        (let ((options (linconf-parse-kconfig-file file)))
          (when (> (length options) 0)
            (message "Found %d options in %s" (length options) file))
          (dolist (option options)
            ;; FIX: Prevent overwriting correctly parsed types with incorrect ones
            ;; Issue: Multiple Kconfig files can define the same option, and later definitions
            ;; with missing/incorrect type information were overwriting correctly parsed
            ;; bool/tristate options, causing them to be treated as 'int' type during validation.
            ;; Solution: Only allow overwriting if the new definition has a valid type.
            (let* ((option-name (car option))
                   (new-plist (cdr option))
                   (new-type (plist-get new-plist :type))
                   (existing-plist (gethash option-name linconf-kconfig-options))
                   (existing-type (when existing-plist (plist-get existing-plist :type))))
              ;; Only overwrite if the new definition has a valid type, or if there's no existing entry
              (when (or (not existing-plist)
                        (and new-type (not (eq new-type 'unknown)))
                        (and existing-type (eq existing-type 'unknown)))
                (puthash option-name new-plist linconf-kconfig-options)))
            (setq total-options (1+ total-options)))))

      ;; Build selection chains after all options are loaded
      (message "Building selection and dependency chains...")
      (linconf-build-selection-chains)

      ;; Apply vendor-specific type corrections for known kernel patches
      (linconf-apply-vendor-type-corrections)

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
  ;; Add architecture to mode line
  (setq mode-line-buffer-identification
        (list (default-value 'mode-line-buffer-identification)
              '(:eval (linconf-mode-line-architecture))))
  ;; Detect architecture from config file when mode is activated
  (when buffer-file-name
    (let ((detected-arch (linconf-detect-architecture-from-config buffer-file-name)))
      (when detected-arch
        (setq linconf-detected-architecture detected-arch)
        (linconf-set-architecture-variables detected-arch)
        (force-mode-line-update)
        (message "Detected architecture: %s" detected-arch)
        ;; Reload Kconfig data with the detected architecture if kernel source is available
        (when (and linconf-kernel-source-path
                   (file-directory-p linconf-kernel-source-path))
          (message "Reloading Kconfig data for architecture: %s" detected-arch)
          (linconf-load-kconfig-data detected-arch)
          ;; Check for kernel version compatibility issues
          (linconf-check-kernel-version-compatibility)))))
  (font-lock-mode 1))

(defvar linconf-vendor-specific-options
  '(;; RedHat/RHEL specific options
    ("RH_AUTOMOTIVE" :type bool :vendor "RedHat" :description "RedHat automotive variant configuration")
    ("RH_KABI_SIZE_ALIGN_CHECKS" :type bool :vendor "RedHat" :description "RedHat KABI size alignment checks")
    ("RHEL_DIFFERENCES" :type bool :vendor "RedHat" :description "RHEL-specific differences")
    ("NETFILTER_XTABLES_LEGACY" :type bool :vendor "RedHat" :description "Legacy xtables support")

    ;; Version-specific options that may be missing in older kernels
    ("KUNIT_DEFAULT_TIMEOUT" :type int :version "6.17" :description "KUnit default test timeout")
    ("RATELIMIT_KUNIT_TEST" :type tristate :version "6.17" :description "Rate limiting KUnit tests")
    ("TEST_KEXEC_HANDOVER" :type tristate :version "6.17" :description "Kexec handover test module")
    ("TEST_MISC_MINOR" :type tristate :vendor "rhel" :description "miscdevice KUnit test (RHEL tristate variant)")
    ("SEQ_BUF_KUNIT_TEST" :type tristate :version "6.17" :description "Seq buffer KUnit tests")
    ("EPROBE_EVENTS" :type bool :version "6.17" :description "Enable eprobe events")
    ("TRACEFS_AUTOMOUNT_DEPRECATED" :type bool :version "6.17" :description "Deprecated tracefs automount")

    ;; ARM64 platform-specific options
    ("ARM64_BRBE" :type bool :arch "arm64" :version "6.17" :description "ARM64 Branch Record Buffer Extension")
    ("ARM_GIC_V5" :type bool :arch "arm64" :version "6.17" :description "ARM GIC version 5 support")
    ("ARM_GIC_ITS_PARENT" :type bool :arch "arm64" :version "6.17" :description "ARM GIC ITS parent support")
    ("PTDUMP_STAGE2_DEBUGFS" :type bool :arch "arm64" :description "Stage 2 page table dumping via debugfs")
    ("NVHE_EL2_DEBUG" :type bool :arch "arm64" :description "Non-VHE EL2 debug support")

    ;; Crypto library options
    ("CRYPTO_LIB_BENCHMARK" :type bool :version "6.17" :description "Crypto library benchmarking")
    ("CRYPTO_LIB_BENCHMARK_VISIBLE" :type bool :version "6.17" :description "Make crypto benchmarks visible")
    ("CRYPTO_LIB_SHA512_KUNIT_TEST" :type tristate :version "6.17" :description "SHA512 library KUnit tests")
    ("CRYPTO_LIB_SHA256_KUNIT_TEST" :type tristate :version "6.17" :description "SHA256 library KUnit tests")
    ("CRYPTO_LIB_SHA1_KUNIT_TEST" :type tristate :version "6.17" :description "SHA1 library KUnit tests")
    ("CRYPTO_LIB_POLY1305_KUNIT_TEST" :type tristate :version "6.17" :description "Poly1305 library KUnit tests")

    ;; Device-specific options
    ("PHY_QCOM_M31_EUSB" :type tristate :version "6.17" :description "Qualcomm M31 eUSB PHY driver")
    ("PWM_ARGON_FAN_HAT" :type tristate :version "6.17" :description "Argon Fan HAT PWM driver")
    ("MISC_RP1" :type tristate :version "6.17" :description "Raspberry Pi RP1 misc driver")

    ;; Platform arch selections - these are typically choice options
    ("ARCH_ZYNQMP" :type bool :arch "arm64" :description "Xilinx ZynqMP platform")
    ("ARCH_XGENE" :type bool :arch "arm64" :description "Applied Micro X-Gene platform")
    ("ARCH_VISCONTI" :type bool :arch "arm64" :description "Toshiba Visconti platform")
    ("ARCH_TEGRA" :type bool :arch "arm64" :description "NVIDIA Tegra platform")
    ("ARCH_QCOM" :type bool :arch "arm64" :description "Qualcomm platform")
    ("ARCH_ROCKCHIP" :type bool :arch "arm64" :description "Rockchip platform")
    ("ARCH_RENESAS" :type bool :arch "arm64" :description "Renesas platform")
    ("ARCH_REALTEK" :type bool :arch "arm64" :description "Realtek platform")
    ("ARCH_MVEBU" :type bool :arch "arm64" :description "Marvell EBU platform")
    ("ARCH_MEDIATEK" :type bool :arch "arm64" :description "MediaTek platform")
    ("ARCH_HISI" :type bool :arch "arm64" :description "HiSilicon platform")
    ("ARCH_EXYNOS" :type bool :arch "arm64" :description "Samsung Exynos platform")
    ("ARCH_BCM" :type bool :arch "arm64" :description "Broadcom platform")
    ("ARCH_APPLE" :type bool :arch "arm64" :description "Apple Silicon platform")
    ("ARCH_ALPINE" :type bool :arch "arm64" :description "Annapurna Labs Alpine platform")

    ;; Additional ARM64 platform options found in RedHat configs
    ("ARCH_VEXPRESS" :type bool :arch "arm64" :description "ARM Versatile Express platform")
    ("ARCH_UNIPHIER" :type bool :arch "arm64" :description "Socionext UniPhier platform")
    ("ARCH_THUNDER2" :type bool :arch "arm64" :description "Cavium ThunderX2 platform")
    ("ARCH_THUNDER" :type bool :arch "arm64" :description "Cavium ThunderX platform")
    ("ARCH_SPRD" :type bool :arch "arm64" :description "Spreadtrum platform")
    ("ARCH_SYNQUACER" :type bool :arch "arm64" :description "Socionext SynQuacer platform")
    ("ARCH_STM32" :type bool :arch "arm64" :description "STMicroelectronics STM32 platform")
    ("ARCH_SOPHGO" :type bool :arch "arm64" :description "Sophgo platform")
    ("ARCH_INTEL_SOCFPGA" :type bool :arch "arm64" :description "Intel SoCFPGA platform")
    ("ARCH_SEATTLE" :type bool :arch "arm64" :description "AMD Seattle platform")
    ("ARCH_PENSANDO" :type bool :arch "arm64" :description "Pensando platform")
    ("ARCH_NPCM" :type bool :arch "arm64" :description "Nuvoton NPCM platform")
    ("ARCH_MA35" :type bool :arch "arm64" :description "Nuvoton MA35 platform")
    ("ARCH_S32" :type bool :arch "arm64" :description "NXP S32 platform")
    ("ARCH_MXC" :type bool :arch "arm64" :description "Freescale i.MX platform")
    ("ARCH_LAYERSCAPE" :type bool :arch "arm64" :description "NXP Layerscape platform")
    ("ARCH_NXP" :type bool :arch "arm64" :description "NXP platform")
    ("ARCH_MMP" :type bool :arch "arm64" :description "Marvell MMP platform")
    ("ARCH_MESON" :type bool :arch "arm64" :description "Amlogic Meson platform")
    ("ARCH_KEEMBAY" :type bool :arch "arm64" :description "Intel Keem Bay platform")
    ("ARCH_LG1K" :type bool :arch "arm64" :description "LG1K platform")
    ("ARCH_K3" :type bool :arch "arm64" :description "Texas Instruments K3 platform")
    ("ARCH_SPARX5" :type bool :arch "arm64" :description "Microchip Sparx5 platform")
    ("ARCH_CIX" :type bool :arch "arm64" :description "CIX platform")
    ("ARCH_BLAIZE" :type bool :arch "arm64" :description "Blaize platform")
    ("ARCH_BITMAIN" :type bool :arch "arm64" :description "Bitmain platform")
    ("ARCH_BERLIN" :type bool :arch "arm64" :description "Marvell Berlin platform")
    ("ARCH_AXIADO" :type bool :arch "arm64" :description "Axiado platform")
    ("ARCH_SUNXI" :type bool :arch "arm64" :description "Allwinner Sunxi platform")
    ("ARCH_AIROHA" :type bool :arch "arm64" :description "Airoha platform")
    ("ARCH_ACTIONS" :type bool :arch "arm64" :description "Actions Semiconductor platform")
    ("ARCH_TEGRA_264_SOC" :type bool :arch "arm64" :version "6.17" :description "NVIDIA Tegra 264 SoC support")

    ;; ARM64 CoreSight debugging and tracing infrastructure
    ("CORESIGHT" :type tristate :arch "arm64" :description "ARM CoreSight debug and trace infrastructure")
    ("CORESIGHT_LINKS_AND_SINKS" :type tristate :arch "arm64" :description "CoreSight links and sinks")
    ("CORESIGHT_LINK_AND_SINK_TMC" :type tristate :arch "arm64" :description "CoreSight Trace Memory Controller driver")
    ("CORESIGHT_CATU" :type tristate :arch "arm64" :description "CoreSight Cached Address Tracing Unit driver")
    ("CORESIGHT_SINK_TPIU" :type tristate :arch "arm64" :description "CoreSight Trace Port Interface Unit driver")
    ("CORESIGHT_SINK_ETBV10" :type tristate :arch "arm64" :description "CoreSight ETB v1.0 driver")
    ("CORESIGHT_SOURCE_ETM4X" :type tristate :arch "arm64" :description "CoreSight ETMv4.x driver")
    ("CORESIGHT_STM" :type tristate :arch "arm64" :description "CoreSight System Trace Macrocell driver")
    ("CORESIGHT_CTCU" :type tristate :arch "arm64" :version "6.17" :description "CoreSight Cross Trigger Coupling Unit")
    ("CORESIGHT_CPU_DEBUG" :type tristate :arch "arm64" :description "CoreSight CPU debug driver")
    ("CORESIGHT_CPU_DEBUG_DEFAULT_ON" :type bool :arch "arm64" :description "Turn on CoreSight CPU debug by default")
    ("CORESIGHT_CTI" :type tristate :arch "arm64" :description "CoreSight Cross Trigger Interface driver")
    ("CORESIGHT_CTI_INTEGRATION_REGS" :type bool :arch "arm64" :description "CoreSight CTI integration registers")
    ("CORESIGHT_TRBE" :type tristate :arch "arm64" :version "6.17" :description "CoreSight Trace Buffer Extension support")
    ("CORESIGHT_TPDM" :type tristate :arch "arm64" :version "6.17" :description "CoreSight Trace, Profiling & Diagnostics Monitor")
    ("CORESIGHT_TPDA" :type tristate :arch "arm64" :version "6.17" :description "CoreSight Trace, Profiling & Diagnostics Aggregator")
    ("CORESIGHT_DUMMY" :type tristate :arch "arm64" :version "6.17" :description "CoreSight dummy driver for testing")
    ("CORESIGHT_KUNIT_TESTS" :type tristate :arch "arm64" :version "6.17" :description "CoreSight KUnit tests")
    ("ETM4X_IMPDEF_FEATURE" :type bool :arch "arm64" :description "ETMv4.x implementation defined features")
    ("ULTRASOC_SMB" :type tristate :arch "arm64" :version "6.17" :description "UltraSoc System Memory Buffer driver")

    ;; Additional ARM64-specific options
    ("ARM64_RELOC_TEST" :type tristate :arch "arm64" :version "6.17" :description "ARM64 relocation test module")
    ("DEBUG_EFI" :type bool :arch "arm64" :description "EFI debug support")
    ("PID_IN_CONTEXTIDR" :type bool :arch "arm64" :description "Write the current PID to the CONTEXTIDR register")

    ;; Runtime verification and monitoring (ARM64-specific in 6.17)
    ("RV_MON_EVENTS" :type bool :version "6.17" :description "Runtime verification event monitoring")
    ("RV_MON_MAINTENANCE_EVENTS" :type bool :version "6.17" :description "Runtime verification maintenance events")
    ("RV_PER_TASK_MONITORS" :type tristate :version "6.17" :description "Runtime verification per-task monitors")
    ("RV_MON_RTAPP" :type tristate :version "6.17" :description "Runtime verification RT application monitor")

    ;; Miscellaneous ARM64 and version-specific options
    ("HAVE_EXTRA_IPI_TRACEPOINTS" :type bool :version "6.17" :description "Extra IPI tracepoints support")
    ("CRYPTO_LIB_SHA512_ARCH" :type bool :arch "arm64" :version "6.17" :description "Architecture-specific SHA512 library")
    ("CRYPTO_LIB_SHA512" :type bool :version "6.17" :description "SHA512 library support")
    ("CRYPTO_LIB_SHA256_ARCH" :type bool :arch "arm64" :version "6.17" :description "Architecture-specific SHA256 library")
    ("CRYPTO_LIB_SHA1_ARCH" :type bool :arch "arm64" :version "6.17" :description "Architecture-specific SHA1 library")
    ("KSTACK_ERASE" :type bool :version "6.17" :description "Kernel stack erasing on syscall exit")
    ("CC_HAS_SANCOV_STACK_DEPTH_CALLBACK" :type bool :version "6.17" :description "Compiler support for stack depth callback")
    ("HAVE_ARCH_KSTACK_ERASE" :type bool :arch "arm64" :description "Architecture supports kernel stack erasing")

    ;; Memory management options (6.17+)
    ("PAGE_BLOCK_MAX_ORDER" :type int :version "6.17" :range (1 . 10) :default 10 :description "Page Block Order Upper Limit")

    ;; ===== RHEL/Legacy Options Below =====
    ;; Options removed from mainline kernel or RHEL-specific

    ;; Removed/deprecated kernel options (no longer in mainline)
    ("CLEANCACHE" :type bool :deprecated "5.11" :description "Transcendent memory cache (removed in 5.11)")
    ("FRONTSWAP" :type bool :deprecated "5.11" :description "Transcendent memory swap support (removed in 5.11)")
    ("EMBEDDED" :type bool :deprecated "5.4" :description "Configure standard kernel features (renamed to EXPERT)")
    ("USELIB" :type bool :deprecated "5.1" :description "uselib syscall support (removed in 5.1)")
    ("DECNET" :type tristate :deprecated "6.1" :description "DECnet protocol support (removed in 6.1)")
    ("REISERFS_FS" :type tristate :deprecated "5.18" :description "Reiserfs filesystem (deprecated in 5.18)")
    ("SYSV_FS" :type tristate :deprecated "6.6" :description "System V filesystem support (deprecated)")
    ("EFI_VARS" :type bool :deprecated "5.5" :description "EFI variable support via sysfs (deprecated, use efivarfs)")
    ("GEN_RTC" :type bool :deprecated "5.5" :description "Generic /dev/rtc emulation (removed)")
    ("PRISM2_USB" :type tristate :deprecated "5.13" :description "Prism2 USB wireless driver (removed)")
    ("QLGE" :type tristate :deprecated "5.18" :description "QLogic QLGE 10Gb Ethernet driver (removed in 5.18)")
    ("BT_HS" :type bool :deprecated "5.8" :description "Bluetooth High Speed support (removed)")
    ("BPFILTER" :type bool :deprecated "6.1" :description "BPF-based packet filtering framework (removed)")
    ("MD_FAULTY" :type tristate :deprecated "5.15" :description "Faulty test module for MD (removed)")
    ("MD_MULTIPATH" :type tristate :deprecated "5.8" :description "Multipath I/O support (removed)")
    ("IXGB" :type tristate :deprecated "5.11" :description "Intel PRO/10GbE driver (removed)")
    ("VIDEO_MEYE" :type tristate :deprecated "5.9" :description "Sony Vaio Picturebook Motion Eye driver (removed)")
    ("USB_ZR364XX" :type tristate :deprecated "5.11" :description "USB ZR364XX camera support (removed)")
    ("SCSI_DPT_I2O" :type tristate :deprecated "5.2" :description "Adaptec I2O RAID driver (removed)")
    ("BLK_DEV_SX8" :type tristate :deprecated "5.11" :description "Promise SATA SX8 driver (removed)")
    ("BLK_DEV_RSXX" :type tristate :deprecated "5.9" :description "IBM Flash Adapter 900GB driver (removed)")
    ("NET_SB1000" :type tristate :deprecated "5.5" :description "General Instruments SB1000 driver (removed)")
    ("CDROM_PKTCDVD" :type tristate :deprecated "5.13" :description "Packet writing on CD/DVD media (removed)")
    ("CDROM_PKTCDVD_BUFFERS" :type int :deprecated "5.13" :description "Packet buffer count")
    ("CDROM_PKTCDVD_WCACHE" :type bool :deprecated "5.13" :description "Enable write caching")

    ;; Test and debug framework options
    ("TEST_USER_COPY" :type tristate :description "Test user copy functions")
    ("TEST_STRSCPY" :type tristate :description "Test strscpy functions")
    ("TEST_STRING_HELPERS" :type tristate :description "Test string helper functions")
    ("TEST_STACKINIT" :type tristate :description "Test stack variable initialization")
    ("TEST_SIPHASH" :type tristate :description "Test siphash hash function")
    ("TEST_SCANF" :type tristate :description "Test scanf implementation")
    ("TEST_PRINTF" :type tristate :description "Test printf implementation")
    ("TEST_OVERFLOW" :type tristate :description "Test arithmetic overflow checking")
    ("TEST_HASH" :type tristate :description "Test hash table implementation")
    ("TEST_CPUMASK" :type tristate :description "Test CPU mask operations")
    ("INT_POW_TEST" :type tristate :version "6.17" :description "Integer power function tests")
    ("KASAN_MODULE_TEST" :type tristate :description "KASAN module test")
    ("STRING_SELFTEST" :type tristate :description "String function self-tests")
    ("STRSCPY_KUNIT_TEST" :type tristate :description "strscpy KUnit tests")
    ("STRCAT_KUNIT_TEST" :type tristate :description "strcat KUnit tests")
    ("MEMCPY_SLOW_KUNIT_TEST" :type tristate :description "Slow memcpy KUnit tests")
    ("CRC16_KUNIT_TEST" :type tristate :description "CRC16 KUnit tests")
    ("CRC32_SELFTEST" :type tristate :description "CRC32 self-tests")
    ("XARRAY_KUNIT" :type tristate :version "6.17" :description "XArray data structure KUnit tests")
    ("PROVE_NVDIMM_LOCKING" :type bool :description "NVDIMM locking correctness checks")
    ("PROVE_CXL_LOCKING" :type bool :description "CXL locking correctness checks")

    ;; Crypto implementation and acceleration options
    ("CRYPTO_BLAKE2S" :type tristate :description "BLAKE2s digest algorithm")
    ("CRYPTO_CRC32C_INTEL" :type tristate :arch "x86" :description "CRC32c using x86 PCLMULQDQ instruction")
    ("CRYPTO_CRC32C_VPMSUM" :type tristate :arch "powerpc" :description "CRC32c using PowerPC VPMSUM instructions")
    ("CRYPTO_CRC32_PCLMUL" :type tristate :arch "x86" :description "CRC32 using x86 PCLMULQDQ instruction")
    ("CRYPTO_CRCT10DIF" :type tristate :description "CRCT10DIF algorithm (for T10 DIF)")
    ("CRYPTO_CRCT10DIF_PCLMUL" :type tristate :arch "x86" :description "CRCT10DIF using x86 PCLMULQDQ")
    ("CRYPTO_CRCT10DIF_VPMSUM" :type tristate :arch "powerpc" :description "CRCT10DIF using PowerPC VPMSUM")
    ("CRYPTO_DEV_HISTB_TRNG" :type tristate :deprecated "6.2" :description "HiSilicon STB TRNG driver (removed)")
    ("CRYPTO_KEYWRAP" :type tristate :description "Key wrapping algorithm (RFC 3394/5649)")
    ("CRYPTO_LIB_BLAKE2S" :type tristate :description "BLAKE2s library interface")
    ("CRYPTO_MANAGER_EXTRA_TESTS" :type bool :description "Extra self-tests for crypto manager")
    ("CRYPTO_POLY1305" :type tristate :description "Poly1305 authenticator algorithm")
    ("CRYPTO_SHA1_SSSE3" :type tristate :arch "x86" :description "SHA1 using x86 SSSE3 instructions")
    ("CRYPTO_SHA256_SSSE3" :type tristate :arch "x86" :description "SHA256 using x86 SSSE3 instructions")
    ("CRYPTO_SHA512_SSSE3" :type tristate :arch "x86" :description "SHA512 using x86 SSSE3 instructions")
    ("CRYPTO_SM2" :type tristate :description "SM2 elliptic curve public key algorithm")
    ("CRYPTO_SM3" :type tristate :description "SM3 hash algorithm")
    ("CRYPTO_STATS" :type bool :description "Crypto usage statistics")
    ("CRYPTO_VMAC" :type tristate :description "VMAC Message Authentication Code")

    ;; CRC implementation options
    ("CRC32_BIT" :type bool :description "CRC32 bit-by-bit implementation")
    ("CRC32_SARWATE" :type bool :description "CRC32 Sarwate (table-based) implementation")
    ("CRC32_SLICEBY4" :type bool :description "CRC32 slice-by-4 implementation")
    ("CRC32_SLICEBY8" :type bool :description "CRC32 slice-by-8 implementation")
    ("CRC32_IMPL_BIT" :type bool :description "Select bit-by-bit CRC32 implementation")
    ("CRC32_IMPL_SLICEBY1" :type bool :description "Select slice-by-1 CRC32 implementation")
    ("CRC32_IMPL_SLICEBY4" :type bool :description "Select slice-by-4 CRC32 implementation")
    ("CRC32_IMPL_SLICEBY8" :type bool :description "Select slice-by-8 CRC32 implementation")
    ("CRC32_IMPL_ARCH_PLUS_SLICEBY1" :type bool :description "Arch CRC32 with slice-by-1 fallback")
    ("CRC32_IMPL_ARCH_PLUS_SLICEBY8" :type bool :description "Arch CRC32 with slice-by-8 fallback")
    ("CRC64_ROCKSOFT" :type tristate :description "CRC64 Rocksoft parameters")
    ("CRC_T10DIF_IMPL_ARCH" :type bool :description "T10 DIF CRC architecture-specific implementation")
    ("CRC_T10DIF_IMPL_GENERIC" :type bool :description "T10 DIF CRC generic implementation")

    ;; RHEL-specific and vendor tuning options
    ("SPECULATION_MITIGATIONS" :type bool :vendor "RedHat" :description "CPU vulnerability mitigations master switch")
    ("PREEMPT_AUTO" :type bool :description "Automatic preemption mode selection")
    ("RANDOMIZE_IDENTITY_BASE" :type bool :description "Randomize process identity base")
    ("CMDLINE_FROM_BOOTLOADER" :type bool :description "Use kernel command line from bootloader")
    ("TOOLCHAIN_DEFAULT_CPU" :type string :description "Default CPU type for toolchain")
    ("BASE_FULL" :type bool :description "Full base kernel (non-embedded config)")

    ;; Architecture-specific: x86
    ("X86_5LEVEL" :type bool :arch "x86" :description "5-level paging support")
    ("X86_CMPXCHG64" :type bool :arch "x86" :description "64-bit CMPXCHG instruction support")
    ("X86_PLATFORM_DRIVERS_INTEL" :type bool :arch "x86" :description "Intel platform drivers")
    ("X86_X32" :type bool :arch "x86" :deprecated "6.3" :description "x32 ABI support (deprecated)")
    ("LEGACY_VSYSCALL_EMULATE" :type bool :arch "x86" :description "Emulate legacy vsyscalls")
    ("MK8" :type bool :arch "x86" :description "Optimize for AMD K8")
    ("MCORE2" :type bool :arch "x86" :description "Optimize for Intel Core 2")

    ;; Architecture-specific: PowerPC
    ("PPC_RTAS_FILTER" :type bool :arch "powerpc" :description "RTAS call filtering")
    ("PPC_QUEUED_SPINLOCKS" :type bool :arch "powerpc" :description "Queued spinlock implementation")
    ("PPC_PROT_SAO_LPAR" :type bool :arch "powerpc" :description "SAO protection for LPAR")
    ("PPC_MICROWATT" :type bool :arch "powerpc" :description "Microwatt softcore support")
    ("POWER10_CPU" :type bool :arch "powerpc" :description "POWER10 processor support")
    ("POWERPC64_CPU" :type bool :arch "powerpc" :description "Generic 64-bit PowerPC support")
    ("PMU_SYSFS" :type bool :arch "powerpc" :description "PMU sysfs interface")
    ("OPAL_CORE" :type bool :arch "powerpc" :description "OPAL core dump support")
    ("HTMDUMP" :type tristate :arch "powerpc" :description "Hardware transactional memory dumps")
    ("VPA_PMU" :type bool :arch "powerpc" :description "VPA PMU support")

    ;; Architecture-specific: S390
    ("S390_MODULES_SANITY_TEST" :type tristate :arch "s390" :description "S390 module sanity tests")
    ("S390_KPROBES_SANITY_TEST" :type tristate :arch "s390" :description "S390 kprobes sanity tests")
    ("MARCH_Z16" :type bool :arch "s390" :description "Optimize for z16 architecture")
    ("MARCH_Z17" :type bool :arch "s390" :description "Optimize for z17 architecture")
    ("TUNE_Z16" :type bool :arch "s390" :description "Tune for z16 architecture")
    ("TUNE_Z17" :type bool :arch "s390" :description "Tune for z17 architecture")

    ;; Architecture-specific: ARM64
    ("ARCH_BCM4908" :type bool :arch "arm64" :description "Broadcom BCM4908 platform")
    ("ROCKCHIP_ERRATUM_3588001" :type bool :arch "arm64" :description "Rockchip RK3588 erratum workaround")
    ("ROCKCHIP_ERRATUM_3568002" :type bool :arch "arm64" :description "Rockchip RK3568 erratum workaround")
    ("AMPERE_ERRATUM_AC04_CPU_23" :type bool :arch "arm64" :description "Ampere AC04 CPU erratum workaround")
    ("HISILICON_ERRATUM_162100801" :type bool :arch "arm64" :description "HiSilicon erratum 162100801 workaround")
    ("HIPERDISPATCH_ON" :type bool :arch "s390" :description "Hyper-dispatch on by default")

    ;; Security and SELinux options
    ("SECURITY_SELINUX_DISABLE" :type bool :description "Allow disabling SELinux at boot")
    ("SECURITY_SELINUX_CHECKREQPROT_VALUE" :type int :description "SELinux checkreqprot default value")
    ("LOCK_DOWN_IN_EFI_SECURE_BOOT" :type bool :vendor "rhel" :description "Lock down kernel in EFI Secure Boot mode")
    ("RANDOM_TRUST_CPU" :type bool :deprecated "6.2" :description "Trust CPU for RNG initialization (removed)")
    ("RANDOM_TRUST_BOOTLOADER" :type bool :deprecated "6.2" :description "Trust bootloader for RNG seed (removed)")
    ("AMD_MEM_ENCRYPT_ACTIVE_BY_DEFAULT" :type bool :arch "x86" :description "AMD memory encryption active by default")

    ;; Memory management options
    ("MEMCG_SWAP" :type bool :deprecated "6.1" :description "Memory cgroup swap extension (deprecated)")
    ("MEMORY_HOTPLUG_DEFAULT_ONLINE" :type bool :description "Memory hotplug default online")
    ("CMA_DEBUG" :type bool :description "CMA debug information")
    ("HUGETLB_PAGE_FREE_VMEMMAP_DEFAULT_ON" :type bool :description "Free HugeTLB vmemmap by default")
    ("RODATA_FULL_DEFAULT_ENABLED" :type bool :description "Full read-only data by default")
    ("STACK_HASH_ORDER" :type int :description "Stack hash order")
    ("STRICT_MM_TYPECHECKS" :type bool :description "Strict memory management type checks")

    ;; Additional crypto options
    ("CRYPTO_POLY1305_X86_64" :type tristate :arch "x86" :description "Poly1305 for x86_64")
    ("CRYPTO_CURVE25519_X86" :type tristate :arch "x86" :description "Curve25519 for x86")
    ("CRYPTO_CURVE25519" :type tristate :description "Curve25519 elliptic curve")
    ("CRYPTO_CHACHA20_X86_64" :type tristate :arch "x86" :description "ChaCha20 stream cipher for x86_64")
    ("CRYPTO_BLAKE2S_X86" :type tristate :arch "x86" :description "BLAKE2s for x86")

    ;; Debug and tracing options
    ("DEBUG_VM_VMACACHE" :type bool :deprecated "5.8" :description "VM cache debugging (removed)")
    ("DEBUG_TIMEKEEPING" :type bool :description "Timekeeping debugging")
    ("DEBUG_KMEMLEAK_TEST" :type tristate :description "Kmemleak testing module")
    ("DEBUG_INFO_COMPRESSED" :type bool :deprecated "5.18" :description "Compressed debug info (removed)")
    ("DEBUG_CREDENTIALS" :type bool :description "Debug credentials")
    ("DEBUG_ALIGN_RODATA" :type bool :deprecated "5.10" :description "Align rodata (removed)")
    ("FTRACE_MCOUNT_RECORD" :type bool :description "Record mcount call sites")
    ("SCHED_DEBUG" :type bool :description "Scheduler debugging")

    ;; Network and netfilter options
    ("NFT_OBJREF" :type tristate :deprecated "5.18" :description "Nftables object reference (removed)")
    ("NFT_COUNTER" :type tristate :deprecated "5.16" :description "Nftables counter (now built-in)")
    ("NFSD_V3" :type bool :deprecated "6.2" :description "NFSv3 server support (always enabled)")
    ("NF_FLOW_TABLE_IPV6" :type tristate :deprecated "5.12" :description "IPv6 flow table (merged into NF_FLOW_TABLE)")
    ("NF_FLOW_TABLE_IPV4" :type tristate :deprecated "5.12" :description "IPv4 flow table (merged into NF_FLOW_TABLE)")
    ("NF_CT_PROTO_DCCP" :type bool :deprecated "5.13" :description "DCCP connection tracking protocol support")
    ("IP_NF_TARGET_CLUSTERIP" :type tristate :deprecated "6.3" :description "CLUSTERIP target (removed)")
    ("IP_DCCP" :type tristate :description "DCCP protocol support")

    ;; Mellanox network driver options
    ("MLX5_TLS" :type bool :description "Mellanox 5th generation TLS offload")
    ("MLX5_IPSEC" :type bool :description "Mellanox 5th generation IPsec offload")
    ("MLX5_FPGA_TLS" :type bool :deprecated "5.16" :description "Mellanox FPGA TLS (removed)")
    ("MLX5_FPGA_IPSEC" :type bool :deprecated "5.16" :description "Mellanox FPGA IPsec (removed)")
    ("MLX5_ACCEL" :type bool :description "Mellanox acceleration library")

    ;; PSTORE compression options
    ("PSTORE_ZSTD_COMPRESS" :type bool :description "PSTORE zstd compression")
    ("PSTORE_LZO_COMPRESS" :type bool :description "PSTORE LZO compression")
    ("PSTORE_LZ4HC_COMPRESS" :type bool :description "PSTORE LZ4HC compression")
    ("PSTORE_LZ4_COMPRESS" :type bool :description "PSTORE LZ4 compression")
    ("PSTORE_DEFLATE_COMPRESS" :type bool :description "PSTORE deflate compression")
    ("PSTORE_DEFLATE_COMPRESS_DEFAULT" :type bool :description "PSTORE deflate as default")
    ("PSTORE_842_COMPRESS" :type bool :description "PSTORE 842 compression")

    ;; DRM/Graphics options
    ("DRM_XE_SIMPLE_ERROR_CAPTURE" :type bool :version "6.8" :description "Intel Xe simple error capture")
    ("DRM_XE_LARGE_GUC_BUFFER" :type bool :version "6.8" :description "Intel Xe large GuC buffer")
    ("DRM_XE_DEVMEM_MIRROR" :type bool :version "6.8" :description "Intel Xe device memory mirroring")
    ("DRM_VMWGFX_FBCON" :type bool :description "VMware SVGA framebuffer console")
    ("DRM_LEGACY" :type bool :deprecated "6.8" :description "Legacy DRM support (deprecated)")
    ("DRM_I2C_SIL164" :type tristate :deprecated "6.6" :description "Silicon Image sil164 TMDS transmitter (removed)")
    ("DRM_I2C_NXP_TDA9950" :type tristate :deprecated "6.6" :description "NXP TDA9950 CEC driver (removed)")
    ("DRM_I2C_CH7006" :type tristate :deprecated "6.6" :description "Chrontel ch7006 TV encoder (removed)")
    ("DRM_DP_CEC" :type bool :description "DisplayPort CEC-Tunneling-over-AUX support")
    ("DRM_DP_AUX_CHARDEV" :type bool :description "DRM DP AUX character device")
    ("DRM_DISPLAY_DEBUG_DP_TUNNEL_STATE" :type bool :version "6.10" :description "DP tunnel state debugging")
    ("DRM_DEBUG_SELFTEST" :type tristate :description "DRM self-tests")
    ("DRM_AMD_DC_HDCP" :type bool :description "AMD Display Core HDCP support")

    ;; DAMON (Data Access Monitor) options
    ("DAMON_DBGFS" :type bool :deprecated "6.9" :description "DAMON debugfs interface (deprecated)")
    ("DAMON_DBGFS_DEPRECATED" :type bool :deprecated "6.9" :description "DAMON debugfs deprecation notice")

    ;; Miscellaneous subsystem options
    ("ARCH_RANDOM" :type bool :description "Architecture random number generator")
    ("ASYMMETRIC_TPM_KEY_SUBTYPE" :type tristate :description "TPM-based asymmetric keys")
    ("BCACHEFS_FS" :type tristate :version "6.7" :description "Bcachefs filesystem")
    ("BLK_DEV_THROTTLING_LOW" :type bool :deprecated "5.0" :description "Block throttling low limit (removed)")
    ("BT_HCIBTUSB_AUTO_ISOC_ALT" :type bool :deprecated "6.1" :description "Bluetooth HCI USB auto isoc (removed)")
    ("CAN_ESD_USB2" :type tristate :deprecated "5.17" :description "ESD USB/2 CAN adapter (removed)")
    ("COMMAND_LINE_SIZE" :type int :description "Kernel command line buffer size")
    ("CPU_LITTLE_ENDIAN" :type bool :description "Little-endian CPU")
    ("CPU5_WDT" :type tristate :deprecated "5.5" :description "CPU5 watchdog (removed)")
    ("DLM_DEPRECATED_API" :type bool :deprecated "6.2" :description "DLM deprecated API (removed)")
    ("ECHO" :type tristate :deprecated "5.8" :description "Line echo canceller for mISDN (removed)")
    ("EFI_FAKE_MEMMAP" :type bool :description "EFI fake memory map")
    ("FB_DA8XX" :type tristate :deprecated "6.8" :description "DA8xx/OMAP-L1xx framebuffer (removed)")
    ("FS_VERITY_DEBUG" :type bool :description "FS-verity debugging")
    ("FS_PID" :type bool :version "6.8" :description "PID namespace filesystem")
    ("FTRACE_MCOUNT_RECORD" :type bool :description "Record mcount call sites for dynamic ftrace")
    ("GENERIC_CPU" :type bool :description "Generic CPU support")
    ("KALLSYMS_BASE_RELATIVE" :type bool :description "Use relative offsets in kallsyms")
    ("MEAN_AND_VARIANCE_UNIT_TEST" :type tristate :version "6.7" :description "Mean and variance unit test")
    ("MDIO_DEVICE" :type tristate :deprecated "5.13" :description "MDIO device (now auto-selected)")
    ("PRINTK_SAFE_LOG_BUF_SHIFT" :type int :deprecated "5.10" :description "Printk safe buffer size (removed)")
    ("RUST_EXTRA_LOCKDEP" :type bool :version "6.10" :description "Extra lockdep checks for Rust code")
    ("SCHED_TOPOLOGY_VERTICAL" :type bool :arch "s390" :description "Vertical CPU topology for scheduling")
    ("SYSFS_DEPRECATED" :type bool :deprecated "5.12" :description "Create deprecated sysfs layout")
    ("THERMAL_WRITABLE_TRIPS" :type bool :description "Allow writing to thermal trip points")
    ("XEN_SAVE_RESTORE" :type bool :description "Xen save/restore support")

    ;; Sound/Audio subsystem options
    ("SND_VERBOSE_PRINTK" :type bool :deprecated "5.2" :description "Verbose printk for sound (removed)")
    ("SND_SOC_SOF_IMX8_SUPPORT" :type bool :description "SOF support for i.MX8")
    ("SND_SOC_SOF_IMX8M_SUPPORT" :type bool :description "SOF support for i.MX8M")
    ("SND_SOC_INTEL_SST" :type tristate :deprecated "5.18" :description "Intel SST audio (removed)")
    ("SND_SOC_INTEL_SOF_DA7219_MAX98373_MACH" :type tristate :description "SOF with DA7219 and MAX98373")
    ("SND_SOC_INTEL_SKYLAKE" :type tristate :deprecated "6.8" :description "Intel Skylake audio (deprecated)")
    ("SND_SOC_INTEL_SKYLAKE_HDAUDIO_CODEC" :type bool :deprecated "6.8" :description "Skylake HDAUDIO codec")
    ("SND_SOC_INTEL_SKL_RT286_MACH" :type tristate :deprecated "6.8" :description "Skylake with RT286")
    ("SND_SOC_INTEL_SKL_NAU88L25_SSM4567_MACH" :type tristate :deprecated "6.8" :description "Skylake with NAU88L25 and SSM4567")
    ("SND_SOC_INTEL_SKL_NAU88L25_MAX98357A_MACH" :type tristate :deprecated "6.8" :description "Skylake with NAU88L25 and MAX98357A")
    ("SND_SOC_INTEL_KBL_RT5663_RT5514_MAX98927_MACH" :type tristate :deprecated "6.8" :description "Kabylake with RT5663, RT5514, MAX98927")
    ("SND_SOC_INTEL_KBL_RT5663_MAX98927_MACH" :type tristate :deprecated "6.8" :description "Kabylake with RT5663 and MAX98927")
    ("SND_SOC_INTEL_KBL_RT5660_MACH" :type tristate :deprecated "6.8" :description "Kabylake with RT5660")
    ("SND_SOC_INTEL_KBL_DA7219_MAX98927_MACH" :type tristate :deprecated "6.8" :description "Kabylake with DA7219 and MAX98927")
    ("SND_SOC_INTEL_KBL_DA7219_MAX98357A_MACH" :type tristate :deprecated "6.8" :description "Kabylake with DA7219 and MAX98357A")
    ("SND_SOC_INTEL_CML_LP" :type tristate :deprecated "6.8" :description "Cometlake LP audio")
    ("SND_SOC_INTEL_CML_H" :type tristate :deprecated "6.8" :description "Cometlake H audio")
    ("SND_SOC_INTEL_BXT_RT298_MACH" :type tristate :deprecated "6.8" :description "Broxton with RT298")
    ("SND_SOC_INTEL_BXT_DA7219_MAX98357A_MACH" :type tristate :deprecated "6.8" :description "Broxton with DA7219 and MAX98357A")
    ("SND_SOC_IMX_SPDIF" :type tristate :description "i.MX SPDIF machine driver")
    ("SND_SOC_IMG" :type tristate :deprecated "5.14" :description "Imagination Technologies SoC audio (removed)")
    ("SND_SOC_ADI" :type tristate :description "Analog Devices SoC audio")
    ("SND_CTL_VALIDATION" :type bool :description "Sound control interface validation")
    ("SND_ATMEL_SOC" :type tristate :deprecated "6.5" :description "Atmel SoC audio (removed)")

    ;; Video/Media drivers
    ("VIDEO_VS6624" :type tristate :deprecated "5.14" :description "VS6624 sensor (removed)")
    ("VIDEO_V4L2" :type tristate :deprecated "5.7" :description "V4L2 core (now auto-selected)")
    ("VIDEO_TM6000" :type tristate :deprecated "5.14" :description "TM6000 USB TV (removed)")
    ("VIDEO_TM6000_ALSA" :type tristate :deprecated "5.14" :description "TM6000 ALSA audio (removed)")
    ("VIDEO_ST_VGXY61" :type tristate :version "6.2" :description "ST VGXY61 image sensor")
    ("VIDEO_STK1160_COMMON" :type tristate :deprecated "5.9" :description "STK1160 common (removed)")
    ("VIDEO_SR030PC30" :type tristate :deprecated "5.9" :description "SR030PC30 sensor (removed)")
    ("VIDEO_S5K6AA" :type tristate :deprecated "5.9" :description "S5K6AA sensor (removed)")
    ("VIDEO_S5K4ECGX" :type tristate :deprecated "5.9" :description "S5K4ECGX sensor (removed)")
    ("VIDEO_NOON010PC30" :type tristate :deprecated "5.9" :description "NOON010PC30 sensor (removed)")
    ("VIDEO_MT9T001" :type tristate :deprecated "5.14" :description "MT9T001 sensor (removed)")
    ("VIDEO_MT9M032" :type tristate :deprecated "5.14" :description "MT9M032 sensor (removed)")
    ("VIDEO_M5MOLS" :type tristate :deprecated "5.9" :description "M5MOLS sensor (removed)")
    ("VIDEO_CPIA2" :type tristate :deprecated "5.11" :description "CPiA2 video for Linux (removed)")
    ("VIDEO_AD9389B" :type tristate :deprecated "5.14" :description "AD9389B encoder (removed)")
    ("USB_STKWEBCAM" :type tristate :deprecated "5.14" :description "USB Syntek webcam (removed)")
    ("USB_NET_RNDIS_WLAN" :type tristate :deprecated "5.12" :description "RNDIS WLAN driver (removed)")

    ;; Sensor/Hardware monitoring drivers
    ("SENSORS_SMM665" :type tristate :deprecated "6.4" :description "SMM665 hardware monitor (removed)")
    ("SENSORS_SBRMI" :type tristate :version "6.0" :description "AMD SB-RMI sensor")
    ("SENSORS_OXP" :type tristate :version "6.4" :description "OneXPlayer mini-PC sensors")
    ("SENSORS_MAX6642" :type tristate :deprecated "5.13" :description "MAX6642 sensor (removed)")
    ("SENSORS_ASUS_WMI_EC" :type tristate :version "5.17" :description "ASUS WMI EC sensors")
    ("SENSORS_ADM1021" :type tristate :deprecated "6.2" :description "ADM1021 sensor (removed)")

    ;; Serial/UART drivers
    ("SERIAL_SC16IS7XX_CORE" :type tristate :description "SC16IS7xx serial core")
    ("SERIAL_KGDB_NMI" :type bool :description "KGDB NMI serial console")

    ;; I2C drivers
    ("I2C_NFORCE2_S4985" :type tristate :deprecated "5.5" :description "nForce2-S4985 I2C (removed)")
    ("I2C_MULTI_INSTANTIATE" :type tristate :description "I2C multi instantiate helper")
    ("I2C_COMPAT" :type bool :deprecated "5.2" :description "I2C compatibility layer (removed)")
    ("I2C_AMD756_S4882" :type tristate :deprecated "5.5" :description "AMD756-S4882 I2C (removed)")

    ;; Touchscreen drivers
    ("TOUCHSCREEN_MCS5000" :type tristate :deprecated "5.12" :description "MCS5000 touchscreen (removed)")
    ("TOUCHSCREEN_CYTTSP4_CORE" :type tristate :deprecated "5.17" :description "Cypress CYTTSP4 core (removed)")

    ;; Input/Keyboard/Mouse drivers
    ("INPUT_EVBUG" :type tristate :description "Event debugging")
    ("KEYBOARD_MCS" :type tristate :deprecated "5.12" :description "MCS keyboard (removed)")
    ("KEYBOARD_ADP5589" :type tristate :deprecated "5.17" :description "ADP5589 I/O expander keyboard (removed)")
    ("MOUSE_PS2_PIXART" :type bool :version "6.9" :description "Pixart PS/2 touchpad protocol")

    ;; GPIO drivers
    ("GPIO_ADP5588" :type tristate :deprecated "5.17" :description "ADP5588 I/O expander (removed)")

    ;; MFD (Multi-Function Device) drivers
    ("MFD_RK808" :type tristate :description "Rockchip RK808/RK818 PMIC")
    ("MFD_PCF50633" :type tristate :deprecated "6.3" :description "NXP PCF50633 (removed)")
    ("MFD_MAX597X" :type tristate :version "6.7" :description "Maxim MAX597x PMIC")
    ("MFD_INTEL_M10_BMC" :type tristate :description "Intel MAX10 BMC")

    ;; MTD (Memory Technology Device) options
    ("MTD_INTEL_VR_NOR" :type tristate :version "5.11" :description "Intel Vendor Defined NOR flash")
    ("MTD_AR7_PARTS" :type tristate :deprecated "5.13" :description "TI AR7 partitioning (removed)")

    ;; Network drivers and protocols
    ("WLAN_VENDOR_CISCO" :type bool :description "Cisco wireless devices")
    ("RTL8192U" :type tristate :deprecated "5.17" :description "Realtek RTL8192U (removed)")
    ("IWLWIFI_BCAST_FILTERING" :type bool :description "Intel wireless broadcast filtering")
    ("INFINIBAND_QIB" :type tristate :deprecated "6.8" :description "QLogic InfiniBand (removed)")
    ("INFINIBAND_HNS" :type tristate :description "HiSilicon Hip06 SoC InfiniBand")

    ;; RTC drivers
    ("RTC_DRV_V3020" :type tristate :deprecated "6.4" :description "V3020 RTC (removed)")

    ;; PHY drivers
    ("PHY_QCOM_SNPS_EUSB2" :type tristate :version "6.8" :description "Qualcomm SNPS eUSB2 PHY")

    ;; Power management drivers
    ("PDA_POWER" :type tristate :deprecated "5.12" :description "PDA power driver (removed)")

    ;; PCI drivers
    ("PCI_PWRCTL_SLOT" :type tristate :version "6.10" :description "PCI power control for slot")

    ;; IIO (Industrial I/O) drivers
    ("ROHM_BU27008" :type tristate :version "6.5" :description "ROHM BU27008 color sensor")

    ;; Platform drivers
    ("YOGABOOK_WMI" :type tristate :version "6.4" :description "Lenovo Yoga Book WMI driver")
    ("PEAQ_WMI" :type tristate :deprecated "6.3" :description "PEAQ WMI hotkeys (removed)")
    ("TINYDRM_ST7735R" :type tristate :deprecated "5.11" :description "ST7735R TFT driver (removed)")
    ("TINYDRM_ST7586" :type tristate :deprecated "5.11" :description "ST7586 TFT driver (removed)")
    ("TI_ST" :type tristate :deprecated "5.17" :description "Texas Instruments shared transport (removed)")
    ("PI433" :type tristate :deprecated "5.12" :description "Pi433 radio module (removed)")
    ("NOUVEAU_LEGACY_CTX_SUPPORT" :type bool :deprecated "6.3" :description "Nouveau legacy context support (removed)")

    ;; UIO drivers
    ("UIO_PRUSS" :type tristate :deprecated "5.13" :description "PRUSS UIO driver (removed)")

    ;; Staging/legacy hardware drivers
    ("ADIS16240" :type tristate :deprecated "5.12" :description "ADIS16240 accelerometer (removed)")
    ("ADE7854" :type tristate :deprecated "5.12" :description "ADE7854 energy meter (removed)")
    ("HTC_PASIC3" :type tristate :deprecated "5.12" :description "HTC PASIC3 LED/DS1WM chip (removed)")
    ("HTC_I2CPLD" :type tristate :deprecated "5.12" :description "HTC I2C PLD chip (removed)")
    ("MPSC" :type bool :deprecated "5.2" :description "Marvell MPSC serial (removed)")
    ("GS_FPGABOOT" :type tristate :deprecated "5.12" :description "Xilinx FPGA firmware download (removed)")

    ;; Miscellaneous hardware options
    ("ACPI_CUSTOM_METHOD" :type bool :description "ACPI custom method support")
    ("TPM_KEY_PARSER" :type bool :deprecated "6.10" :description "TPM key parser (removed)")
    ("MODULE_SIG_SHA224" :type bool :description "Sign kernel modules with SHA-224")
    ("MICROCODE_INTEL" :type tristate :deprecated "6.4" :description "Intel microcode (now built-in)")
    ("MICROCODE_AMD" :type tristate :deprecated "6.4" :description "AMD microcode (now built-in)")
    ("MITIGATION_GDS_FORCE" :type bool :description "Force GDS mitigation")
    ("LEDS_TRIGGER_AUDIO" :type tristate :description "LED trigger for audio mute/micmute")
    ("KVM_MMU_AUDIT" :type bool :description "KVM MMU auditing")
    ("IMA_TEMPLATE" :type string :deprecated "5.1" :description "IMA default template (removed)")
    ("HW_CONSOLE" :type bool :description "Hardware console support")
    ("HABANA_AI" :type tristate :version "5.6" :description "Habana AI accelerators")
    ("GUEST_STATE_BUFFER_TEST" :type tristate :version "6.10" :description "Guest state buffer test module")
    ("QCOM_QFPROM" :type tristate :description "Qualcomm QFPROM support")
    ("PAGE_BLOCK_ORDER" :type int :deprecated "6.17" :description "Page block order (renamed to PAGE_BLOCK_MAX_ORDER)")
    ("INTERRUPT_SANITIZE_REGISTERS" :type bool :version "6.9" :description "Sanitize registers on interrupt entry")
    ("SUNRPC_DISABLE_INSECURE_ENCTYPES" :type bool :description "Disable insecure RPC encryption types")
    ("RPCSEC_GSS_KRB5_ENCTYPES_DES" :type bool :deprecated "5.10" :description "RPC GSS Kerberos DES (removed)")
    ("XZ_DEC_IA64" :type bool :arch "ia64" :deprecated "5.8" :description "XZ decompressor for IA64 (removed)")
    ("XMON_DEFAULT_RO_MODE" :type bool :arch "powerpc" :description "XMON default read-only mode")
    ("ZSWAP_ZPOOL_DEFAULT_Z3FOLD" :type bool :description "Use z3fold as default zswap pool")
    ("ZSWAP_ZPOOL_DEFAULT_Z3FOLD_DEPRECATED" :type bool :deprecated "6.2" :description "Z3fold default deprecated")
    ("ZSWAP_EXCLUSIVE_LOADS_DEFAULT_ON" :type bool :version "6.5" :description "Exclusive loads on by default")
    ("ZBUD" :type tristate :deprecated "6.7" :description "Zbud allocator (removed)")
    ("Z3FOLD" :type tristate :deprecated "6.7" :description "Z3fold allocator (removed)")
    ("Z3FOLD_DEPRECATED" :type bool :deprecated "6.7" :description "Z3fold deprecated notice"))
  "List of vendor-specific and version-specific Kconfig options.
Each entry is (OPTION-NAME :key value ...) with possible keys:
:type - Option type (bool, tristate, string, int, hex)
:vendor - Vendor name (RedHat, SUSE, etc.)
:version - Minimum kernel version
:arch - Required architecture
:description - Human-readable description")

(defun linconf-is-vendor-specific-option (option)
  "Check if OPTION is a known vendor-specific option.
Returns option info if found, nil otherwise."
  (assoc option linconf-vendor-specific-options))

(defun linconf-create-phantom-vendor-option (option option-info)
  "Create a phantom entry for a vendor-specific option."
  (let ((type (plist-get (cdr option-info) :type))
        (description (plist-get (cdr option-info) :description))
        (vendor (plist-get (cdr option-info) :vendor))
        (version (plist-get (cdr option-info) :version))
        (arch (plist-get (cdr option-info) :arch)))
    (puthash option
             (list :type type
                   :help (format "%s%s%s%s"
                               (or description (format "Vendor-specific option: %s" option))
                               (if vendor (format " (Vendor: %s)" vendor) "")
                               (if version (format " (Min version: %s)" version) "")
                               (if arch (format " (Architecture: %s)" arch) ""))
                   :phantom t
                   :vendor-specific t
                   :vendor vendor
                   :min-version version
                   :required-arch arch)
             linconf-kconfig-options)))

(defun linconf-detect-config-kernel-version (config-file)
  "Detect kernel version from config file header.
Returns version string or nil if not found."
  (when (file-readable-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file nil 0 1000) ; Read first 1000 chars
      (goto-char (point-min))
      (cond
       ;; Look for "Linux/arch X.Y.Z Kernel Configuration" format
       ((re-search-forward "Linux/[a-z0-9_]+ \\([0-9]+\\.[0-9]+\\.[0-9]+[^[:space:]]*\\) Kernel Configuration" nil t)
        (match-string 1))
       ;; Look for standalone version patterns like "6.17.0-rc6"
       ((re-search-forward "^# [0-9]+\\.[0-9]+\\.[0-9]+[^[:space:]]*$" nil t)
        (substring (match-string 0) 2)) ; Remove "# " prefix
       (t nil)))))

(defun linconf-detect-source-kernel-version (kernel-source-path)
  "Detect kernel version from source tree Makefile.
Returns version string or nil if not found."
  (when (and kernel-source-path (file-directory-p kernel-source-path))
    (let ((makefile (expand-file-name "Makefile" kernel-source-path)))
      (when (file-readable-p makefile)
        (with-temp-buffer
          (insert-file-contents makefile nil 0 2000) ; Read first 2000 chars
          (goto-char (point-min))
          (let (version patchlevel sublevel extraversion)
            (when (re-search-forward "^VERSION = \\([0-9]+\\)" nil t)
              (setq version (match-string 1)))
            (when (re-search-forward "^PATCHLEVEL = \\([0-9]+\\)" nil t)
              (setq patchlevel (match-string 1)))
            (when (re-search-forward "^SUBLEVEL = \\([0-9]+\\)" nil t)
              (setq sublevel (match-string 1)))
            (when (re-search-forward "^EXTRAVERSION = \\(.*\\)" nil t)
              (setq extraversion (match-string 1)))
            (when (and version patchlevel sublevel)
              (concat version "." patchlevel "." sublevel
                      (if (and extraversion (not (string-empty-p extraversion)))
                          extraversion "")))))))))

(defun linconf-compare-kernel-versions (version1 version2)
  "Compare two kernel version strings.
Returns -1 if version1 < version2, 0 if equal, 1 if version1 > version2.
Handles formats like '6.16.5', '6.17.0-rc6', etc."
  (if (or (null version1) (null version2))
      0 ; Treat nil as equal
    (let* ((v1-parts (split-string (replace-regexp-in-string "-.*" "" version1) "\\."))
           (v2-parts (split-string (replace-regexp-in-string "-.*" "" version2) "\\."))
           (max-parts (max (length v1-parts) (length v2-parts))))
      (catch 'done
        (dotimes (i max-parts)
          (let ((part1 (if (< i (length v1-parts))
                          (string-to-number (nth i v1-parts)) 0))
                (part2 (if (< i (length v2-parts))
                          (string-to-number (nth i v2-parts)) 0)))
            (cond
             ((< part1 part2) (throw 'done -1))
             ((> part1 part2) (throw 'done 1)))))
        0)))) ; All parts equal

(defun linconf-check-kernel-version-compatibility ()
  "Check compatibility between config file and kernel source versions.
Display warnings if versions are mismatched and return compatibility info."
  (when (and buffer-file-name linconf-kernel-source-path)
    (let ((config-version (linconf-detect-config-kernel-version buffer-file-name))
          (source-version (linconf-detect-source-kernel-version linconf-kernel-source-path)))
      (when (and config-version source-version)
        (let ((comparison (linconf-compare-kernel-versions source-version config-version)))
          (cond
           ((< comparison 0) ; source older than config
            (message "⚠ WARNING: Kernel source (%s) is older than config (%s). Some options may be missing."
                     source-version config-version)
            (list :status 'source-older :config-version config-version :source-version source-version))
           ((> comparison 0) ; source newer than config
            (message "ℹ INFO: Kernel source (%s) is newer than config (%s). Validation should be complete."
                     source-version config-version)
            (list :status 'source-newer :config-version config-version :source-version source-version))
           (t ; versions match
            (message "✓ Kernel source (%s) matches config version (%s)."
                     source-version config-version)
            (list :status 'compatible :config-version config-version :source-version source-version)))))
      (when (and (not config-version) (not source-version))
        (message "⚠ Could not determine kernel versions for compatibility check")
        (list :status 'unknown))
      (when config-version
        (list :status 'config-only :config-version config-version))
      (when source-version
        (list :status 'source-only :source-version source-version)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.config\\'" . linconf-mode))

(provide 'linconf)

;;; linconf.el ends here
