;;; kconfig.el --- Linux Kernel Kconfig Parser and Validator -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Clark Williams <clrkwllms@kernel.org>

;; Author: Generated with Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: tools, kernel, kconfig
;; URL:

;;; Commentary:

;; This package provides Kconfig parsing, validation, and query services
;; for Linux kernel configuration systems. It can:
;;
;; - Parse Kconfig files from kernel source trees
;; - Validate configuration option values against Kconfig definitions
;; - Evaluate dependency expressions
;; - Handle select chains and circular dependencies
;; - Support vendor-specific options (643 option database)
;; - Detect architecture and kernel versions
;;
;; Main entry points:
;;   (kconfig-load-data &optional target-arch) - Load Kconfig from kernel source
;;   (kconfig-validate-option-value option value) - Validate an option value
;;   (kconfig-get-config-value option) - Get current configuration value
;;   (kconfig-set-config-value option value) - Set configuration value
;;   (kconfig-evaluate-condition condition) - Evaluate dependency condition
;;
;; This package is designed to be used by major modes like linconf-mode,
;; but can also be used standalone for Kconfig validation, linting, or
;; kernel build automation.

;;; Code:

;;
;; Customization
;;

(defgroup kconfig nil
  "Linux kernel Kconfig parser and validator."
  :group 'tools
  :prefix "kconfig-")

(defcustom kconfig-kernel-source-path nil
  "Path to kernel source tree for Kconfig parsing.
If nil, Kconfig parsing will be disabled."
  :type '(choice (const :tag "No kernel source" nil)
                 (directory :tag "Kernel source directory"))
  :group 'kconfig)

(defcustom kconfig-cache-file "~/.emacs.d/kconfig-cache.el"
  "File to cache parsed Kconfig data."
  :type 'file
  :group 'kconfig)

;;
;; Data Structures
;;

(defvar kconfig-options (make-hash-table :test 'equal)
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

(defvar kconfig-config-values (make-hash-table :test 'equal)
  "Hash table storing current configuration values.
Keys are option names, values are configuration values.")

(defvar kconfig-loaded nil
  "Non-nil if Kconfig data has been loaded.")

(defvar kconfig-main-menu-title nil
  "Main menu title from mainmenu directive.")

(defvar kconfig-detected-architecture nil
  "Architecture detected from config file or kernel source.
Set automatically when Kconfig data is loaded.")

;;
;; Internal parsing state
;;

(defvar kconfig-token-pos 0
  "Current position in token stream for condition parsing.")

(defvar kconfig-token-stream nil
  "Token stream for condition parsing.")

;;
;; Public API - Configuration Value Access
;;

(defun kconfig-set-config-value (option value)
  "Set configuration value for OPTION to VALUE."
  (puthash option value kconfig-config-values))

(defun kconfig-get-config-value (option)
  "Get current configuration value for OPTION."
  (gethash option kconfig-config-values))

;;
;; Utility Functions
;;

(defun kconfig-relative-source-path (source-file)
  "Return SOURCE-FILE path relative to kernel source root for display.
If SOURCE-FILE is absolute and within kernel source tree, return relative path.
If SOURCE-FILE is already relative, return as-is.
Otherwise return the filename only."
  (when source-file
    (cond
     ((not (file-name-absolute-p source-file)) source-file)
     ((and kconfig-kernel-source-path
           (string-prefix-p (expand-file-name kconfig-kernel-source-path) source-file))
      (file-relative-name source-file kconfig-kernel-source-path))
     (t (file-name-nondirectory source-file)))))

(defun kconfig-detect-source-version (kernel-source-path)
  "Detect kernel version from source tree Makefile.
Returns version string or nil if not found."
  (when (and kernel-source-path (file-directory-p kernel-source-path))
    (let ((makefile (expand-file-name "Makefile" kernel-source-path)))
      (when (file-readable-p makefile)
        (with-temp-buffer
          (insert-file-contents makefile nil 0 2000)
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

(defun kconfig-compare-versions (version1 version2)
  "Compare two kernel version strings.
Returns -1 if version1 < version2, 0 if equal, 1 if version1 > version2.
Handles formats like '6.16.5', '6.17.0-rc6', etc."
  (if (or (null version1) (null version2))
      0
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
        0))))

;;
;; Dependency Evaluation Functions
;;

(defun kconfig-evaluate-condition (condition)
  "Evaluate a Kconfig condition expression.
CONDITION is a string like 'X86 && PCI' or '!ARM || (X86_64 && SMP)'.
Returns t if condition is true, nil if false, 'unknown if cannot determine."
  (when (and condition (> (length (string-trim condition)) 0))
    (let ((expr (string-trim condition)))
      (cond
       ((string-match "^!?[A-Z0-9_]+$" expr)
        (let* ((negated (string-prefix-p "!" expr))
               (option (if negated (substring expr 1) expr))
               (value (gethash option kconfig-config-values 'unknown)))
          (cond
           ((eq value 'unknown) 'unknown)
           ((eq value t) (not negated))
           ((eq value nil) negated)
           (t (if negated nil t)))))
       (t (kconfig-parse-condition-expr expr))))))

(defun kconfig-parse-condition-expr (expr)
  "Parse and evaluate complex condition expression EXPR."
  (let ((tokens (kconfig-tokenize-condition expr)))
    (kconfig-eval-condition-tokens tokens)))

(defun kconfig-tokenize-condition (expr)
  "Tokenize condition expression EXPR into list of tokens."
  (let ((tokens '())
        (i 0)
        (len (length expr))
        current-token)
    (while (< i len)
      (let ((char (aref expr i)))
        (cond
         ((memq char '(?\s ?\t))
          (when current-token
            (push (nreverse current-token) tokens)
            (setq current-token nil))
          (setq i (1+ i)))
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
         ((memq char '(?\( ?\) ?!))
          (when current-token
            (push (nreverse current-token) tokens)
            (setq current-token nil))
          (push (string char) tokens)
          (setq i (1+ i)))
         (t
          (push char current-token)
          (setq i (1+ i))))))
    (when current-token
      (push (nreverse current-token) tokens))
    (mapcar (lambda (token) (if (listp token) (apply #'string token) token))
            (nreverse tokens))))

(defun kconfig-eval-condition-tokens (tokens)
  "Evaluate tokenized condition expression using recursive descent parser."
  (let ((kconfig-token-pos 0)
        (kconfig-token-stream tokens))
    (kconfig-eval-or-expr)))

(defun kconfig-current-token ()
  "Get current token without advancing position."
  (when (< kconfig-token-pos (length kconfig-token-stream))
    (nth kconfig-token-pos kconfig-token-stream)))

(defun kconfig-advance-token ()
  "Advance to next token and return current one."
  (let ((token (kconfig-current-token)))
    (when token (setq kconfig-token-pos (1+ kconfig-token-pos)))
    token))

(defun kconfig-eval-or-expr ()
  "Evaluate OR expression (lowest precedence)."
  (let ((result (kconfig-eval-and-expr)))
    (while (equal (kconfig-current-token) "||")
      (kconfig-advance-token)
      (let ((right (kconfig-eval-and-expr)))
        (setq result (kconfig-logical-or result right))))
    result))

(defun kconfig-eval-and-expr ()
  "Evaluate AND expression (higher precedence than OR)."
  (let ((result (kconfig-eval-not-expr)))
    (while (equal (kconfig-current-token) "&&")
      (kconfig-advance-token)
      (let ((right (kconfig-eval-not-expr)))
        (setq result (kconfig-logical-and result right))))
    result))

(defun kconfig-eval-not-expr ()
  "Evaluate NOT expression and primary expressions."
  (cond
   ((equal (kconfig-current-token) "!")
    (kconfig-advance-token)
    (kconfig-logical-not (kconfig-eval-primary-expr)))
   (t (kconfig-eval-primary-expr))))

(defun kconfig-eval-primary-expr ()
  "Evaluate primary expression (config names, parentheses)."
  (let ((token (kconfig-current-token)))
    (cond
     ((equal token "(")
      (kconfig-advance-token)
      (let ((result (kconfig-eval-or-expr)))
        (unless (equal (kconfig-advance-token) ")")
          (error "Missing closing parenthesis in condition"))
        result))
     ((and token (string-match "^[A-Z0-9_]+$" token))
      (kconfig-advance-token)
      (let ((value (gethash token kconfig-config-values 'unknown)))
        (cond
         ((eq value 'unknown) 'unknown)
         ((or (eq value t) (eq value 'y) (eq value 'm)) t)
         (t nil))))
     (t (error "Invalid token in condition: %s" token)))))

(defun kconfig-logical-or (left right)
  "Logical OR with three-valued logic (t, nil, unknown)."
  (cond
   ((eq left t) t)
   ((eq right t) t)
   ((and (eq left nil) (eq right nil)) nil)
   (t 'unknown)))

(defun kconfig-logical-and (left right)
  "Logical AND with three-valued logic (t, nil, unknown)."
  (cond
   ((eq left nil) nil)
   ((eq right nil) nil)
   ((and (eq left t) (eq right t)) t)
   (t 'unknown)))

(defun kconfig-logical-not (value)
  "Logical NOT with three-valued logic (t, nil, unknown)."
  (cond
   ((eq value t) nil)
   ((eq value nil) t)
   (t 'unknown)))

(defun kconfig-validate-dependencies (option)
  "Validate that all dependencies for OPTION are satisfied.
Returns (valid . error-message) where valid is t/nil."
  (let* ((kconfig-info (gethash option kconfig-options))
         (depends (when kconfig-info (plist-get kconfig-info :depends))))
    (if (not depends)
        (cons t nil)
      (let ((result (kconfig-evaluate-condition depends)))
        (cond
         ((eq result t) (cons t nil))
         ((eq result nil) (cons nil (format "Dependencies not satisfied: %s" depends)))
         (t (cons t (format "Warning: Cannot verify dependencies: %s" depends))))))))

(defun kconfig-get-unsatisfied-dependencies (option)
  "Get list of unsatisfied dependencies for OPTION.
Returns list of dependency expressions that evaluate to false."
  (let* ((kconfig-info (gethash option kconfig-options))
         (depends (when kconfig-info (plist-get kconfig-info :depends))))
    (when depends
      (let ((result (kconfig-evaluate-condition depends)))
        (when (eq result nil)
          (list depends))))))

(defun kconfig-get-dependency-chain (option &optional visited)
  "Get full dependency chain for OPTION, detecting cycles.
Returns list of options in dependency order, or nil if circular dependency found."
  (let ((visited (or visited '())))
    (if (member option visited)
        nil
      (let* ((kconfig-info (gethash option kconfig-options))
             (depends (when kconfig-info (plist-get kconfig-info :depends)))
             (new-visited (cons option visited))
             (chain (list option)))
        (when depends
          (let ((dep-options (kconfig-extract-options-from-condition depends)))
            (dolist (dep-option dep-options)
              (let ((sub-chain (kconfig-get-dependency-chain dep-option new-visited)))
                (if sub-chain
                    (setq chain (append sub-chain chain))
                  (setq chain nil)
                  (return))))))
        chain))))

(defun kconfig-extract-options-from-condition (condition)
  "Extract all config option names from CONDITION expression."
  (let ((options '()))
    (when (stringp condition)
      (let ((tokens (kconfig-tokenize-condition condition)))
        (dolist (token tokens)
          (when (string-match "^[A-Z0-9_]+$" token)
            (push token options)))))
    (delete-dups options)))

;;
;; Architecture Detection and Configuration
;;

(defun kconfig-ensure-kconfig-loaded ()
  "Ensure Kconfig data is loaded if kernel source path is available.
Returns t if data is available, nil if no kernel source path is configured."
  (when (and kconfig-kernel-source-path
             (file-directory-p kconfig-kernel-source-path)
             (= (hash-table-count kconfig-options) 0))
    (message "Loading Kconfig data automatically...")
    (kconfig-load-kconfig-data kconfig-detected-architecture))
  (and kconfig-kernel-source-path
       (> (hash-table-count kconfig-options) 0)))

(defun kconfig-detect-architecture (kernel-root)
  "Detect the target architecture from kernel source tree."
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
           (t "x86")))
      "x86")))

(defun kconfig-detect-architecture-from-config (config-file)
  "Detect architecture from config file first line comment.
Returns the architecture string if detected, nil otherwise.
Supports patterns like '# x86_64', '# arm64', '# riscv', etc."
  (when (file-readable-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file nil 0 100)
      (goto-char (point-min))
      (when (looking-at "^#[ \t]*\\([a-zA-Z0-9_]+\\)")
        (let ((arch-comment (match-string 1)))
          (cond
           ((string-match "x86_64\\|x86" arch-comment) "x86")
           ((string-match "arm64\\|aarch64" arch-comment) "arm64")
           ((string-match "arm" arch-comment) "arm")
           ((string-match "riscv" arch-comment) "riscv")
           ((string-match "s390" arch-comment) "s390")
           ((string-match "powerpc\\|ppc" arch-comment) "powerpc")
           ((string-match "mips" arch-comment) "mips")
           (t arch-comment)))))))

(defun kconfig-set-architecture-variables (arch)
  "Set architecture-specific config variables based on ARCH.
These variables are commonly used in Kconfig dependency expressions."
  (when arch
    (pcase arch
      ("x86" (kconfig-set-config-value "X86" t)
             (kconfig-set-config-value "X86_64" t)
             (kconfig-set-config-value "64BIT" t))
      ("arm64" (kconfig-set-config-value "ARM64" t)
               (kconfig-set-config-value "64BIT" t))
      ("arm" (kconfig-set-config-value "ARM" t))
      ("riscv" (kconfig-set-config-value "RISCV" t)
               (kconfig-set-config-value "64BIT" t))
      ("s390" (kconfig-set-config-value "S390" t)
              (kconfig-set-config-value "64BIT" t))
      ("powerpc" (kconfig-set-config-value "PPC" t)
                 (kconfig-set-config-value "PPC64" t)
                 (kconfig-set-config-value "64BIT" t))
      ("mips" (kconfig-set-config-value "MIPS" t))
      (_ nil))))

(defun kconfig-mode-line-architecture ()
  "Return architecture string for mode line display.
Returns formatted string like '[x86_64]' or empty string if no arch detected."
  (if kconfig-detected-architecture
      (format " [%s]" kconfig-detected-architecture)
    ""))

(defun kconfig-get-kernel-build-vars (kernel-root)
  "Get kernel build variables from the source tree context."
  (let ((arch (kconfig-detect-architecture kernel-root)))
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

(defun kconfig-expand-kconfig-variables (path kernel-root)
  "Expand Kconfig variables in PATH using KERNEL-ROOT context.
Supports $(VAR), ${VAR}, and $VAR patterns."
  (let ((expanded-path path)
        (build-vars (kconfig-get-kernel-build-vars kernel-root)))
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
    (setq expanded-path (replace-regexp-in-string "\\$(srctree)/" "" expanded-path))
    (setq expanded-path (replace-regexp-in-string "\\${srctree}/" "" expanded-path))
    expanded-path))

(defun kconfig-expand-glob-pattern (pattern kernel-root)
  "Expand glob PATTERN relative to KERNEL-ROOT, returning list of matching files.
Supports *, ?, [abc], and ** patterns."
  (let ((expanded-pattern (kconfig-expand-kconfig-variables pattern kernel-root)))
    (if (string-match-p "[*?\\[]" expanded-pattern)
        (let ((full-pattern (expand-file-name expanded-pattern kernel-root)))
          (if (string-match-p "\\*\\*" full-pattern)
              (let* ((parts (split-string full-pattern "/\\*\\*/"))
                     (base-dir (car parts))
                     (file-pattern (if (> (length parts) 1) (cadr parts) "Kconfig")))
                (when (and (file-directory-p base-dir) (> (length file-pattern) 0))
                  (let ((find-cmd (format "find '%s' -name '%s' -type f 2>/dev/null"
                                          base-dir file-pattern)))
                    (let ((result (shell-command-to-string find-cmd)))
                      (when (> (length (string-trim result)) 0)
                        (split-string (string-trim result) "\n"))))))
            (file-expand-wildcards full-pattern)))
      (let ((full-path (expand-file-name expanded-pattern kernel-root)))
        (when (file-readable-p full-path)
          (list full-path))))))

;;
;; Kconfig Parsing Functions
;;

(defun kconfig-parse-source-directives (file kernel-root)
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
           ((looking-at "^\\s-*if\\s-+\\(.+\\)")
            (setq in-if-block t
                  if-condition (match-string 1))
            (forward-line 1))
           ((looking-at "^\\s-*endif")
            (setq in-if-block nil
                  if-condition nil)
            (forward-line 1))
           ((looking-at "^\\s-*source\\s-+\"\\([^\"]+\\)\"\\(\\s-+if\\s-+\\(.+\\)\\)?")
            (let* ((source-path (match-string 1))
                   (condition (match-string 3))
                   (should-include (if condition
                                     (let ((result (kconfig-evaluate-condition condition)))
                                       (if (eq result 'unknown) t result))
                                   t)))
              (when should-include
                (let ((expanded-files
                       (cond
                        ((file-name-absolute-p source-path)
                         (if (string-match-p "[*?]" source-path)
                             (file-expand-wildcards source-path)
                           (when (file-readable-p source-path)
                             (list source-path))))
                        (t
                         (kconfig-expand-glob-pattern source-path kernel-root)))))
                  (dolist (expanded-file expanded-files)
                    (when (and expanded-file (file-readable-p expanded-file))
                      (push expanded-file sources)))))
              (forward-line 1)))
           (t
            (forward-line 1))))
        (nreverse sources)))))

(defun kconfig-collect-kconfig-files (kernel-root)
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
            (let ((sources (kconfig-parse-source-directives current-file kernel-root)))
              (setq queue (append queue sources)))))))
    (nreverse files)))

(defun kconfig-parse-kconfig-option (lines &optional entry-type source-file)
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
       ((string-match "^comment\\s-+\"\\([^\"]+\\)\"" line)
        (setq name (format "COMMENT_%s" (replace-regexp-in-string "[^A-Z0-9_]" "_" (upcase (match-string 1 line)))))
        (setq comment-text (match-string 1 line))
        (setq type 'comment))
       ((string-match "^[ \t]*\\(menu\\)?config[ \t]+\\([A-Z0-9_]+\\)" line)
        (setq name (match-string 2 line)))
       ((and (not in-help) (string-match "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\b\\(?: \"\\([^\"]*\\)\"\\)?\\(?: if .*\\)?" line))
        (setq type (intern (match-string 1 line))))
       ((string-match "^[ \t]+def_\\(bool\\|tristate\\)\\s-+\\(.+\\)" line)
        (setq type (intern (match-string 1 line)))
        (setq default (match-string 2 line)))
       ((string-match "^[ \t]+depends on \\(.+\\)" line)
        (setq depends (match-string 1 line)))
       ((string-match "^[ \t]+select \\([A-Z0-9_]+\\)\\(?: if \\(.+\\)\\)?" line)
        (let ((selected-option (match-string 1 line))
              (select-condition (match-string 2 line)))
          (push (if select-condition
                    (cons selected-option select-condition)
                  selected-option)
                select)))
       ((string-match "^[ \t]+default \\(.+\\)" line)
        (setq default (match-string 1 line)))
       ((string-match "^[ \t]+range \\([0-9]+\\) \\([0-9]+\\)" line)
        (setq range (cons (string-to-number (match-string 1 line))
                          (string-to-number (match-string 2 line)))))
       ((string-match "^[ \t]+help" line)
        (setq in-help t))
       ((and in-help (string-match "^[ \t]+\\(.+\\)" line))
        (setq help (if help
                       (concat help "\\n" (match-string 1 line))
                     (match-string 1 line))))
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

(defun kconfig-preprocess-continuations (content)
  "Preprocess CONTENT to handle line continuations (backslash at end of line)."
  (let ((lines (split-string content "\n"))
        (processed-lines '())
        (current-line ""))
    (dolist (line lines)
      (if (string-match "\\\\\\s-*$" line)
          (setq current-line (concat current-line (replace-regexp-in-string "\\\\\\s-*$" " " line)))
        (setq current-line (concat current-line line))
        (push current-line processed-lines)
        (setq current-line "")))
    (when (> (length current-line) 0)
      (push current-line processed-lines))
    (nreverse processed-lines)))

(defun kconfig-parse-kconfig-file (file)
  "Parse a single Kconfig file and return list of (name . plist) pairs.
Handles config, menuconfig, choice/endchoice, and menu/endmenu blocks."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (kconfig-preprocess-continuations (buffer-string)))
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
             ((string-match "^mainmenu\\s-+\"\\([^\"]+\\)\"" line)
              (setq kconfig-main-menu-title
                    (kconfig-expand-kconfig-variables (match-string 1 line)
                                                     (file-name-directory file))))
             ((string-match "^menu\\s-+\"\\([^\"]+\\)\"" line)
              (push (match-string 1 line) menu-stack)
              (setq in-menu t))
             ((string-match "^endmenu" line)
              (when menu-stack
                (pop menu-stack))
              (setq in-menu (> (length menu-stack) 0)))
             ((string-match "^if\\s-+\\(.+\\)" line)
              (let ((condition (match-string 1 line)))
                (push condition if-conditions)
                (setq in-if-block t)))
             ((string-match "^endif" line)
              (when if-conditions
                (pop if-conditions))
              (setq in-if-block (> (length if-conditions) 0)))
             ((string-match "^choice" line)
              (when current-config
                (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
                  (when option (push option options))))
              (setq current-choice (list line)
                    choice-options '()
                    in-choice t
                    current-config nil
                    in-config nil))
             ((string-match "^endchoice" line)
              (when current-choice
                (when current-config
                  (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
                    (when option (push option options))))
                (dolist (choice-opt choice-options)
                  (let ((phantom-option (cons choice-opt
                                             (list :type 'bool
                                                   :help (format "Choice option in group")
                                                   :phantom t))))
                    (push phantom-option options)))
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
             ((string-match "^comment\\s-+\"\\([^\"]+\\)\"" line)
              (when current-config
                (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
                  (when option (push option options))))
              (setq current-config (list line)
                    in-config t
                    config-type 'comment))
             ((string-match "^[ \t]*config[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((config-name (match-string 1 line)))
                (when current-config
                  (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
                    (when option (push option options))))
                (setq current-config (list line)
                      in-config t
                      config-type 'config)
                (when in-choice
                  (push config-name choice-options))))
             ((string-match "^[ \t]*menuconfig[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((menuconfig-name (match-string 1 line)))
                (when current-config
                  (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
                    (when option (push option options))))
                (setq current-config (list line)
                      in-config t
                      config-type 'menuconfig)))
             ((and (or in-config in-choice) (string-match "^[ \t]+" line))
              (when current-config
                (push line current-config)))
             ((and in-config (not (string-match "^[ \t]*$" line))
                   (not (string-match "^\\(config\\|menuconfig\\|choice\\|endchoice\\|menu\\|endmenu\\|source\\)" line)))
              (when current-config
                (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
                  (when option (push option options))))
              (setq current-config nil
                    in-config nil)))))

        (when current-config
          (let ((option (kconfig-parse-kconfig-option (nreverse current-config) config-type file)))
            (when option (push option options))))

        (when current-choice
          (let* ((choice-name (format "CHOICE_%d" (random 10000)))
                 (choice-option (cons choice-name
                                     (list :type 'choice
                                           :choices choice-options
                                           :help "Choice group"))))
            (push choice-option options)))

        (nreverse options)))))

(defun kconfig-build-selection-chains ()
  "Build forward (:selects) and reverse (:selected-by) selection chains.
Also extract :depends-on lists from :depends expressions.
This consolidates selection chain data directly into kconfig-options,
eliminating the need for a separate kconfig-select-chains hash table."
  (let ((all-options '()))
    (maphash (lambda (name _plist) (push name all-options))
             kconfig-options)
    (dolist (option-name all-options)
      (let* ((option-plist (gethash option-name kconfig-options))
             (select-list (plist-get option-plist :select))
             (depends-expr (plist-get option-plist :depends))
             (selects-chain '())
             (depends-on-list '()))
        (when select-list
          (dolist (select-stmt select-list)
            (if (consp select-stmt)
                (push select-stmt selects-chain)
              (push (cons select-stmt nil) selects-chain))))
        (when depends-expr
          (setq depends-on-list (kconfig-extract-options-from-condition depends-expr)))
        (puthash option-name
                 (plist-put (plist-put option-plist
                                      :selects (nreverse selects-chain))
                           :depends-on depends-on-list)
                 kconfig-options)))
    (dolist (selector-name all-options)
      (let* ((selector-plist (gethash selector-name kconfig-options))
             (selects-chain (plist-get selector-plist :selects)))
        (dolist (select-pair selects-chain)
          (let* ((selected-name (car select-pair))
                 (condition (cdr select-pair))
                 (selected-plist (gethash selected-name kconfig-options)))
            (when selected-plist
              (let ((current-selected-by (plist-get selected-plist :selected-by)))
                (puthash selected-name
                        (plist-put selected-plist
                                  :selected-by
                                  (cons (cons selector-name condition)
                                        current-selected-by))
                        kconfig-options)))))))))

(defun kconfig-apply-vendor-type-corrections ()
  "Apply vendor-specific type corrections for known kernel patches.
This handles cases where distributions (like RHEL) patch the mainline kernel
to change option types (e.g., bool to tristate)."
  (let ((corrections '(("TEST_MISC_MINOR" tristate "RHEL patches TEST_MISC_MINOR to tristate")
                       ("ACPI_CUSTOM_METHOD" tristate "RedHat/Fedora debug kernels patch ACPI_CUSTOM_METHOD to tristate")
                       ("EFI_VARS" tristate "RedHat automotive kernels patch EFI_VARS to tristate")))
        (applied-count 0))
    (dolist (correction corrections)
      (let* ((option (nth 0 correction))
             (new-type (nth 1 correction))
             (reason (nth 2 correction))
             (option-plist (gethash option kconfig-options)))
        (when option-plist
          (let ((old-type (plist-get option-plist :type)))
            (when (and old-type (not (eq old-type new-type)))
              (puthash option
                      (plist-put option-plist :type new-type)
                      kconfig-options)
              (setq applied-count (1+ applied-count))
              (message "Applied type correction: %s %s -> %s (%s)"
                      option old-type new-type reason))))))
    (when (> applied-count 0)
      (message "Applied %d vendor-specific type corrections" applied-count))))

(defun kconfig-load-kconfig-data (&optional target-arch)
  "Load and parse all Kconfig files from kernel source tree.
TARGET-ARCH can be specified to load architecture-specific Kconfig files.
If nil, detects architecture from kernel source tree."
  (interactive)
  (when kconfig-kernel-source-path
    (unless (file-directory-p kconfig-kernel-source-path)
      (error "Kernel source path does not exist: %s" kconfig-kernel-source-path))
    (message "Loading Kconfig data from %s..." kconfig-kernel-source-path)
    (clrhash kconfig-options)
    (clrhash kconfig-config-values)
    (let ((arch (or target-arch
                    kconfig-detected-architecture
                    (kconfig-detect-architecture kconfig-kernel-source-path))))
      (pcase arch
        ("x86" (kconfig-set-config-value "X86" t)
               (kconfig-set-config-value "X86_64" t)
               (kconfig-set-config-value "64BIT" t))
        ("arm64" (kconfig-set-config-value "ARM64" t)
                 (kconfig-set-config-value "64BIT" t))
        ("arm" (kconfig-set-config-value "ARM" t))
        ("riscv" (kconfig-set-config-value "RISCV" t)
                 (kconfig-set-config-value "64BIT" t))
        ("s390" (kconfig-set-config-value "S390" t)
                (kconfig-set-config-value "64BIT" t))
        ("powerpc" (kconfig-set-config-value "PPC" t)
                   (kconfig-set-config-value "PPC64" t)
                   (kconfig-set-config-value "64BIT" t))
        (_ nil))
      (kconfig-set-config-value "EXPERT" nil)
      (kconfig-set-config-value "COMPILE_TEST" nil)
      (let ((kconfig-files (kconfig-collect-kconfig-files kconfig-kernel-source-path))
            (total-options 0))
        (when arch
          (let ((arch-kconfig (expand-file-name (format "arch/%s/Kconfig" arch)
                                               kconfig-kernel-source-path)))
            (when (file-readable-p arch-kconfig)
              (push arch-kconfig kconfig-files)
              (message "Including architecture-specific Kconfig: arch/%s/Kconfig" arch))))
        (dolist (file kconfig-files)
        (let ((options (kconfig-parse-kconfig-file file)))
          (when (> (length options) 0)
            (message "Found %d options in %s" (length options) file))
          (dolist (option options)
            (let* ((option-name (car option))
                   (new-plist (cdr option))
                   (new-type (plist-get new-plist :type))
                   (existing-plist (gethash option-name kconfig-options))
                   (existing-type (when existing-plist (plist-get existing-plist :type))))
              (when (or (not existing-plist)
                        (and new-type (not (eq new-type 'unknown)))
                        (and existing-type (eq existing-type 'unknown)))
                (puthash option-name new-plist kconfig-options)))
            (setq total-options (1+ total-options)))))
      (message "Building selection and dependency chains...")
      (kconfig-build-selection-chains)
      (kconfig-apply-vendor-type-corrections)
      (setq kconfig-loaded t)
        (message "Loaded %d options from %d Kconfig files"
                 total-options (length kconfig-files))))))

;;
;; Validation Functions
;;

(defun kconfig-validate-option-value (option value)
  "Validate VALUE for configuration OPTION based on its Kconfig type.
Returns (valid . error-message) where valid is t/nil and error-message explains validation failure."
  (kconfig-ensure-kconfig-loaded)
  (let* ((kconfig-info (gethash option kconfig-options))
         (option-type (when kconfig-info (plist-get kconfig-info :type)))
         (option-range (when kconfig-info (plist-get kconfig-info :range))))
    (if (not kconfig-info)
        (let ((vendor-info (kconfig-is-vendor-specific-option option)))
          (if vendor-info
              (progn
                (kconfig-create-phantom-vendor-option option vendor-info)
                (kconfig-validate-option-value option value))
            (cons t (if kconfig-kernel-source-path
                        (format "Warning: No Kconfig definition found for %s" option)
                      (format "Warning: %s not validated (no kernel source path set)" option)))))
      (cond
       ((eq option-type 'bool)
        (kconfig-validate-bool-value value))
       ((eq option-type 'tristate)
        (kconfig-validate-tristate-value value))
       ((eq option-type 'string)
        (kconfig-validate-string-value value))
       ((eq option-type 'int)
        (kconfig-validate-int-value value option-range))
       ((eq option-type 'hex)
        (kconfig-validate-hex-value value option-range))
       (t
        (cons t nil))))))

(defun kconfig-validate-bool-value (value)
  "Validate VALUE for bool option. Returns (valid . error-message)."
  (cond
   ((member value '("y" "n" nil)) (cons t nil))
   (t (cons nil "Boolean options only accept 'y' or 'n' (or unset)"))))

(defun kconfig-validate-tristate-value (value)
  "Validate VALUE for tristate option. Returns (valid . error-message)."
  (cond
   ((member value '("y" "m" "n" nil)) (cons t nil))
   (t (cons nil "Tristate options only accept 'y', 'm', 'n' (or unset)"))))

(defun kconfig-validate-string-value (value)
  "Validate VALUE for string option. Returns (valid . error-message)."
  (cond
   ((null value) (cons t nil))
   ((and (stringp value)
         (or (not (string-match "\"" value))
             (and (string-prefix-p "\"" value)
                  (string-suffix-p "\"" value)
                  (= (length (split-string value "\"")) 3))))
    (cons t nil))
   (t (cons nil "String values should not contain unescaped quotes"))))

(defun kconfig-validate-int-value (value range)
  "Validate VALUE for int option with optional RANGE. Returns (valid . error-message)."
  (cond
   ((null value) (cons t nil))
   ((not (string-match "^-?[0-9]+$" value))
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

(defun kconfig-validate-hex-value (value range)
  "Validate VALUE for hex option with optional RANGE. Returns (valid . error-message)."
  (cond
   ((null value) (cons t nil))
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

;;
;; Vendor-specific Option Support Functions
;;

(defun kconfig-is-vendor-specific-option (option)
  "Check if OPTION is a known vendor-specific option.
Returns option info if found, nil otherwise."
  (assoc option kconfig-vendor-specific-options))

(defun kconfig-create-phantom-vendor-option (option option-info)
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
             kconfig-options)))

(defun kconfig-detect-config-version (config-file)
  "Detect kernel version from config file header.
Returns version string or nil if not found."
  (when (file-readable-p config-file)
    (with-temp-buffer
      (insert-file-contents config-file nil 0 1000)
      (goto-char (point-min))
      (cond
       ((re-search-forward "Linux/[a-z0-9_]+ \\([0-9]+\\.[0-9]+\\.[0-9]+[^[:space:]]*\\) Kernel Configuration" nil t)
        (match-string 1))
       ((re-search-forward "^# [0-9]+\\.[0-9]+\\.[0-9]+[^[:space:]]*$" nil t)
        (substring (match-string 0) 2))
       (t nil)))))

;;
;; Vendor-specific Options Database (643 options)
;;

(defvar kconfig-vendor-specific-options
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
    ("EFI_VARS" :type tristate :deprecated "5.5" :description "EFI variable support via sysfs (RedHat automotive: tristate, deprecated)")
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
    ("PSTORE_ZSTD_COMPRESS" :type tristate :description "PSTORE zstd compression")
    ("PSTORE_LZO_COMPRESS" :type tristate :description "PSTORE LZO compression")
    ("PSTORE_LZ4HC_COMPRESS" :type tristate :description "PSTORE LZ4HC compression")
    ("PSTORE_LZ4_COMPRESS" :type tristate :description "PSTORE LZ4 compression")
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
    ("ACPI_CUSTOM_METHOD" :type tristate :description "ACPI custom method support (RedHat/Fedora: tristate)")
    ("TPM_KEY_PARSER" :type tristate :deprecated "6.10" :description "TPM key parser (removed)")
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
    ("Z3FOLD_DEPRECATED" :type bool :deprecated "6.7" :description "Z3fold deprecated notice")

    ;; ARM64-specific platform and architecture options
    ("ARM64_CRYPTO" :type bool :arch "arm64" :description "ARM64 cryptographic extensions")
    ("ARM64_ERRATUM_3312417" :type bool :arch "arm64" :version "6.17" :description "ARM64 erratum 3312417 workaround")
    ("ARCH_NR_GPIO" :type int :description "Maximum number of GPIOs in the system")
    ("PCI_MSI_IRQ_DOMAIN" :type bool :description "PCI MSI IRQ domain support")
    ("VDSO" :type bool :description "Virtual Dynamic Shared Object support")
    ("BIG_LITTLE" :type bool :arch "arm64" :description "ARM big.LITTLE support")
    ("SCHED_THERMAL_PRESSURE" :type bool :description "Scheduler thermal pressure tracking")

    ;; ARM legacy/ARM32 options (should not appear in ARM64 but included for compatibility)
    ("ARCH_MULTIPLATFORM" :type bool :arch "arm" :deprecated "6.0" :description "ARM multiplatform support")
    ("ARM_CRYPTO" :type bool :arch "arm" :description "ARM cryptographic extensions")
    ("ARM_PSCI" :type bool :arch "arm" :description "ARM Power State Coordination Interface")
    ("ARM_PTDUMP_DEBUGFS" :type bool :arch "arm" :description "ARM page table dumping via debugfs")
    ("ARM_DMA_USE_IOMMU" :type bool :arch "arm" :description "ARM DMA API uses IOMMU")
    ("ARM_DMA_IOMMU_ALIGNMENT" :type int :arch "arm" :description "ARM DMA-IOMMU alignment")
    ("UNWINDER_ARM" :type bool :arch "arm" :description "ARM stack unwinder")
    ("IWMMXT" :type bool :arch "arm" :description "Intel Wireless MMX technology support")
    ("CACHE_FEROCEON_L2" :type bool :arch "arm" :description "Feroceon L2 cache support")

    ;; Broadcom platform options
    ("ARCH_BCM2835" :type bool :arch "arm64" :description "Broadcom BCM2835 family")
    ("ARCH_BRCMSTB" :type bool :arch "arm64" :description "Broadcom Set-Top Box SoCs")
    ("ARCH_BCM_IPROC" :type bool :arch "arm64" :description "Broadcom iProc family")
    ("ARCH_BCM_21664" :type bool :arch "arm" :description "Broadcom BCM21664")
    ("ARCH_BCM_23550" :type bool :arch "arm" :description "Broadcom BCM23550")
    ("ARCH_BCM_281XX" :type bool :arch "arm" :description "Broadcom BCM281XX")
    ("ARCH_BCM_5301X" :type bool :arch "arm" :description "Broadcom BCM5301X")
    ("ARCH_BCM_53573" :type bool :arch "arm" :description "Broadcom BCM53573")
    ("ARCH_BCM_63XX" :type bool :arch "arm" :description "Broadcom BCM63XX")
    ("ARCH_BCM_CYGNUS" :type bool :arch "arm64" :description "Broadcom Cygnus")
    ("ARCH_BCM_HR2" :type bool :arch "arm" :description "Broadcom Hurricane 2")
    ("ARCH_BCM_NSP" :type bool :arch "arm" :description "Broadcom Northstar Plus")
    ("ARCH_BCMBCA" :type bool :arch "arm64" :description "Broadcom Broadband SoC")

    ;; Other ARM platforms
    ("ARCH_ASPEED" :type bool :arch "arm64" :description "Aspeed BMC SoC family")
    ("ARCH_DOVE" :type bool :arch "arm" :deprecated "6.2" :description "Marvell Dove (removed)")
    ("ARCH_EP93XX" :type bool :arch "arm" :description "Cirrus Logic EP93xx")
    ("ARCH_FOOTBRIDGE" :type bool :arch "arm" :deprecated "6.2" :description "FootBridge (removed)")
    ("ARCH_IOP32X" :type bool :arch "arm" :deprecated "6.2" :description "Intel IOP32x (removed)")
    ("ARCH_IXP4XX" :type bool :arch "arm" :description "Intel IXP4xx")
    ("ARCH_MSM8916" :type bool :arch "arm64" :description "Qualcomm MSM8916")
    ("ARCH_MSTARV7" :type bool :arch "arm" :description "MStar/SigmaStar ARMv7")
    ("ARCH_OMAP1" :type bool :arch "arm" :deprecated "6.2" :description "TI OMAP1 (removed)")
    ("ARCH_PXA" :type bool :arch "arm" :description "Intel/Marvell PXA2xx/PXA3xx")
    ("ARCH_RDA" :type bool :arch "arm" :description "RDA Micro SoC")
    ("ARCH_SA1100" :type bool :arch "arm" :description "Intel StrongARM SA-1100")

    ;; ARM/ARM64 Crypto acceleration
    ("CRYPTO_SHA1_ARM" :type tristate :arch "arm" :description "SHA1 digest algorithm (ARM)")
    ("CRYPTO_SHA1_ARM_NEON" :type tristate :arch "arm" :description "SHA1 using ARM NEON")
    ("CRYPTO_SHA1_ARM64_CE" :type tristate :arch "arm64" :description "SHA-1 using ARMv8 Crypto Extensions")
    ("CRYPTO_SHA256_ARM" :type tristate :arch "arm" :description "SHA-224/256 using ARM asm")
    ("CRYPTO_SHA256_ARM64" :type tristate :arch "arm64" :description "SHA-224/256 using ARMv8")
    ("CRYPTO_SHA2_ARM64_CE" :type tristate :arch "arm64" :description "SHA-224/256 using ARMv8 Crypto Extensions")
    ("CRYPTO_SHA512_ARM" :type tristate :arch "arm" :description "SHA-384/512 using ARM asm")
    ("CRYPTO_SHA512_ARM64" :type tristate :arch "arm64" :description "SHA-384/512 using ARMv8")
    ("CRYPTO_SHA512_ARM64_CE" :type tristate :arch "arm64" :description "SHA-384/512 using ARMv8 Crypto Extensions")
    ("CRYPTO_BLAKE2S_ARM" :type tristate :arch "arm" :description "BLAKE2s using ARM asm")
    ("CRYPTO_CHACHA20_NEON" :type tristate :arch "arm" :description "ChaCha20 using ARM NEON")
    ("CRYPTO_POLY1305_NEON" :type tristate :arch "arm" :description "Poly1305 using ARM NEON")
    ("CRYPTO_CRC32_ARM_CE" :type tristate :arch "arm" :description "CRC32 using ARM Crypto Extensions")
    ("CRYPTO_CRCT10DIF_ARM_CE" :type tristate :arch "arm" :description "CRCT10DIF using ARM Crypto Extensions")
    ("CRYPTO_CRCT10DIF_ARM64_CE" :type tristate :arch "arm64" :description "CRCT10DIF using ARMv8 Crypto Extensions")
    ("CRYPTO_DEV_CAVIUM_ZIP" :type tristate :description "Cavium ZIP compression/decompression")

    ;; Bcachefs filesystem options (kernel 6.7+)
    ("BCACHEFS_POSIX_ACL" :type bool :version "6.7" :description "Bcachefs POSIX ACL support")
    ("BCACHEFS_QUOTA" :type bool :version "6.7" :description "Bcachefs quota support")
    ("BCACHEFS_ERASURE_CODING" :type bool :version "6.7" :description "Bcachefs erasure coding")
    ("BCACHEFS_DEBUG" :type bool :version "6.7" :description "Bcachefs debugging")
    ("BCACHEFS_DEBUG_TRANSACTIONS" :type bool :version "6.7" :description "Bcachefs transaction debugging")
    ("BCACHEFS_LOCK_TIME_STATS" :type bool :version "6.7" :description "Bcachefs lock timing statistics")
    ("BCACHEFS_NO_LATENCY_ACCT" :type bool :version "6.7" :description "Disable Bcachefs latency accounting")
    ("BCACHEFS_PATH_TRACEPOINTS" :type bool :version "6.7" :description "Bcachefs path tracepoints")
    ("BCACHEFS_SIX_OPTIMISTIC_SPIN" :type bool :version "6.7" :description "Bcachefs six-lock optimistic spinning")
    ("BCACHEFS_TESTS" :type tristate :version "6.7" :description "Bcachefs self-tests")
    ("BCACHEFS_ASYNC_OBJECT_LISTS" :type bool :version "6.7" :description "Bcachefs async object lists")
    ("BCACHEFS_TRANS_KMALLOC_TRACE" :type bool :version "6.7" :description "Bcachefs transaction kmalloc tracing")
    ("BCACHEFS_INJECT_TRANSACTION_RESTARTS" :type bool :version "6.7" :description "Inject transaction restarts for testing")
    ("BCACHE_CLOSURES_DEBUG" :type bool :description "Bcache closures debugging")

    ;; PSTORE compression defaults
    ("PSTORE_842_COMPRESS_DEFAULT" :type bool :description "Use 842 as default PSTORE compression")
    ("PSTORE_LZ4_COMPRESS_DEFAULT" :type bool :description "Use LZ4 as default PSTORE compression")
    ("PSTORE_LZ4HC_COMPRESS_DEFAULT" :type bool :description "Use LZ4HC as default PSTORE compression")
    ("PSTORE_LZO_COMPRESS_DEFAULT" :type bool :description "Use LZO as default PSTORE compression")

    ;; SOC/Platform-specific options
    ("MESON_EFUSE" :type tristate :description "Amlogic Meson eFuse driver")
    ("MESON_MX_EFUSE" :type tristate :description "Amlogic Meson MX eFuse driver")
    ("MESON_GX_PM_DOMAINS" :type bool :description "Amlogic Meson GX power domains")
    ("ROCKCHIP_EFUSE" :type tristate :description "Rockchip eFuse driver")
    ("ROCKCHIP_OTP" :type tristate :description "Rockchip OTP controller")
    ("TEGRA_VDE" :type tristate :deprecated "6.1" :description "NVIDIA Tegra VDE driver (removed)")
    ("SOC_EXYNOS4212" :type bool :deprecated "5.18" :description "Samsung Exynos4212 (removed)")
    ("QCOM_SCM_DOWNLOAD_MODE_DEFAULT" :type bool :description "Qualcomm SCM download mode by default")
    ("SM_DISPCC_8650" :type tristate :version "6.9" :description "SM8650 Display Clock Controller")
    ("SC_MSS_7180" :type tristate :description "SC7180 Modem SubSystem")
    ("SCR24X" :type tristate :description "Spreadtrum SC27xx series")
    ("MDM_LCC_9615" :type tristate :deprecated "5.16" :description "MDM9615 LCC (removed)")
    ("OPTEE_SHM_NUM_PRIV_PAGES" :type int :description "OP-TEE shared memory private pages")
    ("OMAP_MBOX_KFIFO_SIZE" :type int :arch "arm" :description "OMAP mailbox kfifo size")

    ;; Sound/Audio platform options
    ("SND_SOC_ROCKCHIP" :type tristate :description "Rockchip SoC audio support")
    ("SND_SOC_SOF_IMX8M" :type tristate :description "SOF support for i.MX8M")
    ("SND_SOC_SOF_IMX8ULP" :type tristate :version "6.5" :description "SOF support for i.MX8ULP")
    ("TI_AM65_CPSW_TAS" :type bool :description "TI AM65 CPSW EST/TAS offload")

    ;; Peripheral/Storage options
    ("SDHCI_AM654" :type tristate :description "TI AM654 Secure Digital Host Controller")
    ("SCSI_UFS_HPB" :type bool :deprecated "5.18" :description "UFS Host Performance Booster (removed)")
    ("SERIAL_EARLYCON_ARM_SEMIHOST" :type bool :arch "arm" :description "Early console using ARM semihosting")

    ;; Networking options
    ("NET_DSA_MICROCHIP_KSZ8795" :type tristate :description "Microchip KSZ8795 Ethernet switch")
    ("NET_DSA_MICROCHIP_KSZ9477" :type tristate :description "Microchip KSZ9477 Ethernet switch")
    ("NET_DSA_MICROCHIP_KSZ9477_SPI" :type tristate :description "Microchip KSZ9477 SPI support")

    ;; Deprecated/removed drivers
    ("REISERFS_CHECK" :type bool :deprecated "5.18" :description "ReiserFS consistency checking (removed)")
    ("REISERFS_FS_XATTR" :type bool :deprecated "5.18" :description "ReiserFS extended attributes (removed)")
    ("REISERFS_FS_POSIX_ACL" :type bool :deprecated "5.18" :description "ReiserFS POSIX ACLs (removed)")
    ("REISERFS_FS_SECURITY" :type bool :deprecated "5.18" :description "ReiserFS security labels (removed)")
    ("REISERFS_PROC_INFO" :type bool :deprecated "5.18" :description "ReiserFS /proc support (removed)")
    ("USB_FUSB300" :type tristate :deprecated "5.9" :description "Faraday FUSB300 USB peripheral (removed)")
    ("USB_MV_UDC" :type tristate :deprecated "6.3" :description "Marvell USB2.0 Device Controller (removed)")
    ("USB_MV_U3D" :type tristate :deprecated "6.3" :description "Marvell USB 3.0 Device Controller (removed)")
    ("USB_NET2272" :type tristate :deprecated "5.9" :description "NetChip NET2272 USB peripheral (removed)")
    ("VIDEO_CADENCE" :type tristate :deprecated "6.0" :description "Cadence video driver (removed)")
    ("VIDEO_STKWEBCAM" :type tristate :deprecated "6.7" :description "USB Syntek DC1125 webcam (removed)")
    ("VIDEO_TM6000_DVB" :type tristate :deprecated "6.0" :description "TM6000 DVB support (removed)")
    ("VXGE" :type tristate :deprecated "5.11" :description "Neterion X3100 10GbE driver (removed)")
    ("VXGE_DEBUG_TRACE_ALL" :type bool :deprecated "5.11" :description "Neterion X3100 debug tracing (removed)")
    ("W1_MASTER_DS1WM" :type tristate :deprecated "5.17" :description "Dallas 1-wire master (removed)")
    ("WILINK_PLATFORM_DATA" :type bool :deprecated "5.19" :description "TI WiLink platform data (removed)")
    ("RAPIDIO_TSI568" :type tristate :deprecated "6.0" :description "Tsi568 RapidIO switch (removed)")
    ("RAPIDIO_TSI57X" :type tristate :deprecated "6.0" :description "Tsi57x RapidIO switch (removed)")

    ;; Miscellaneous options
    ("ANDROID_BINDER_IPC_SELFTEST" :type tristate :description "Android Binder IPC self-tests")
    ("XEN_PRIVCMD_IRQFD" :type bool :version "6.1" :description "Xen privcmd irqfd support")
    ("BOOTPARAM_SOFTLOCKUP_PANIC_VALUE" :type int :description "Softlockup panic boot parameter default")
    ("BTRFS_FS_CHECK_INTEGRITY" :type bool :description "Btrfs integrity checking")
    ("BTRFS_FS_REF_VERIFY" :type bool :description "Btrfs ref verify")
    ("CIO2_BRIDGE" :type bool :description "Intel IPU3 CIO2 ACPI bridge")
    ("CROS_KUNIT" :type tristate :description "ChromeOS KUnit tests")
    ("DRM_MSM_REGISTER_LOGGING" :type bool :description "MSM DRM register logging")
    ("HZ_200" :type bool :description "200 Hz timer frequency")
    ("HZ_500" :type bool :description "500 Hz timer frequency")
    ("IEEE802154_AT86RF230_DEBUGFS" :type bool :description "AT86RF230 debugfs support")
    ("INFINIBAND_QIB_DCA" :type bool :deprecated "5.11" :description "QLogic IB DCA (removed)")
    ("INIT_MLOCKED_ON_FREE_DEFAULT_ON" :type bool :version "6.6" :description "Initialize mlocked pages on free by default")
    ("MCDI_LOGGING" :type bool :description "Solarflare MCDI logging")
    ("PWM_SYSFS" :type bool :deprecated "5.16" :description "PWM sysfs interface (removed)")
    ("REGULATOR_MAX597X" :type tristate :version "6.5" :description "Maxim MAX597x regulator")
    ("RELOCATABLE_TEST" :type tristate :description "Kernel relocation self-test")
    ("UBSAN_SANITIZE_ALL" :type bool :description "Instrument entire kernel with UBSAN")
    ("UBSAN_SIGNED_WRAP" :type bool :version "6.3" :description "Detect signed arithmetic wrap")
    ("UNICODE_NORMALIZATION_SELFTEST" :type tristate :description "Unicode normalization self-tests")
    ("UNICODE_UTF8_DATA" :type bool :description "Include UTF-8 normalization data")

    ("ALTIVEC" :type bool :arch "powerpc" :description "PowerPC AltiVec SIMD support")
    ("BOOTX_TEXT" :type bool :arch "powerpc" :description "Support for early boot text console (BootX)")
    ("CMM" :type tristate :arch "powerpc" :description "Collaborative Memory Management")
    ("CRYPTO_CURVE25519_PPC64" :type tristate :arch "powerpc" :description "Curve25519 using PowerPC64 instructions")
    ("DEBUGGER" :type bool :arch "powerpc" :description "PowerPC debugger support")
    ("DTL" :type bool :arch "powerpc" :description "Dispatch Trace Log for pSeries")
    ("HV_PERF_CTRS" :type bool :arch "powerpc" :description "Hypervisor supplied performance counters")
    ("IO_EVENT_IRQ" :type bool :arch "powerpc" :description "PowerPC IO event interrupt support")
    ("KVM_BOOK3S_64" :type tristate :arch "powerpc" :description "KVM support for PowerPC book3s_64 processors")
    ("KVM_BOOK3S_64_HV" :type tristate :arch "powerpc" :description "KVM for book3s_64 (HV mode)")
    ("KVM_BOOK3S_64_PR" :type tristate :arch "powerpc" :description "KVM for book3s_64 (PR mode)")
    ("KVM_BOOK3S_HV_NESTED_PMU_WORKAROUND" :type bool :arch "powerpc" :description "Nested PMU workaround for KVM HV")
    ("KVM_XICS" :type bool :arch "powerpc" :description "KVM in-kernel XICS interrupt controller")
    ("LPARCFG" :type bool :arch "powerpc" :description "LPAR configuration interface")
    ("MSI_BITMAP_SELFTEST" :type bool :arch "powerpc" :description "MSI bitmap self-test")
    ("OPAL_PRD" :type tristate :arch "powerpc" :description "OPAL Processor Recovery Diagnostics driver")
    ("PAPR_SCM" :type tristate :arch "powerpc" :description "Support for the PAPR Storage Class Memory")
    ("POWER8_CPU" :type bool :arch "powerpc" :description "Optimize for POWER8")
    ("PPC_BOOK3S_64" :type bool :arch "powerpc" :description "Server processors (POWER4+)")
    ("PPC_DISABLE_WERROR" :type bool :arch "powerpc" :description "Disable -Werror for PowerPC build")
    ("PPC_DT_CPU_FTRS" :type bool :arch "powerpc" :description "Device tree CPU feature support")
    ("PPC_EMULATED_STATS" :type bool :arch "powerpc" :description "Emulated instruction statistics")
    ("PPC_KUAP" :type bool :arch "powerpc" :description "Kernel Userspace Access Prevention")
    ("PPC_KUEP" :type bool :arch "powerpc" :description "Kernel Userspace Execution Prevention")
    ("PPC_OF_BOOT_TRAMPOLINE" :type bool :arch "powerpc" :description "OF boot trampoline support")
    ("PPC_POWERNV" :type bool :arch "powerpc" :description "IBM PowerNV (Non-Virtualized) platform")
    ("PPC_PSERIES" :type bool :arch "powerpc" :description "IBM pSeries & new iSeries")
    ("PPC_RADIX_MMU" :type bool :arch "powerpc" :description "Radix MMU support")
    ("PPC_RADIX_MMU_DEFAULT" :type bool :arch "powerpc" :description "Default to Radix MMU")
    ("PPC_SMLPAR" :type bool :arch "powerpc" :description "Support for shared-memory logical partitions")
    ("PPC_SPLPAR" :type bool :arch "powerpc" :description "Support for shared-processor logical partitions")
    ("PPC_SVM" :type bool :arch "powerpc" :description "Secure Virtual Machine (SVM) support")
    ("PPC_VAS" :type bool :arch "powerpc" :description "Virtual Accelerator Switchboard")
    ("PRINT_STACK_DEPTH" :type int :arch "powerpc" :description "Stack depth to print")
    ("PSERIES_ENERGY" :type tristate :arch "powerpc" :description "pSeries energy management driver")
    ("PSERIES_PLPKS" :type bool :arch "powerpc" :description "Support for pSeries platform keystore")
    ("RTAS_FLASH" :type tristate :arch "powerpc" :description "Firmware flash interface")
    ("RTAS_PROC" :type bool :arch "powerpc" :description "Proc interface to RTAS")
    ("SCANLOG" :type tristate :arch "powerpc" :description "Scanlog dump interface")
    ("SCOM_DEBUGFS" :type bool :arch "powerpc" :description "Expose SCOM controllers via debugfs")
    ("VSX" :type bool :arch "powerpc" :description "Vector-Scalar Extension (VSX) support")
    ("XMON" :type bool :arch "powerpc" :description "Include xmon kernel debugger")
    ("XMON_DISASSEMBLY" :type bool :arch "powerpc" :description "Enable disassembly in xmon")

    ("ARCH_CANAAN" :type bool :arch "riscv" :description "Canaan Kendryte K210 SoC")
    ("ARCH_MICROCHIP" :type bool :arch "riscv" :description "Microchip PolarFire SoC")
    ("ARCH_MICROCHIP_POLARFIRE" :type bool :arch "riscv" :description "Microchip PolarFire SoC")
    ("ARCH_SIFIVE" :type bool :arch "riscv" :description "SiFive SoCs")
    ("ARCH_SOPHGO" :type bool :arch "riscv" :description "Sophgo SoCs")
    ("ARCH_STARFIVE" :type bool :arch "riscv" :description "StarFive SoCs")
    ("ARCH_THEAD" :type bool :arch "riscv" :description "T-Head Xuantie SoCs")
    ("ARCH_VIRT" :type bool :arch "riscv" :description "QEMU Virt Machine")
    ("CRYPTO_CHACHA_RISCV64" :type tristate :arch "riscv" :description "ChaCha cipher using RISC-V vector instructions")
    ("CRYPTO_SHA256_RISCV64" :type tristate :arch "riscv" :description "SHA-256 using RISC-V vector instructions")
    ("CRYPTO_SHA512_RISCV64" :type tristate :arch "riscv" :description "SHA-512 using RISC-V vector instructions")
    ("ERRATA_ANDES" :type bool :arch "riscv" :description "Andes AX45MP errata")
    ("ERRATA_ANDES_CMO" :type bool :arch "riscv" :description "Andes AX45MP cache management errata")
    ("ERRATA_SIFIVE" :type bool :arch "riscv" :description "SiFive errata")
    ("ERRATA_SIFIVE_CIP_1200" :type bool :arch "riscv" :description "SiFive CIP-1200 errata")
    ("ERRATA_SIFIVE_CIP_453" :type bool :arch "riscv" :description "SiFive CIP-453 errata")
    ("ERRATA_THEAD" :type bool :arch "riscv" :description "T-Head errata")
    ("ERRATA_THEAD_CMO" :type bool :arch "riscv" :description "T-Head cache management errata")
    ("ERRATA_THEAD_GHOSTWRITE" :type bool :arch "riscv" :description "T-Head ghost write errata")
    ("ERRATA_THEAD_MAE" :type bool :arch "riscv" :description "T-Head misaligned access emulation")
    ("ERRATA_THEAD_PMU" :type bool :arch "riscv" :description "T-Head PMU errata")
    ("MAXPHYSMEM_128GB" :type bool :arch "riscv" :description "Support up to 128GB of physical memory")
    ("PINCTRL_STARFIVE" :type tristate :arch "riscv" :description "StarFive pinctrl driver")
    ("RISCV_BASE_PMU" :type bool :arch "riscv" :description "RISC-V Base Performance Monitoring Unit")
    ("RISCV_ISA_VENDOR_EXT_ANDES" :type bool :arch "riscv" :description "Andes vendor ISA extensions")
    ("RISCV_ISA_VENDOR_EXT_THEAD" :type bool :arch "riscv" :description "T-Head vendor ISA extensions")
    ("RISCV_ISA_XTHEADVECTOR" :type bool :arch "riscv" :description "T-Head vector extension")
    ("RISCV_PLIC" :type bool :arch "riscv" :description "Platform-Level Interrupt Controller")
    ("SND_SOC_STARFIVE" :type tristate :arch "riscv" :description "StarFive SoC audio support")
    ("SOC_STARFIVE" :type bool :arch "riscv" :description "StarFive SoC platform")

    ("CRYPTO_CHACHA_S390" :type tristate :arch "s390" :description "ChaCha cipher using s390 CPACF instructions")
    ("CRYPTO_CRC32_S390" :type tristate :arch "s390" :description "CRC-32 using s390 Vector Extension Facility")
    ("CRYPTO_SHA1_S390" :type tristate :arch "s390" :description "SHA-1 using s390 CPACF instructions")
    ("CRYPTO_SHA256_S390" :type tristate :arch "s390" :description "SHA-256 using s390 CPACF instructions")
    ("CRYPTO_SHA512_S390" :type tristate :arch "s390" :description "SHA-512 using s390 CPACF instructions")
    ("PACK_STACK" :type bool :arch "s390" :description "Pack kernel stack for s390")
    ("PROTECTED_VIRTUALIZATION_GUEST" :type bool :arch "s390" :description "Protected virtualization guest support")
    ("SCHED_BOOK" :type bool :arch "s390" :description "Book scheduling domain for s390")
    ("TN3270_TTY" :type tristate :arch "s390" :description "TN3270 terminal emulator TTY driver")
    ("ZCRYPT_MULTIDEVNODES" :type bool :arch "s390" :description "Multiple device nodes for zcrypt"))
  "List of vendor-specific and version-specific Kconfig options.
Each entry is (OPTION-NAME :key value ...) with possible keys:
:type - Option type (bool, tristate, string, int, hex)
:vendor - Vendor name (RedHat, SUSE, etc.)
:version - Minimum kernel version
:arch - Required architecture
:description - Human-readable description")


;;
;; Forward declarations for functions to be moved
;;

;; Parsing functions
(declare-function kconfig-load-data "kconfig")
(declare-function kconfig-parse-file "kconfig")
(declare-function kconfig-parse-option "kconfig")
(declare-function kconfig-collect-files "kconfig")

;; Validation functions
(declare-function kconfig-validate-option-value "kconfig")
(declare-function kconfig-validate-dependencies "kconfig")

;; Dependency evaluation
(declare-function kconfig-evaluate-condition "kconfig")

;; Query functions
(declare-function kconfig-get-dependency-info "kconfig")

;;
;; Implementation will be moved from linconf.el in subsequent steps
;;

(provide 'kconfig)
;;; kconfig.el ends here
