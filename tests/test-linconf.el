;;; test-linconf.el --- Tests for linconf.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Clark Williams <clrkwllms@kernel.org>

;;; Commentary:

;; Unit tests for linconf.el using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'linconf)

(ert-deftest test-kconfig-parse-kconfig-option-bool ()
  "Test parsing a simple bool option."
  (let ((lines '("config DEBUG_KERNEL"
                 "	bool \"Kernel debugging\""
                 "	help"
                 "	  Enable kernel debugging features.")))
    (let ((result (kconfig-parse-kconfig-option lines)))
      (should (equal (car result) "DEBUG_KERNEL"))
      (should (eq (plist-get (cdr result) :type) 'bool))
      (should (string-match "Enable kernel debugging" (plist-get (cdr result) :help))))))

(ert-deftest test-kconfig-parse-kconfig-option-tristate ()
  "Test parsing a tristate option with dependencies."
  (let ((lines '("config EXT4_FS"
                 "	tristate \"The Extended 4 (ext4) filesystem\""
                 "	depends on BLOCK"
                 "	select JBD2"
                 "	select CRC16"
                 "	default y"
                 "	help"
                 "	  This is the next generation of the ext3 filesystem.")))
    (let ((result (kconfig-parse-kconfig-option lines)))
      (should (equal (car result) "EXT4_FS"))
      (should (eq (plist-get (cdr result) :type) 'tristate))
      (should (equal (plist-get (cdr result) :depends) "BLOCK"))
      (should (member "JBD2" (plist-get (cdr result) :select)))
      (should (member "CRC16" (plist-get (cdr result) :select)))
      (should (equal (plist-get (cdr result) :default) "y")))))

(ert-deftest test-kconfig-parse-kconfig-option-string ()
  "Test parsing a string option."
  (let ((lines '("config LOCALVERSION"
                 "	string \"Local version - append to kernel release\""
                 "	help"
                 "	  Append an extra string to the end of your kernel version.")))
    (let ((result (kconfig-parse-kconfig-option lines)))
      (should (equal (car result) "LOCALVERSION"))
      (should (eq (plist-get (cdr result) :type) 'string)))))

(ert-deftest test-kconfig-parse-kconfig-option-int-with-range ()
  "Test parsing an int option with range."
  (let ((lines '("config LOG_BUF_SHIFT"
                 "	int \"Kernel log buffer size (16 => 64KB, 17 => 128KB)\""
                 "	range 12 25"
                 "	default 17"
                 "	help"
                 "	  Select the minimal kernel log buffer size.")))
    (let ((result (kconfig-parse-kconfig-option lines)))
      (should (equal (car result) "LOG_BUF_SHIFT"))
      (should (eq (plist-get (cdr result) :type) 'int))
      (should (equal (plist-get (cdr result) :range) '(12 . 25)))
      (should (equal (plist-get (cdr result) :default) "17")))))

(ert-deftest test-kconfig-parse-kconfig-option-hex ()
  "Test parsing a hex option."
  (let ((lines '("config PHYSICAL_START"
                 "	hex \"Physical address where the kernel is loaded\""
                 "	default \"0x1000000\""
                 "	help"
                 "	  This gives the physical address where the kernel is loaded.")))
    (let ((result (kconfig-parse-kconfig-option lines)))
      (should (equal (car result) "PHYSICAL_START"))
      (should (eq (plist-get (cdr result) :type) 'hex))
      (should (equal (plist-get (cdr result) :default) "\"0x1000000\"")))))

(ert-deftest test-kconfig-parse-kconfig-option-no-config ()
  "Test that parsing non-config lines returns nil."
  (let ((lines '("# This is just a comment"
                 "source \"drivers/Kconfig\"")))
    (should (null (kconfig-parse-kconfig-option lines)))))

(ert-deftest test-linconf-collect-kconfig-files ()
  "Test collecting Kconfig files by following source directives."
  (let ((temp-dir (make-temp-file "linconf-test" t)))
    (unwind-protect
        (progn
          ;; Create test directory structure
          (make-directory (expand-file-name "drivers" temp-dir))
          (make-directory (expand-file-name "arch/x86" temp-dir) t)
          
          ;; Create test Kconfig files with source directives
          (with-temp-file (expand-file-name "Kconfig" temp-dir)
            (insert "# Main Kconfig\n"
                    "source \"drivers/Kconfig\"\n"
                    "source \"arch/x86/Kconfig.debug\"\n"))
          (with-temp-file (expand-file-name "drivers/Kconfig" temp-dir)
            (insert "# Drivers Kconfig\n"))
          (with-temp-file (expand-file-name "arch/x86/Kconfig.debug" temp-dir)
            (insert "# Debug Kconfig\n"))
          (with-temp-file (expand-file-name "README" temp-dir)
            (insert "# Not a Kconfig file"))
          
          ;; Test the function
          (let ((files (linconf-collect-kconfig-files temp-dir)))
            (should (= (length files) 3))
            (should (cl-some (lambda (f) (string-match "Kconfig$" f)) files))
            (should (cl-some (lambda (f) (string-match "drivers/Kconfig$" f)) files))
            (should (cl-some (lambda (f) (string-match "Kconfig\\.debug$" f)) files))
            (should-not (cl-some (lambda (f) (string-match "README$" f)) files))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest test-kconfig-parse-kconfig-file ()
  "Test parsing a complete Kconfig file."
  (let ((temp-file (make-temp-file "kconfig-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "# Sample Kconfig file\n"
                    "\n"
                    "config EXAMPLE_BOOL\n"
                    "	bool \"Example boolean option\"\n"
                    "	default y\n"
                    "	help\n"
                    "	  This is an example boolean option.\n"
                    "\n"
                    "config EXAMPLE_TRISTATE\n"
                    "	tristate \"Example tristate option\"\n"
                    "	depends on EXAMPLE_BOOL\n"
                    "	help\n"
                    "	  This is an example tristate option.\n"))
          
          (let ((options (kconfig-parse-kconfig-file temp-file)))
            (should (= (length options) 2))
            
            ;; Check first option
            (let ((bool-option (assoc "EXAMPLE_BOOL" options)))
              (should bool-option)
              (should (eq (plist-get (cdr bool-option) :type) 'bool))
              (should (equal (plist-get (cdr bool-option) :default) "y")))
            
            ;; Check second option
            (let ((tri-option (assoc "EXAMPLE_TRISTATE" options)))
              (should tri-option)
              (should (eq (plist-get (cdr tri-option) :type) 'tristate))
              (should (equal (plist-get (cdr tri-option) :depends) "EXAMPLE_BOOL")))))
      ;; Cleanup
      (delete-file temp-file))))

(ert-deftest test-linconf-get-option-name ()
  "Test extracting option names from various config line formats."
  (with-temp-buffer
    (linconf-mode)
    
    ;; Test enabled option
    (erase-buffer)
    (insert "CONFIG_DEBUG_KERNEL=y")
    (goto-char (point-min))
    (should (equal (linconf-get-option-name) "DEBUG_KERNEL"))
    
    ;; Test disabled option
    (erase-buffer)
    (insert "# CONFIG_UNUSED_SYMBOLS is not set")
    (goto-char (point-min))
    (should (equal (linconf-get-option-name) "UNUSED_SYMBOLS"))
    
    ;; Test module option
    (erase-buffer)
    (insert "CONFIG_EXT4_FS=m")
    (goto-char (point-min))
    (should (equal (linconf-get-option-name) "EXT4_FS"))
    
    ;; Test string option
    (erase-buffer)
    (insert "CONFIG_LOCALVERSION=\"-custom\"")
    (goto-char (point-min))
    (should (equal (linconf-get-option-name) "LOCALVERSION"))
    
    ;; Test invalid line
    (erase-buffer)
    (insert "# This is just a comment")
    (goto-char (point-min))
    (should (null (linconf-get-option-name)))))

(ert-deftest test-linconf-set-option ()
  "Test setting option values."
  (with-temp-buffer
    (linconf-mode)
    
    ;; Test setting to y
    (insert "# CONFIG_DEBUG_KERNEL is not set")
    (goto-char (point-min))
    (linconf-set-option "DEBUG_KERNEL" "y")
    (should (equal (buffer-string) "CONFIG_DEBUG_KERNEL=y"))
    
    ;; Test setting to nil (unset)
    (erase-buffer)
    (insert "CONFIG_DEBUG_KERNEL=y")
    (goto-char (point-min))
    (linconf-set-option "DEBUG_KERNEL" nil)
    (should (equal (buffer-string) "# CONFIG_DEBUG_KERNEL is not set"))
    
    ;; Test setting to string
    (erase-buffer)
    (insert "CONFIG_LOCALVERSION=\"\"")
    (goto-char (point-min))
    (linconf-set-option "LOCALVERSION" "\"-test\"")
    (should (equal (buffer-string) "CONFIG_LOCALVERSION=\"-test\""))))

(provide 'test-linconf)

;;; test-linconf.el ends here