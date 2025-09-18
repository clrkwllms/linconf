;;; debug-source-file-tracking.el --- Test source file path tracking functionality -*- lexical-binding: t; -*-

;; Test the new source file path tracking functionality

(require 'linconf)

(defun test-source-file-tracking ()
  "Test that source file paths are correctly tracked and displayed."
  (message "=== Testing Source File Path Tracking ===")

  ;; Create a temporary test Kconfig file
  (let ((test-file "/tmp/test-kconfig-source"))
    (with-temp-file test-file
      (insert "config TEST_OPTION\n")
      (insert "\tbool \"Test option for source tracking\"\n")
      (insert "\thelp\n")
      (insert "\t  This is a test option to verify source file tracking.\n"))

    ;; Test parsing with source file tracking
    (message "Testing linconf-parse-kconfig-option with source file...")
    (let* ((lines '("config TEST_OPTION"
                   "\tbool \"Test option for source tracking\""
                   "\thelp"
                   "\t  This is a test option to verify source file tracking."))
           (result (linconf-parse-kconfig-option lines 'config test-file)))

      (if result
          (let* ((option-name (car result))
                 (option-plist (cdr result))
                 (source-file (plist-get option-plist :source-file)))
            (message "✓ Parsed option: %s" option-name)
            (message "✓ Source file: %s" source-file)
            (if (equal source-file test-file)
                (message "✓ Source file correctly stored")
              (message "✗ Source file incorrect. Expected: %s, Got: %s" test-file source-file)))
        (message "✗ Failed to parse test option")))

    ;; Test file parsing
    (message "\nTesting linconf-parse-kconfig-file with source tracking...")
    (let ((options (linconf-parse-kconfig-file test-file)))
      (if options
          (let* ((option (car options))
                 (option-name (car option))
                 (option-plist (cdr option))
                 (source-file (plist-get option-plist :source-file)))
            (message "✓ File parsing successful")
            (message "✓ Option: %s" option-name)
            (message "✓ Source file: %s" source-file)
            (if (equal source-file test-file)
                (message "✓ Source file correctly tracked through file parsing")
              (message "✗ Source file incorrect in file parsing. Expected: %s, Got: %s" test-file source-file)))
        (message "✗ File parsing failed")))

    ;; Test relative path function
    (message "\nTesting linconf-relative-source-path...")
    (let ((test-cases `(("/kernel/source/arch/x86/Kconfig" "arch/x86/Kconfig")
                       ("/some/other/path/Kconfig" "Kconfig")
                       ("relative/path/Kconfig" "Kconfig")
                       (nil nil))))
      (let ((linconf-kernel-source-path "/kernel/source"))
        (dolist (test-case test-cases)
          (let* ((input (car test-case))
                 (expected (cadr test-case))
                 (result (linconf-relative-source-path input)))
            (if (equal result expected)
                (message "✓ Relative path test: %s -> %s" input result)
              (message "✗ Relative path test failed: %s -> %s (expected %s)" input result expected))))))

    ;; Cleanup
    (delete-file test-file)
    (message "\n=== Source File Path Tracking Test Complete ===")))