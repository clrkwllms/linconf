;;; debug-real-source-tracking.el --- Test with real kernel source -*- lexical-binding: t; -*-

;; Test source file tracking with actual kernel files

(require 'linconf)

(defun test-real-source-tracking ()
  "Test source file tracking with real kernel Kconfig files."
  (message "=== Testing Real Source File Tracking ===")

  (let ((kernel-root "/usr/src/kernels/6.15.10-200.fc42.x86_64")
        (test-file "/usr/src/kernels/6.15.10-200.fc42.x86_64/kernel/bpf/Kconfig"))

    (when (file-exists-p test-file)
      (message "Testing with: %s" test-file)

      ;; Set kernel source path for relative path calculation
      (setq linconf-kernel-source-path kernel-root)

      ;; Parse the file
      (let ((options (linconf-parse-kconfig-file test-file)))
        (message "Found %d options in file" (length options))

        ;; Test first few options
        (let ((count 0))
          (dolist (option options)
            (when (< count 3)
              (let* ((option-name (car option))
                     (option-plist (cdr option))
                     (source-file (plist-get option-plist :source-file))
                     (relative-path (linconf-relative-source-path source-file))
                     (option-type (plist-get option-plist :type)))
                (message "Option %d: %s" (1+ count) option-name)
                (message "  Type: %s" option-type)
                (message "  Source file: %s" source-file)
                (message "  Relative path: %s" relative-path)
                (setq count (1+ count)))))

        ;; Simulate showing help for an option
        (when options
          (let* ((first-option (car options))
                 (option-name (car first-option))
                 (option-plist (cdr first-option)))
            (message "\nSimulating help display for %s:" option-name)
            (puthash option-name option-plist linconf-kconfig-options)

            ;; Test the help display function components
            (let ((source-file (plist-get option-plist :source-file)))
              (when source-file
                (let ((relative-path (linconf-relative-source-path source-file)))
                  (message "Help would show: Source: %s" relative-path)))))))

    (unless (file-exists-p test-file)
      (message "Kernel source not available at expected location")))

  (message "=== Real Source File Tracking Test Complete ==="))

(provide 'debug-real-source-tracking)

;;; debug-real-source-tracking.el ends here