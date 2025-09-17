;;; test-config-validation-suite.el --- Comprehensive validation test for all config files -*- lexical-binding: t; -*-

(require 'linconf)

(defvar test-config-validation-results-file
  (expand-file-name "test-results/config-validation-results.txt"
                    (file-name-directory
                     (file-name-directory (or load-file-name buffer-file-name)))))

(defvar test-config-files-directory
  (expand-file-name "test-files"
                    (file-name-directory
                     (file-name-directory (or load-file-name buffer-file-name)))))

(defun test-ensure-results-directory ()
  "Ensure test-results directory exists."
  (let ((results-dir (file-name-directory test-config-validation-results-file)))
    (unless (file-exists-p results-dir)
      (make-directory results-dir t))))

(defun test-get-all-config-files ()
  "Get list of all .config files in test-files directory."
  (directory-files test-config-files-directory t "\\.config$"))

(defun test-validate-single-config (config-file)
  "Validate a single config file and return results summary."
  (let ((start-time (current-time))
        (results-buffer "*LinConf Validation*")
        valid-count invalid-count warnings-count)

    ;; Open config file in temporary buffer
    (with-temp-buffer
      (insert-file-contents config-file)
      (linconf-mode)

      ;; Run validation and capture results
      (linconf-validate-all-options))

    ;; Parse validation results from the results buffer
    (when (get-buffer results-buffer)
      (with-current-buffer results-buffer
        (goto-char (point-min))

        ;; Count different types of results using built-in functions
        (setq valid-count (how-many "valid$" (point-min) (point-max)))
        (setq invalid-count (how-many "ERROR\\|INVALID" (point-min) (point-max)))
        (setq warnings-count (how-many "warning:" (point-min) (point-max)))

        ;; Look for summary line
        (goto-char (point-min))
        (when (re-search-forward "Total: \\([0-9]+\\) options validated" nil t)
          (let ((total-validated (string-to-number (match-string 1))))
            (when (= valid-count 0)  ; If we didn't count individual lines, use summary
              (goto-char (point-min))
              (when (re-search-forward "Valid: \\([0-9]+\\)" nil t)
                (setq valid-count (string-to-number (match-string 1))))
              (when (re-search-forward "Invalid: \\([0-9]+\\)" nil t)
                (setq invalid-count (string-to-number (match-string 1))))
              (when (re-search-forward "Warnings: \\([0-9]+\\)" nil t)
                (setq warnings-count (string-to-number (match-string 1)))))))

        (kill-buffer results-buffer)))

    (let ((elapsed-time (float-time (time-subtract (current-time) start-time))))
      (list :file (file-name-nondirectory config-file)
            :valid valid-count
            :invalid invalid-count
            :warnings warnings-count
            :total (+ valid-count invalid-count warnings-count)
            :time elapsed-time))))

(defun test-run-config-validation-suite ()
  "Run validation test on all config files and generate results report."
  (interactive)
  (message "Starting comprehensive config validation test suite...")

  (test-ensure-results-directory)

  (let ((config-files (test-get-all-config-files))
        (all-results '())
        (start-time (current-time)))

    (unless config-files
      (error "No .config files found in %s" test-config-files-directory))

    (message "Found %d config files to test" (length config-files))

    ;; Test each config file
    (dolist (config-file config-files)
      (message "Testing %s..." (file-name-nondirectory config-file))
      (let ((result (test-validate-single-config config-file)))
        (push result all-results)
        (message "  -> Valid: %d, Invalid: %d, Warnings: %d (%.2fs)"
                 (plist-get result :valid)
                 (plist-get result :invalid)
                 (plist-get result :warnings)
                 (plist-get result :time))))

    ;; Generate summary report
    (let ((total-time (float-time (time-subtract (current-time) start-time)))
          (total-valid 0)
          (total-invalid 0)
          (total-warnings 0)
          (total-configs (length all-results)))

      ;; Calculate totals
      (dolist (result all-results)
        (setq total-valid (+ total-valid (plist-get result :valid)))
        (setq total-invalid (+ total-invalid (plist-get result :invalid)))
        (setq total-warnings (+ total-warnings (plist-get result :warnings))))

      ;; Write detailed results to file
      (with-temp-file test-config-validation-results-file
        (insert (format "LinConf Config Validation Test Suite Results\n"))
        (insert (format "==========================================\n\n"))
        (insert (format "Test Run: %s\n" (current-time-string)))
        (insert (format "Total Config Files: %d\n" total-configs))
        (insert (format "Total Execution Time: %.2f seconds\n\n" total-time))

        (insert "SUMMARY:\n")
        (insert (format "  Total Valid Options: %d\n" total-valid))
        (insert (format "  Total Invalid Options: %d\n" total-invalid))
        (insert (format "  Total Warnings: %d\n" total-warnings))
        (insert (format "  Grand Total Options: %d\n\n" (+ total-valid total-invalid total-warnings)))

        (insert "DETAILED RESULTS BY CONFIG FILE:\n")
        (insert "================================\n\n")

        ;; Sort results by filename for consistent output
        (setq all-results (sort all-results
                               (lambda (a b)
                                 (string< (plist-get a :file) (plist-get b :file)))))

        (dolist (result all-results)
          (insert (format "File: %s\n" (plist-get result :file)))
          (insert (format "  Valid: %d\n" (plist-get result :valid)))
          (insert (format "  Invalid: %d\n" (plist-get result :invalid)))
          (insert (format "  Warnings: %d\n" (plist-get result :warnings)))
          (insert (format "  Total: %d\n" (plist-get result :total)))
          (insert (format "  Time: %.2f seconds\n\n" (plist-get result :time))))

        (insert "\nTEST COMPLETION STATISTICS:\n")
        (insert "===========================\n")
        (insert (format "Average validation time per file: %.2f seconds\n"
                        (/ total-time total-configs)))
        (insert (format "Average options per file: %.1f\n"
                        (/ (float (+ total-valid total-invalid total-warnings)) total-configs)))
        (insert (format "Success rate (valid options): %.2f%%\n"
                        (* 100.0 (/ (float total-valid) (+ total-valid total-invalid total-warnings)))))
        (insert (format "Warning rate: %.2f%%\n"
                        (* 100.0 (/ (float total-warnings) (+ total-valid total-invalid total-warnings))))))

      ;; Display summary
      (message "\n=== CONFIG VALIDATION TEST SUITE COMPLETE ===")
      (message "Tested %d config files in %.2f seconds" total-configs total-time)
      (message "Results: %d valid, %d invalid, %d warnings"
               total-valid total-invalid total-warnings)
      (message "Success rate: %.2f%%"
               (* 100.0 (/ (float total-valid) (+ total-valid total-invalid total-warnings))))
      (message "Detailed results written to: %s" test-config-validation-results-file)

      ;; Return summary for programmatic use
      (list :total-configs total-configs
            :total-valid total-valid
            :total-invalid total-invalid
            :total-warnings total-warnings
            :total-time total-time
            :results-file test-config-validation-results-file))))

;; Run the test suite when loaded in batch mode
(when noninteractive
  (test-run-config-validation-suite))

(provide 'test-config-validation-suite)
;;; test-config-validation-suite.el ends here