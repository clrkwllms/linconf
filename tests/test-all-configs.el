;;; test-all-configs.el --- Test all config files in test-files directory -*- lexical-binding: t; -*-

(add-to-list 'load-path "..")
(require 'kconfig)
(require 'linconf)

(defun test-all-config-files ()
  "Test all config files in test-files directory."
  (interactive)
  (let* ((arch-filter (getenv "LINCONF_ARCH_FILTER"))
         (all-files (directory-files "test-files" t "\\.config$"))
         (config-files (if arch-filter
                          (seq-filter (lambda (f)
                                       (string-match-p (concat "kernel-" arch-filter "-")
                                                      (file-name-nondirectory f)))
                                     all-files)
                        all-files))
         (results-file "test-results/all-config-validation-results.txt")
         (all-results '())
         (start-time (current-time)))

    ;; Set kernel source path for proper validation
    (setq linconf-kernel-source-path (expand-file-name "~/src/linux.git"))
    (message "Using kernel source: %s" linconf-kernel-source-path)

    (unless config-files
      (error "No .config files found in test-files/"))

    (message "Found %d config files to test" (length config-files))
    (make-directory "test-results" t)

    ;; Test each config file
    (dolist (config-file config-files)
      (message "Testing %s..." (file-name-nondirectory config-file))
      (let ((file-start-time (current-time))
            (valid-count 0)
            (invalid-count 0)
            (warning-count 0))

        ;; Open file and run validation
        (with-temp-buffer
          (insert-file-contents config-file)
          ;; Set buffer-file-name so architecture detection works
          (setq buffer-file-name config-file)
          (linconf-mode)
          (linconf-validate-all-options))

        ;; Parse results from validation buffer
        (let ((results-buffer "*LinConf Validation*"))
          (when (get-buffer results-buffer)
            (with-current-buffer results-buffer
              (goto-char (point-min))
              ;; Look for summary line first: "Valid options: N"
              (if (re-search-forward "^Valid options: \\([0-9]+\\)" nil t)
                  (setq valid-count (string-to-number (match-string 1)))
                (setq valid-count (how-many " - valid$" (point-min) (point-max))))

              ;; Look for errors summary: "Errors: N"
              (goto-char (point-min))
              (if (re-search-forward "^Errors: \\([0-9]+\\)" nil t)
                  (setq invalid-count (string-to-number (match-string 1)))
                (setq invalid-count (how-many "ERROR\\|INVALID" (point-min) (point-max))))

              ;; Look for warnings summary: "Warnings: N"
              (goto-char (point-min))
              (if (re-search-forward "^Warnings: \\([0-9]+\\)" nil t)
                  (setq warning-count (string-to-number (match-string 1)))
                (setq warning-count (how-many "warning:" (point-min) (point-max))))

              ;; Save individual results to file
              (let ((individual-file (format "test-results/%s-results.txt"
                                           (file-name-base config-file))))
                (write-file individual-file))
              (kill-buffer))))

        (let ((elapsed (float-time (time-subtract (current-time) file-start-time))))
          (push (list :file (file-name-nondirectory config-file)
                     :valid valid-count
                     :invalid invalid-count
                     :warnings warning-count
                     :total (+ valid-count invalid-count warning-count)
                     :time elapsed)
                all-results)
          (message "  -> Valid: %d, Invalid: %d, Warnings: %d, Total: %d (%.2fs)"
                  valid-count invalid-count warning-count
                  (+ valid-count invalid-count warning-count) elapsed))))

    ;; Calculate totals and generate summary
    (let ((total-time (float-time (time-subtract (current-time) start-time)))
          (total-valid 0)
          (total-invalid 0)
          (total-warnings 0)
          (total-configs (length all-results)))

      (dolist (result all-results)
        (setq total-valid (+ total-valid (plist-get result :valid)))
        (setq total-invalid (+ total-invalid (plist-get result :invalid)))
        (setq total-warnings (+ total-warnings (plist-get result :warnings))))

      ;; Write comprehensive summary
      (with-temp-file results-file
        (insert "LinConf Comprehensive Config Validation Results\n")
        (insert "===============================================\n\n")
        (insert (format "Test Run: %s\n" (current-time-string)))
        (insert (format "Total Config Files Tested: %d\n" total-configs))
        (insert (format "Total Execution Time: %.2f seconds\n\n" total-time))

        (insert "OVERALL SUMMARY:\n")
        (insert "================\n")
        (insert (format "Total Valid Options: %d\n" total-valid))
        (insert (format "Total Invalid Options: %d\n" total-invalid))
        (insert (format "Total Warning Options: %d\n" total-warnings))
        (insert (format "Grand Total Options: %d\n\n" (+ total-valid total-invalid total-warnings)))

        (insert (format "Success Rate: %.2f%%\n"
                        (* 100.0 (/ (float total-valid) (+ total-valid total-invalid total-warnings)))))
        (insert (format "Error Rate: %.2f%%\n"
                        (* 100.0 (/ (float total-invalid) (+ total-valid total-invalid total-warnings)))))
        (insert (format "Warning Rate: %.2f%%\n\n"
                        (* 100.0 (/ (float total-warnings) (+ total-valid total-invalid total-warnings)))))

        (insert "PERFORMANCE STATISTICS:\n")
        (insert "=======================\n")
        (insert (format "Average validation time per file: %.2f seconds\n"
                        (/ total-time total-configs)))
        (insert (format "Average options per file: %.1f\n"
                        (/ (float (+ total-valid total-invalid total-warnings)) total-configs)))
        (insert (format "Fastest file validation: %.2f seconds\n"
                        (apply 'min (mapcar (lambda (r) (plist-get r :time)) all-results))))
        (insert (format "Slowest file validation: %.2f seconds\n\n"
                        (apply 'max (mapcar (lambda (r) (plist-get r :time)) all-results))))

        (insert "DETAILED RESULTS BY CONFIG FILE:\n")
        (insert "=================================\n\n")

        ;; Sort results by filename
        (setq all-results (sort all-results
                               (lambda (a b)
                                 (string< (plist-get a :file) (plist-get b :file)))))

        (dolist (result all-results)
          (insert (format "File: %s\n" (plist-get result :file)))
          (insert (format "  Valid Options: %d\n" (plist-get result :valid)))
          (insert (format "  Invalid Options: %d\n" (plist-get result :invalid)))
          (insert (format "  Warning Options: %d\n" (plist-get result :warnings)))
          (insert (format "  Total Options: %d\n" (plist-get result :total)))
          (insert (format "  Validation Time: %.2f seconds\n" (plist-get result :time)))
          (insert (format "  Success Rate: %.1f%%\n"
                          (if (> (plist-get result :total) 0)
                              (* 100.0 (/ (float (plist-get result :valid)) (plist-get result :total)))
                            0.0)))
          (insert "\n")))

      (message "\n=== COMPREHENSIVE CONFIG VALIDATION COMPLETE ===")
      (message "Tested %d config files in %.2f seconds" total-configs total-time)
      (message "Grand totals: %d valid, %d invalid, %d warnings (%d total options)"
               total-valid total-invalid total-warnings (+ total-valid total-invalid total-warnings))
      (message "Overall success rate: %.2f%%"
               (* 100.0 (/ (float total-valid) (+ total-valid total-invalid total-warnings))))
      (message "Results written to: %s" results-file)
      (message "Individual results saved in test-results/")

      all-results)))

;; Auto-run when loaded in batch mode
(when noninteractive
  (test-all-config-files))

(provide 'test-all-configs)
;;; test-all-configs.el ends here