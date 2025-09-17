;;; test-simple-config-validation.el --- Simple config validation test -*- lexical-binding: t; -*-

(require 'linconf)

(defun test-validate-config-files ()
  "Simple test to validate a few config files and save results."
  (interactive)
  (let ((test-files '("test-files/kernel-aarch64-fedora.config"
                     "test-files/kernel-x86_64-fedora.config"
                     "test-files/kernel-riscv64-fedora.config"))
        (results-file "test-results/validation-summary.txt")
        (all-results '()))

    ;; Ensure results directory exists
    (make-directory "test-results" t)

    (dolist (config-file test-files)
      (when (file-exists-p config-file)
        (message "Testing %s..." config-file)
        (let ((start-time (current-time)))

          ;; Open file and run validation
          (with-temp-buffer
            (insert-file-contents config-file)
            (linconf-mode)
            (linconf-validate-all-options))

          ;; Check results buffer
          (let ((results-buffer "*LinConf Validation*")
                (valid-count 0)
                (invalid-count 0)
                (warning-count 0))

            (when (get-buffer results-buffer)
              (with-current-buffer results-buffer
                (goto-char (point-min))
                (let ((content (buffer-string)))
                  ;; Count different types of results using built-in functions
                  (setq valid-count (how-many "valid$" (point-min) (point-max)))
                  (setq invalid-count (how-many "ERROR\\|INVALID" (point-min) (point-max)))
                  (setq warning-count (how-many "warning:" (point-min) (point-max))))

                ;; Save buffer content to file
                (let ((result-file (format "test-results/%s-validation.txt"
                                         (file-name-base config-file))))
                  (write-file result-file))
                (kill-buffer)))

            (let ((elapsed (float-time (time-subtract (current-time) start-time))))
              (push (list :file config-file
                         :valid valid-count
                         :invalid invalid-count
                         :warnings warning-count
                         :time elapsed)
                    all-results)
              (message "  -> Valid: %d, Invalid: %d, Warnings: %d (%.2fs)"
                      valid-count invalid-count warning-count elapsed))))))

    ;; Write summary
    (with-temp-file results-file
      (insert "LinConf Validation Test Summary\n")
      (insert "===============================\n\n")
      (insert (format "Test run: %s\n\n" (current-time-string)))

      (dolist (result all-results)
        (insert (format "File: %s\n" (plist-get result :file)))
        (insert (format "  Valid: %d\n" (plist-get result :valid)))
        (insert (format "  Invalid: %d\n" (plist-get result :invalid)))
        (insert (format "  Warnings: %d\n" (plist-get result :warnings)))
        (insert (format "  Time: %.2f seconds\n\n" (plist-get result :time)))))

    (message "Results written to %s" results-file)
    all-results))

;; Auto-run when loaded in batch mode
(when noninteractive
  (test-validate-config-files))

(provide 'test-simple-config-validation)
;;; test-simple-config-validation.el ends here