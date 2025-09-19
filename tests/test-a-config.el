#!/usr/bin/emacs --script
;;; test-a-config.el --- Test a single config file for debugging -*- lexical-binding: t; -*-

;; Usage: emacs --batch -l tests/test-a-config.el CONFIG_FILE_PATH
;; Example: emacs --batch -l tests/test-a-config.el test-files/kernel-aarch64-16k-debug-fedora.config

;; Add parent directory to load path so we can find linconf.el
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(load "linconf.el")
(message "Loaded linconf.el, function available: %s" (fboundp 'linconf-validate-all-options))

(defun test-single-config-file (config-file)
  "Test a single config file and provide detailed debugging output."
  (unless (file-readable-p config-file)
    (error "Config file not readable: %s" config-file))

  (message "=== LinConf Single Config Test ===")
  (message "Config file: %s" config-file)

  ;; Set kernel source path
  (setq linconf-kernel-source-path "/usr/src/kernels/6.16.5-200.fc42.x86_64")
  (message "Kernel source: %s" linconf-kernel-source-path)

  (let ((start-time (current-time)))
    (message "\n=== Testing Configuration ===")

    ;; Test with temp buffer (like the test suite)
    (with-temp-buffer
      (insert-file-contents config-file)
      ;; Set buffer-file-name so architecture detection works
      (setq buffer-file-name config-file)

      (message "Activating linconf-mode...")
      (linconf-mode)
      (message "LinConf mode activated, functions available: %s"
               (fboundp 'linconf-validate-all-options))

      (message "Running validation...")
      (if (fboundp 'linconf-validate-all-options)
          (linconf-validate-all-options)
        (error "Function linconf-validate-all-options not found")))

    ;; Parse and display results
    (let ((results-buffer "*LinConf Validation*"))
      (if (get-buffer results-buffer)
          (with-current-buffer results-buffer
            (goto-char (point-min))

            ;; Extract summary info
            (let ((valid-count 0)
                  (invalid-count 0)
                  (warning-count 0)
                  (arch nil))

              ;; Look for architecture line
              (when (re-search-forward "^Architecture: \\(.+\\)" nil t)
                (setq arch (match-string 1)))

              ;; Look for summary lines
              (goto-char (point-min))
              (when (re-search-forward "^Valid options: \\([0-9]+\\)" nil t)
                (setq valid-count (string-to-number (match-string 1))))

              (goto-char (point-min))
              (when (re-search-forward "^Errors: \\([0-9]+\\)" nil t)
                (setq invalid-count (string-to-number (match-string 1))))

              (goto-char (point-min))
              (when (re-search-forward "^Warnings: \\([0-9]+\\)" nil t)
                (setq warning-count (string-to-number (match-string 1))))

              (let ((total-count (+ valid-count invalid-count warning-count))
                    (elapsed (float-time (time-subtract (current-time) start-time))))

                (message "\n=== RESULTS SUMMARY ===")
                (message "Architecture detected: %s" (or arch "NONE"))
                (message "Valid options: %d" valid-count)
                (message "Invalid options: %d" invalid-count)
                (message "Warning options: %d" warning-count)
                (message "Total options: %d" total-count)
                (message "Success rate: %.1f%%"
                         (if (> total-count 0)
                             (* 100.0 (/ (float valid-count) total-count))
                           0.0))
                (message "Validation time: %.2f seconds" elapsed)

                ;; Show first few errors for debugging
                (when (> invalid-count 0)
                  (message "\n=== FIRST FEW ERRORS ===")
                  (goto-char (point-min))
                  (when (re-search-forward "^ERRORS:" nil t)
                    (forward-line 1)
                    (let ((error-count 0))
                      (while (and (< error-count 5)
                                  (looking-at "[ \\t]*• Line \\([0-9]+\\): \\(.+\\)"))
                        (message "Line %s: %s" (match-string 1) (match-string 2))
                        (forward-line 1)
                        (setq error-count (1+ error-count))))))

                ;; Show first few warnings if there are any
                (when (> warning-count 0)
                  (message "\n=== FIRST FEW WARNINGS ===")
                  (goto-char (point-min))
                  (when (re-search-forward "^WARNINGS:" nil t)
                    (forward-line 1)
                    (let ((warning-count 0))
                      (while (and (< warning-count 5)
                                  (looking-at "[ \\t]*• Line \\([0-9]+\\): \\(.+\\)"))
                        (message "Line %s: %s" (match-string 1) (match-string 2))
                        (forward-line 1)
                        (setq warning-count (1+ warning-count))))))

                ;; Save detailed results to file for inspection
                (let ((output-file (format "%s-debug-results.txt"
                                          (file-name-base config-file))))
                  (write-file output-file)
                  (message "\nDetailed results saved to: %s" output-file))))

            (kill-buffer))
        (message "ERROR: No validation results found!"))))

  (message "\n=== Test Complete ==="))

;; Main execution
(let ((config-file (or (car command-line-args-left)
                       (error "Usage: emacs --batch -l test-a-config.el CONFIG_FILE_PATH"))))
  (test-single-config-file config-file))