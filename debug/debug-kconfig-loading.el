#!/usr/bin/emacs --script

;; Debug script to test Kconfig loading and TEST_MISC_MINOR parsing
;; Run with: emacs --batch -l debug/debug-kconfig-loading.el

(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'linconf)

(let ((linconf-kernel-source-path "/usr/src/kernels/6.16.5-200.fc42.x86_64"))
  (message "=== LinConf Kconfig Loading Debug ===")
  (message "Kernel source path: %s" linconf-kernel-source-path)

  ;; Initialize empty hash tables
  (setq linconf-kconfig-options (make-hash-table :test 'equal))
  (setq linconf-config-values (make-hash-table :test 'equal))

  ;; Force load Kconfig
  (message "Loading Kconfig files...")
  (linconf-parse-kconfig-file "Kconfig")

  (message "Total options loaded: %d" (hash-table-count linconf-kconfig-options))

  ;; Check for TEST_MISC_MINOR
  (let ((test-misc-data (gethash "TEST_MISC_MINOR" linconf-kconfig-options)))
    (if test-misc-data
        (progn
          (message "✓ TEST_MISC_MINOR found!")
          (message "  Type: %s" (plist-get test-misc-data :type))
          (message "  Help: %s" (truncate-string-to-width
                                 (or (plist-get test-misc-data :help) "No help") 80))
          (message "  Source: %s" (plist-get test-misc-data :source-file)))
      (message "✗ TEST_MISC_MINOR NOT found!")))

  ;; Test validation
  (message "\n=== Validation Tests ===")
  (let ((result (linconf-validate-option-value "TEST_MISC_MINOR" "m")))
    (message "Validate TEST_MISC_MINOR=m: %s"
             (if (car result) "VALID" (format "INVALID: %s" (cdr result)))))

  ;; Check a few other test options that should be tristate
  (message "\n=== Other test options ===")
  (dolist (option '("TEST_MIN_HEAP" "TEST_LOCKUP"))
    (let ((data (gethash option linconf-kconfig-options)))
      (if data
          (message "%s: type=%s" option (plist-get data :type))
        (message "%s: NOT FOUND" option))))

  (message "\n=== Debug Complete ==="))