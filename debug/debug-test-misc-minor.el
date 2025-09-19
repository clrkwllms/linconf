#!/usr/bin/emacs --script

;; Debug script to investigate TEST_MISC_MINOR type detection issue
;; Run with: emacs --batch -l debug/debug-test-misc-minor.el

(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'linconf)

(let ((linconf-kernel-source-path "/usr/src/kernels"))
  (message "=== TEST_MISC_MINOR Debug Analysis ===")

  ;; Load Kconfig definitions
  (message "Loading Kconfig definitions...")
  (linconf-parse-kconfig-file "Kconfig")

  ;; Check what type TEST_MISC_MINOR is parsed as
  (let ((option-data (gethash "TEST_MISC_MINOR" linconf-kconfig-options)))
    (if option-data
        (progn
          (message "TEST_MISC_MINOR found in Kconfig definitions:")
          (message "  Type: %s" (plist-get option-data :type))
          (message "  Help: %s" (truncate-string-to-width
                                  (or (plist-get option-data :help) "No help") 60))
          (message "  Depends: %s" (plist-get option-data :depends))
          (message "  Default: %s" (plist-get option-data :default))
          (message "  Source file: %s" (plist-get option-data :source-file)))
      (message "TEST_MISC_MINOR NOT found in Kconfig definitions!")))

  ;; Test validation of the actual value
  (message "\n=== Validation Test ===")
  (let ((validation-result (linconf-validate-option-value "TEST_MISC_MINOR" "m")))
    (message "Validating TEST_MISC_MINOR=m: %s"
             (if (car validation-result) "VALID"
               (format "INVALID - %s" (cdr validation-result)))))

  ;; Test if it's a boolean vs tristate issue
  (message "\n=== Type-specific validation tests ===")
  (let ((bool-result (linconf-validate-bool-value "m"))
        (tristate-result (linconf-validate-tristate-value "m")))
    (message "Boolean validation of 'm': %s"
             (if (car bool-result) "VALID" (format "INVALID - %s" (cdr bool-result))))
    (message "Tristate validation of 'm': %s"
             (if (car tristate-result) "VALID" (format "INVALID - %s" (cdr tristate-result)))))

  (message "\n=== Debug Complete ==="))
