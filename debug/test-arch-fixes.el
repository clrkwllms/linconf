#!/usr/bin/emacs --script

;; Test script to check architecture-specific Kconfig loading fixes
;; Run with: emacs --batch -l debug/test-arch-fixes.el

(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'linconf)

(let ((test-config "test-files/kernel-aarch64-16k-debug-fedora.config")
      (linconf-kernel-source-path "/home/williams/src/linux.git"))

  (message "=== Testing Architecture-Specific Kconfig Loading ===")
  (message "Config file: %s" test-config)
  (message "Kernel source: %s" linconf-kernel-source-path)

  ;; Set up architecture detection from config file
  (let ((detected-arch (linconf-detect-architecture-from-config test-config)))
    (setq linconf-detected-architecture detected-arch)
    (message "Detected architecture: %s" detected-arch)

    ;; Clear and reload Kconfig data with architecture awareness
    (clrhash linconf-kconfig-options)
    (linconf-load-kconfig-data detected-arch)
    (message "Total options loaded: %d" (hash-table-count linconf-kconfig-options))

    ;; Test ARM64-specific options that should now be found
    (let ((arm64-options '("ARM64_16K_PAGES" "ARM64_ERRATUM_845719" "ARCH_BCM")))
      (message "\n=== ARM64-specific options test ===")
      (dolist (option arm64-options)
        (let ((data (gethash option linconf-kconfig-options)))
          (if data
              (message "✓ %s: Found (type: %s)" option (plist-get data :type))
            (message "✗ %s: NOT FOUND" option)))))

    ;; Test TEST_MISC_MINOR type
    (message "\n=== TEST_MISC_MINOR type test ===")
    (let ((data (gethash "TEST_MISC_MINOR" linconf-kconfig-options)))
      (if data
          (message "TEST_MISC_MINOR: type=%s, help=%s"
                   (plist-get data :type)
                   (substring (or (plist-get data :help) "No help") 0 50))
        (message "TEST_MISC_MINOR: NOT FOUND")))

    ;; Test validation of TEST_MISC_MINOR=m
    (message "\n=== Validation test ===")
    (let ((result (linconf-validate-option-value "TEST_MISC_MINOR" "m")))
      (message "TEST_MISC_MINOR=m validation: %s"
               (if (car result) "VALID" (format "INVALID: %s" (cdr result))))))

  (message "\n=== Test Complete ==="))