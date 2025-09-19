#!/usr/bin/emacs --script

;; Debug script to investigate TEST_MISC_MINOR type parsing issue
;; Run with: emacs --batch -l debug/debug-test-misc-minor-parsing.el

(add-to-list 'load-path (expand-file-name "../" (file-name-directory load-file-name)))
(require 'linconf)

(let ((linconf-kernel-source-path "/home/williams/src/linux.git"))
  (message "=== TEST_MISC_MINOR Type Parsing Debug ===")

  ;; First, let's parse lib/Kconfig.debug directly and look for TEST_MISC_MINOR
  (let ((kconfig-debug-file (expand-file-name "lib/Kconfig.debug" linconf-kernel-source-path)))
    (message "Parsing %s directly..." kconfig-debug-file)

    (when (file-readable-p kconfig-debug-file)
      (let ((options (linconf-parse-kconfig-file kconfig-debug-file)))
        (message "Found %d options in lib/Kconfig.debug" (length options))

        ;; Look for TEST_MISC_MINOR specifically
        (let ((test-misc-option (assoc "TEST_MISC_MINOR" options)))
          (if test-misc-option
              (progn
                (message "✓ Found TEST_MISC_MINOR in lib/Kconfig.debug!")
                (message "  Type: %s" (plist-get (cdr test-misc-option) :type))
                (message "  Help: %s" (truncate-string-to-width
                                      (or (plist-get (cdr test-misc-option) :help) "No help") 80))
                (message "  Full plist: %S" (cdr test-misc-option)))
            (message "✗ TEST_MISC_MINOR NOT found in lib/Kconfig.debug")))

        ;; Also check a few other test options for comparison
        (dolist (test-option '("TEST_MIN_HEAP" "TEST_LOCKUP" "TEST_KMOD"))
          (let ((option-data (assoc test-option options)))
            (if option-data
                (message "%s: type=%s" test-option (plist-get (cdr option-data) :type))
              (message "%s: NOT FOUND" test-option)))))))

  ;; Now test full Kconfig loading
  (message "\n=== Full Kconfig Loading Test ===")
  (clrhash linconf-kconfig-options)
  (linconf-load-kconfig-data)
  (message "Total options loaded: %d" (hash-table-count linconf-kconfig-options))

  (let ((test-misc-data (gethash "TEST_MISC_MINOR" linconf-kconfig-options)))
    (if test-misc-data
        (progn
          (message "✓ TEST_MISC_MINOR found in full hash table!")
          (message "  Type: %s" (plist-get test-misc-data :type))
          (message "  Help: %s" (truncate-string-to-width
                                 (or (plist-get test-misc-data :help) "No help") 80))
          (message "  Source: %s" (plist-get test-misc-data :source-file)))
      (message "✗ TEST_MISC_MINOR NOT found in full hash table!")))

  ;; Test parsing the exact lines from the Kconfig file
  (message "\n=== Direct Line Parsing Test ===")
  (let ((test-lines '("config TEST_MISC_MINOR"
                     "	tristate \"miscdevice KUnit test\" if !KUNIT_ALL_TESTS"
                     "	depends on KUNIT"
                     "	default KUNIT_ALL_TESTS"
                     "	help"
                     "	  Kunit test for miscdevice API, specially its behavior in respect to"
                     "	  static and dynamic minor numbers.")))
    (let ((parsed-option (linconf-parse-kconfig-option test-lines)))
      (if parsed-option
          (progn
            (message "✓ Direct parsing successful!")
            (message "  Name: %s" (car parsed-option))
            (message "  Type: %s" (plist-get (cdr parsed-option) :type))
            (message "  Full plist: %S" (cdr parsed-option)))
        (message "✗ Direct parsing failed!"))))

  (message "\n=== Debug Complete ==="))