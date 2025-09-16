#!/usr/bin/env emacs --script
;; Debug Kconfig parsing to see why HZ options are missing

(load-file "linconf.el")

(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")

;; Test whether HZ options are parsed when we load Kconfig
(message "=== Testing Kconfig Parsing ===")

;; Initialize the kconfig options hash table
(unless linconf-kconfig-options
  (setq linconf-kconfig-options (make-hash-table :test 'equal :size 10000)))

;; Load all Kconfig options
(message "Loading Kconfig definitions...")
(linconf-load-kconfig-data)

;; Check if HZ options are loaded
(let ((hz-options '("HZ_100" "HZ_250" "HZ_300" "HZ_1000")))
  (message "\n=== HZ Options Check ===")
  (dolist (option hz-options)
    (let ((definition (gethash option linconf-kconfig-options)))
      (if definition
          (message "✓ %s found: %s" option (plist-get definition :type))
        (message "✗ %s NOT found" option))))

  ;; Check some other missing options from the report
  (message "\n=== Other Missing Options Check ===")
  (let ((other-options '("CRYPTO_JITTERENTROPY_MEMSIZE_1024" "RANDSTRUCT_FULL" "DEFAULT_SECURITY_SELINUX")))
    (dolist (option other-options)
      (let ((definition (gethash option linconf-kconfig-options)))
        (if definition
            (message "✓ %s found: %s" option (plist-get definition :type))
          (message "✗ %s NOT found" option)))))

  ;; Show total number of loaded options
  (message "\n=== Summary ===")
  (message "Total Kconfig options loaded: %d" (hash-table-count linconf-kconfig-options))

  ;; Show first 10 options for verification
  (message "\n=== Sample Options ===")
  (let ((count 0))
    (maphash (lambda (key value)
               (when (< count 10)
                 (message "%s: %s" key (plist-get value :type))
                 (setq count (1+ count))))
             linconf-kconfig-options)))

(message "\n=== DONE ===")