#!/usr/bin/env emacs --script
;; Test if SNET_VDPA is now recognized in full validation

(load-file "linconf.el")

(message "=== SNET_VDPA Full Validation Test ===")

;; Set kernel source path
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")

;; Test if SNET_VDPA is found in parsed options
(message "Loading kernel configuration data...")
(let ((start-time (current-time)))
  (linconf-load-kconfig-data)
  (message "Loading completed in %.2f seconds" (float-time (time-subtract (current-time) start-time))))

;; Check if SNET_VDPA is in the hash table
(let ((option (gethash "SNET_VDPA" linconf-kconfig-options)))
  (if option
      (progn
        (message "✓ SNET_VDPA found in parsed options!")
        (message "  Type: %s" (plist-get option :type))
        (message "  Help: %s" (if (plist-get option :help)
                                 (substring (plist-get option :help) 0 (min 50 (length (plist-get option :help))))
                               "No help text")))
    (message "✗ SNET_VDPA NOT found in parsed options")))

;; Test validation of a sample config line
(message "\nTesting validation of SNET_VDPA=m:")
(let ((validation-result (linconf-validate-option-value "SNET_VDPA" "m")))
  (if (car validation-result)
      (message "✓ SNET_VDPA=m validates successfully")
    (message "✗ SNET_VDPA=m validation failed: %s" (cdr validation-result))))

(message "\n=== DONE ===")