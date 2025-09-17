#!/usr/bin/env emacs --script
;; Quick test for SNET_VDPA recognition

(load-file "linconf.el")

(message "=== Quick SNET_VDPA Test ===")

;; Set kernel source path and load data
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")
(linconf-load-kconfig-data)

;; Check if SNET_VDPA is in the hash table
(let ((option (gethash "SNET_VDPA" linconf-kconfig-options)))
  (if option
      (progn
        (message "✓ SNET_VDPA found!")
        (message "  Type: %s" (plist-get option :type)))
    (message "✗ SNET_VDPA NOT found")))

;; Test validation
(let ((validation-result (linconf-validate-option-value "SNET_VDPA" "m")))
  (if (car validation-result)
      (message "✓ SNET_VDPA=m validates successfully")
    (message "✗ SNET_VDPA=m validation failed: %s" (cdr validation-result))))

(message "=== DONE ===")