#!/usr/bin/emacs --script
;;; debug-krb5-line.el --- Check specific CRYPTO_KRB5 line

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Set kernel source path
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/os-build")

;; Load the automotive config
(find-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64-automotive.config")

;; Load Kconfig data quietly
(let ((inhibit-message t))
  (linconf-load-kconfig-data))

;; Go to line 6633 and check CRYPTO_KRB5
(goto-char (point-min))
(forward-line 6632) ;; Line 6633 (0-indexed)
(let* ((line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
       (option (linconf-get-option-name))
       (value (linconf-get-config-value option))
       (kconfig-info (gethash option linconf-kconfig-options))
       (option-type (when kconfig-info (plist-get kconfig-info :type))))
  (message "Line 6633 content: %s" line-content)
  (message "Parsed option: %s" option)
  (message "Parsed value: %s" value)
  (message "Kconfig type: %s" option-type)
  (message "Validation result: %s" (linconf-validate-option-value option "m")))