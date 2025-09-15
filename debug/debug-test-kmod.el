#!/usr/bin/emacs --script
;;; debug-test-kmod.el --- Debug TEST_KMOD tristate parsing issue

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Set kernel source path to where TEST_KMOD is defined
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/os-build")

;; Load the automotive config
(find-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64-automotive.config")

;; Load Kconfig data quietly
(let ((inhibit-message t))
  (linconf-load-kconfig-data))

;; Check what type TEST_KMOD has in our hash table
(let ((test-kmod-info (gethash "TEST_KMOD" linconf-kconfig-options)))
  (if test-kmod-info
      (progn
        (message "TEST_KMOD found in Kconfig data:")
        (message "  Type: %s" (plist-get test-kmod-info :type))
        (message "  Help: %s" (substring (or (plist-get test-kmod-info :help) "No help") 0 50))
        (message "  Depends: %s" (plist-get test-kmod-info :depends))
        (message "  Select: %s" (plist-get test-kmod-info :select)))
    (message "TEST_KMOD NOT found in Kconfig data!")))

;; Also check if validation recognizes it correctly
(let ((validation-result (linconf-validate-option-value "TEST_KMOD" "m")))
  (message "Validation of TEST_KMOD=m: %s" validation-result))

;; Check what happens with validation of the automotive config line
(save-excursion
  (goto-char (point-min))
  (when (search-forward "CONFIG_TEST_KMOD=m" nil t)
    (message "Found CONFIG_TEST_KMOD=m at line %d" (line-number-at-pos))
    (beginning-of-line)
    (let* ((option (linconf-get-option-name))
           (value (linconf-get-config-value option))
           (kconfig-info (gethash option linconf-kconfig-options))
           (option-type (when kconfig-info (plist-get kconfig-info :type))))
      (message "Current line option: %s" option)
      (message "Current line value: %s" value)
      (message "Option type from Kconfig: %s" option-type))))