#!/usr/bin/emacs --script
;;; debug-tmpfs-inode64.el --- Debug TMPFS_INODE64 validation issue

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Set kernel source path
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/os-build")

;; Load the x86_64 config
(find-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config")

;; Load Kconfig data quietly
(let ((inhibit-message t))
  (linconf-load-kconfig-data))

;; Check what type TMPFS_INODE64 has in our hash table
(let ((tmpfs-info (gethash "TMPFS_INODE64" linconf-kconfig-options)))
  (if tmpfs-info
      (progn
        (message "TMPFS_INODE64 found in Kconfig data:")
        (message "  Type: %s" (plist-get tmpfs-info :type))
        (message "  Help: %s" (substring (or (plist-get tmpfs-info :help) "No help") 0 50))
        (message "  Depends: %s" (plist-get tmpfs-info :depends))
        (message "  Default: %s" (plist-get tmpfs-info :default)))
    (message "TMPFS_INODE64 NOT found in Kconfig data!")))

;; Test validation
(let ((validation-result (linconf-validate-option-value "TMPFS_INODE64" "y")))
  (message "Validation of TMPFS_INODE64=y: %s" validation-result))

;; Check the actual line in the config
(save-excursion
  (goto-char (point-min))
  (when (search-forward "CONFIG_TMPFS_INODE64=y" nil t)
    (message "Found CONFIG_TMPFS_INODE64=y at line %d" (line-number-at-pos))
    (beginning-of-line)
    (let* ((option (linconf-get-option-name))
           (value (linconf-get-config-value option))
           (kconfig-info (gethash option linconf-kconfig-options))
           (option-type (when kconfig-info (plist-get kconfig-info :type))))
      (message "Current line option: %s" option)
      (message "Current line value: %s" value)
      (message "Option type from Kconfig: %s" option-type))))