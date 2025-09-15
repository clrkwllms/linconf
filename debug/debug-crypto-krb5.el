#!/usr/bin/emacs --script
;;; debug-crypto-krb5.el --- Check CRYPTO_KRB5 tristate parsing

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Set kernel source path
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/os-build")

;; Load the automotive config
(find-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64-automotive.config")

;; Load Kconfig data quietly
(let ((inhibit-message t))
  (linconf-load-kconfig-data))

;; Check what type CRYPTO_KRB5 has in our hash table
(let ((crypto-krb5-info (gethash "CRYPTO_KRB5" linconf-kconfig-options)))
  (if crypto-krb5-info
      (progn
        (message "CRYPTO_KRB5 found in Kconfig data:")
        (message "  Type: %s" (plist-get crypto-krb5-info :type))
        (message "  Help: %s" (substring (or (plist-get crypto-krb5-info :help) "No help") 0 50))
        (message "  Depends: %s" (plist-get crypto-krb5-info :depends))
        (message "  Select: %s" (plist-get crypto-krb5-info :select)))
    (message "CRYPTO_KRB5 NOT found in Kconfig data!")))

;; Also check if validation recognizes it correctly
(let ((validation-result (linconf-validate-option-value "CRYPTO_KRB5" "m")))
  (message "Validation of CRYPTO_KRB5=m: %s" validation-result))

;; Check what happens with validation in the automotive config
(save-excursion
  (goto-char (point-min))
  (if (search-forward "CONFIG_CRYPTO_KRB5" nil t)
      (progn
        (message "Found CONFIG_CRYPTO_KRB5 at line %d" (line-number-at-pos))
        (beginning-of-line)
        (let* ((line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (option (linconf-get-option-name))
               (value (linconf-get-config-value option))
               (kconfig-info (gethash option linconf-kconfig-options))
               (option-type (when kconfig-info (plist-get kconfig-info :type))))
          (message "Line content: %s" line-content)
          (message "Current line option: %s" option)
          (message "Current line value: %s" value)
          (message "Option type from Kconfig: %s" option-type)))
    (message "CONFIG_CRYPTO_KRB5 not found in automotive config")))