#!/usr/bin/emacs --script
;;; debug-riscv-mode.el --- Test mode activation with RISC-V config

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test RISC-V config file
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-riscv64.config"))
  (when (file-exists-p test-config)
    (message "Testing RISC-V config: %s" (file-name-nondirectory test-config))

    ;; Clear state
    (setq linconf-detected-architecture nil)

    (with-temp-buffer
      (setq buffer-file-name test-config)
      (insert-file-contents test-config)

      ;; Activate linconf-mode
      (linconf-mode)

      (message "Detected architecture: %s" linconf-detected-architecture)
      (message "Mode line display: '%s'" (linconf-mode-line-architecture))
      (message "RISCV variable: %s" (gethash "RISCV" linconf-config-values))
      (message "64BIT variable: %s" (gethash "64BIT" linconf-config-values)))))