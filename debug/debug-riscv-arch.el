#!/usr/bin/emacs --script
;;; debug-riscv-arch.el --- Test RISC-V architecture detection

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test RISC-V config
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-riscv64.config"))
  (when (file-exists-p test-config)
    ;; Clear previous state
    (setq linconf-detected-architecture nil)
    (clrhash linconf-config-values)

    ;; Parse config file
    (message "Parsing RISC-V config file: %s" (file-name-nondirectory test-config))
    (let ((config-hash (linconf-parse-config-file test-config)))
      (message "Detected architecture: %s" linconf-detected-architecture)
      (message "Architecture variables set:")
      (message "  RISCV: %s" (gethash "RISCV" linconf-config-values))
      (message "  64BIT: %s" (gethash "64BIT" linconf-config-values))
      (message "  X86: %s" (gethash "X86" linconf-config-values))
      (message "Config options parsed: %d" (hash-table-count config-hash)))))