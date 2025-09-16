#!/usr/bin/emacs --script
;;; debug-architecture-detection.el --- Test architecture detection from config files

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test architecture detection function directly
(let ((test-configs '("/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"
                      "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-riscv64.config"
                      "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-s390x-debug.config")))
  (message "Testing architecture detection from config files...")
  (dolist (config test-configs)
    (when (file-exists-p config)
      (let ((detected-arch (linconf-detect-architecture-from-config config)))
        (message "Config: %s" (file-name-nondirectory config))
        (message "  Detected architecture: %s" (or detected-arch "none"))
        (message ""))))

;; Test the full config parsing with architecture detection
(message "Testing full config parsing with architecture detection...")
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    ;; Clear previous state
    (setq linconf-detected-architecture nil)
    (clrhash linconf-config-values)

    ;; Parse config file (this should detect architecture)
    (let ((config-hash (linconf-parse-config-file test-config)))
      (message "Parsed config file: %s" (file-name-nondirectory test-config))
      (message "Detected architecture: %s" linconf-detected-architecture)
      (message "Architecture variables set:")
      (message "  X86: %s" (gethash "X86" linconf-config-values))
      (message "  X86_64: %s" (gethash "X86_64" linconf-config-values))
      (message "  64BIT: %s" (gethash "64BIT" linconf-config-values))
      (message "Config options parsed: %d" (hash-table-count config-hash)))))

;; Test RISC-V config
(message "\nTesting RISC-V config...")
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-riscv64.config"))
  (when (file-exists-p test-config)
    ;; Clear previous state
    (setq linconf-detected-architecture nil)
    (clrhash linconf-config-values)

    ;; Parse config file
    (let ((config-hash (linconf-parse-config-file test-config)))
      (message "Parsed config file: %s" (file-name-nondirectory test-config))
      (message "Detected architecture: %s" linconf-detected-architecture)
      (message "Architecture variables set:")
      (message "  RISCV: %s" (gethash "RISCV" linconf-config-values))
      (message "  64BIT: %s" (gethash "64BIT" linconf-config-values))
      (message "Config options parsed: %d" (hash-table-count config-hash)))))