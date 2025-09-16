#!/usr/bin/emacs --script
;;; debug-mode-line-arch.el --- Test mode line architecture display

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test mode line architecture function directly
(message "Testing mode line architecture function...")

;; Test with no architecture detected
(setq linconf-detected-architecture nil)
(message "No arch detected: '%s'" (linconf-mode-line-architecture))

;; Test with x86 architecture
(setq linconf-detected-architecture "x86")
(message "x86 arch detected: '%s'" (linconf-mode-line-architecture))

;; Test with riscv architecture
(setq linconf-detected-architecture "riscv")
(message "riscv arch detected: '%s'" (linconf-mode-line-architecture))

;; Test with arm64 architecture
(setq linconf-detected-architecture "arm64")
(message "arm64 arch detected: '%s'" (linconf-mode-line-architecture))

;; Test mode line buffer identification format
(message "\nTesting mode line buffer identification...")
(setq linconf-detected-architecture "x86")
(let ((mode-line-format '((:eval (linconf-mode-line-architecture)))))
  (message "Mode line with x86: '%s'" (format-mode-line mode-line-format)))

(setq linconf-detected-architecture "riscv")
(let ((mode-line-format '((:eval (linconf-mode-line-architecture)))))
  (message "Mode line with riscv: '%s'" (format-mode-line mode-line-format)))

;; Test config file loading and architecture detection
(message "\nTesting config file loading with architecture detection...")
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    (setq linconf-detected-architecture nil)
    (linconf-parse-config-file test-config)
    (message "After parsing x86_64 config:")
    (message "  Detected architecture: %s" linconf-detected-architecture)
    (message "  Mode line display: '%s'" (linconf-mode-line-architecture))))