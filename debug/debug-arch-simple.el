#!/usr/bin/emacs --script
;;; debug-arch-simple.el --- Simple architecture detection test

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test architecture detection function directly
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    (let ((detected-arch (linconf-detect-architecture-from-config test-config)))
      (message "Config: %s" (file-name-nondirectory test-config))
      (message "Detected architecture: %s" (or detected-arch "none")))))