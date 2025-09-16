#!/usr/bin/emacs --script
;;; test-mode-line-demo.el --- Demonstrate mode line architecture display

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test opening a config file and showing mode line
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    (message "Opening config file: %s" (file-name-nondirectory test-config))

    ;; Clear state
    (setq linconf-detected-architecture nil)

    ;; Create a test buffer in linconf-mode
    (with-temp-buffer
      (linconf-mode)
      (insert-file-contents test-config)

      ;; Parse to detect architecture
      (linconf-parse-config-file test-config)

      ;; Show what the mode line would display
      (message "Mode line buffer identification:")
      (let ((buffer-id (format-mode-line mode-line-buffer-identification)))
        (message "  '%s'" buffer-id))

      (message "Architecture detected: %s" linconf-detected-architecture)
      (message "Architecture mode line part: '%s'" (linconf-mode-line-architecture)))))