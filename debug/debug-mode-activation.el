#!/usr/bin/emacs --script
;;; debug-mode-activation.el --- Test architecture detection during mode activation

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test what happens when we activate linconf-mode on a file
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    (message "Testing mode activation with file: %s" (file-name-nondirectory test-config))

    ;; Clear state
    (setq linconf-detected-architecture nil)

    ;; Simulate find-file behavior more accurately
    (with-temp-buffer
      ;; Set buffer-file-name to simulate a real file visit
      (setq buffer-file-name test-config)
      (insert-file-contents test-config)

      (message "Before linconf-mode activation:")
      (message "  buffer-file-name: %s" buffer-file-name)
      (message "  linconf-detected-architecture: %s" linconf-detected-architecture)

      ;; Activate linconf-mode (this should now detect architecture)
      (linconf-mode)

      (message "After linconf-mode activation:")
      (message "  linconf-detected-architecture: %s" linconf-detected-architecture)
      (message "  Mode line arch display: '%s'" (linconf-mode-line-architecture))
      (message "  Mode line buffer ID: '%s'" (format-mode-line mode-line-buffer-identification)))))