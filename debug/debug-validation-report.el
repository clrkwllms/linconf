#!/usr/bin/emacs --script
;;; debug-validation-report.el --- Debug validation report config file display

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test validation report with a real config file
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    (message "Testing validation report with: %s" test-config)

    ;; Open the config file and activate linconf-mode
    (with-temp-buffer
      (setq buffer-file-name test-config)
      (insert-file-contents test-config)
      (linconf-mode)

      (message "Buffer file name: %s" buffer-file-name)
      (message "Detected architecture: %s" linconf-detected-architecture)

      ;; Test just the first few lines to avoid long output
      (goto-char (point-min))
      (forward-line 100)  ; Only process first 100 lines for testing
      (delete-region (point) (point-max))

      ;; Run validation
      (linconf-validate-all-options)

      ;; Show what was written to the validation buffer
      (with-current-buffer "*LinConf Validation*"
        (message "Validation buffer contents:")
        (message "%s" (buffer-string))))))