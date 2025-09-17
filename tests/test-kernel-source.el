;;; test-kernel-source.el --- Test validation with kernel source -*- lexical-binding: t; -*-

(require 'linconf)

(defun test-kernel-source-validation ()
  "Test validation with kernel source path set."
  (interactive)
  (setq linconf-kernel-source-path (expand-file-name "~/src/linux.git"))
  (message "Using kernel source: %s" linconf-kernel-source-path)

  (let ((config-file "test-files/kernel-x86_64-fedora.config"))
    (when (file-exists-p config-file)
      (message "Testing %s with kernel source..." config-file)

      (with-temp-buffer
        (insert-file-contents config-file)
        (linconf-mode)
        (linconf-validate-all-options))

      (let ((results-buffer "*LinConf Validation*"))
        (when (get-buffer results-buffer)
          (with-current-buffer results-buffer
            (goto-char (point-min))
            (when (re-search-forward "^Valid options: \\([0-9]+\\)" nil t)
              (message "Valid: %s" (match-string 1)))
            (goto-char (point-min))
            (when (re-search-forward "^Errors: \\([0-9]+\\)" nil t)
              (message "Errors: %s" (match-string 1)))
            (goto-char (point-min))
            (when (re-search-forward "^Warnings: \\([0-9]+\\)" nil t)
              (message "Warnings: %s" (match-string 1)))
            (kill-buffer))))

    (message "Test complete")))

(when noninteractive
  (test-kernel-source-validation))

(provide 'test-kernel-source)
;;; test-kernel-source.el ends here