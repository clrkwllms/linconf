#!/usr/bin/emacs --script
;;; debug-actual-lines.el --- Parse actual TEST_KMOD lines from file

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Get the actual lines from the file that correspond to TEST_KMOD
(with-temp-buffer
  (insert-file-contents "/nas/src/RedHat/gitlab/kernel-ark/os-build/lib/Kconfig.debug")
  (goto-char (point-min))
  (when (search-forward "config TEST_KMOD" nil t)
    (beginning-of-line)
    (let ((start (point))
          (lines '()))
      ;; Collect lines until we hit the next config
      (while (and (not (eobp))
                  (progn
                    (forward-line 1)
                    (or (eobp)
                        (not (looking-at "^config\\|^menuconfig\\|^choice\\|^endchoice\\|^menu\\|^endmenu")))))
        )
      ;; Go back to collect the lines
      (goto-char start)
      (while (and (not (eobp))
                  (progn
                    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                      (push line lines)
                      (forward-line 1)
                      (or (eobp)
                          (not (looking-at "^config\\|^menuconfig\\|^choice\\|^endchoice\\|^menu\\|^endmenu"))))))
        )

      (setq lines (nreverse lines))
      (message "Actual lines for TEST_KMOD:")
      (dolist (line lines)
        (message "  Line: %s" line))

      ;; Parse these exact lines
      (message "\nParsing actual lines...")
      (let ((result (linconf-parse-kconfig-option lines 'config)))
        (message "Result: %s" result)
        (when result
          (message "Name: %s" (car result))
          (message "Type: %s" (plist-get (cdr result) :type)))))))