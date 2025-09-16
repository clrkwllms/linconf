#!/usr/bin/env emacs --script
;; Debug choice parsing to understand why HZ options are not being extracted

(load-file "linconf.el")

;; Direct test of parsing kernel/Kconfig.hz with detailed tracing
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")

(message "=== Choice Parsing Debug ===")

;; Let's manually trace through the parsing logic
(let ((hz-file "/nas/src/RedHat/gitlab/kernel-ark/linus/kernel/Kconfig.hz"))
  (message "Parsing file: %s" hz-file)

  ;; Read the file contents and see what lines exist
  (with-temp-buffer
    (insert-file-contents hz-file)
    (let ((lines (split-string (buffer-string) "\n"))
          (line-num 0))
      (message "\n=== File Contents ===")
      (dolist (line lines)
        (setq line-num (1+ line-num))
        (let ((trimmed-line (string-trim line)))
          (when (and (> (length trimmed-line) 0)
                     (not (string-match "^[ \t]*#" line)))
            (message "%d: %s" line-num line)
            ;; Check what patterns match
            (cond
             ((string-match "^choice" line)
              (message "    → CHOICE START"))
             ((string-match "^endchoice" line)
              (message "    → CHOICE END"))
             ((string-match "^config[ \t]+\\([A-Z0-9_]+\\)" line)
              (message "    → CONFIG: %s" (match-string 1 line)))
             ((string-match "bool" line)
              (message "    → TYPE: bool"))
             ((string-match "int" line)
              (message "    → TYPE: int"))))))))))

;; Now test the actual parsing function with our fix
(message "\n=== Actual Parsing Results ===")
(let ((options (linconf-parse-kconfig-file "/nas/src/RedHat/gitlab/kernel-ark/linus/kernel/Kconfig.hz")))
  (message "Total options parsed: %d" (length options))
  (dolist (option options)
    (let ((name (car option))
          (props (cdr option)))
      (message "%s: type=%s" name (plist-get props :type)))))

(message "\n=== DONE ===")