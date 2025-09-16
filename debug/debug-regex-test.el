#!/usr/bin/env emacs --script
;; Test regex patterns directly

(let ((test-lines '("config HZ_100"
                   "\tconfig HZ_100"
                   "	config HZ_100"
                   " config HZ_100")))

  (message "=== Regex Pattern Testing ===")

  (dolist (line test-lines)
    (message "Testing line: '%s'" line)

    ;; Test original pattern
    (if (string-match "^config[ \t]+\\([A-Z0-9_]+\\)" line)
        (message "  ✓ Original pattern matches: %s" (match-string 1 line))
      (message "  ✗ Original pattern does not match"))

    ;; Test new pattern
    (if (string-match "^[ \t]*config[ \t]+\\([A-Z0-9_]+\\)" line)
        (message "  ✓ New pattern matches: %s" (match-string 1 line))
      (message "  ✗ New pattern does not match"))

    (message "")))

(message "=== DONE ===")