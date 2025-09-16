#!/usr/bin/env emacs --script
;; Debug if/endif parsing step by step

(load-file "linconf.el")

;; Create a debug version of parse function with tracing
(defun debug-parse-if-blocks (file)
  "Debug version with if/endif tracing."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (linconf-preprocess-continuations (buffer-string)))
            (options '())
            (current-config '())
            (in-if-block nil)
            (if-conditions '())
            (line-num 0))

        (dolist (line lines)
          (setq line-num (1+ line-num))
          (let ((trimmed-line (string-trim line)))
            (when (or (string-match "^\\s-*if\\s-" line)
                      (string-match "^\\s-*endif" line)
                      (string-match "^\\s-*config\\s-" line))
              (message "[%d] %s (if-block: %s, conditions: %s)"
                       line-num trimmed-line in-if-block if-conditions))

            (cond
             ;; Handle if blocks
             ((string-match "^if\\s-+\\(.+\\)" line)
              (let ((condition (match-string 1 line)))
                (message "    → IF detected: %s" condition)
                (push condition if-conditions)
                (setq in-if-block t)))

             ;; Handle endif blocks
             ((string-match "^endif" line)
              (message "    → ENDIF detected")
              (when if-conditions
                (pop if-conditions))
              (setq in-if-block (> (length if-conditions) 0)))

             ;; Handle config entries
             ((string-match "^\\s-*config[ \\t]+\\([A-Z0-9_]+\\)" line)
              (let ((config-name (match-string 1 line)))
                (message "    → CONFIG: %s (in-if: %s)" config-name in-if-block)
                (push (cons config-name (list :type 'config :in-if in-if-block)) options))))))

        (message "\n=== Final Summary ===")
        (message "Total options: %d" (length options))
        (dolist (option options)
          (message "  %s: in-if=%s" (car option) (plist-get (cdr option) :in-if)))

        options))))

;; Test with IP_VS file
(debug-parse-if-blocks "/nas/src/RedHat/gitlab/kernel-ark/linus/net/netfilter/ipvs/Kconfig")