#!/usr/bin/env emacs --script
;; Detailed debugging of choice parsing logic step-by-step

(load-file "linconf.el")

(message "=== Detailed Choice Parsing Debug ===")

;; Create a temporary version of the parsing function with debug output
(defun debug-parse-kconfig-file (file)
  "Debug version of linconf-parse-kconfig-file with detailed tracing."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (linconf-preprocess-continuations (buffer-string)))
            (options '())
            (current-config '())
            (current-choice '())
            (choice-options '())
            (in-config nil)
            (in-choice nil)
            (in-menu nil)
            (menu-stack '())
            (config-type 'config)
            (line-num 0))

        (dolist (line lines)
          (setq line-num (1+ line-num))
          (let ((trimmed-line (string-trim line)))
            (message "[%d] Processing: %s" line-num trimmed-line)
            (message "    State: in-choice=%s in-config=%s choice-opts=%s current-config-len=%d"
                     in-choice in-config choice-options (length current-config))

            (cond
             ;; Handle choice start
             ((string-match "^choice" line)
              (message "    → CHOICE START detected")
              (when current-config
                (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                  (when option
                    (message "    → Finishing previous config: %s" (car option))
                    (push option options))))
              (setq current-choice (list line)
                    choice-options '()
                    in-choice t
                    current-config nil
                    in-config nil))

             ;; Handle choice end
             ((string-match "^endchoice" line)
              (message "    → CHOICE END detected")
              (when current-choice
                (when current-config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                    (when option
                      (message "    → Processing final choice config: %s" (car option))
                      (push option options))))
                (let* ((choice-name (format "CHOICE_%d" (random 10000)))
                       (choice-option (cons choice-name
                                           (list :type 'choice
                                                 :choices choice-options
                                                 :help "Choice group"))))
                  (message "    → Creating choice group: %s with options %s" choice-name choice-options)
                  (push choice-option options)))
              (setq in-choice nil
                    current-choice nil
                    choice-options '()
                    current-config nil
                    in-config nil))

             ;; Handle config entries
             ((string-match "^[ \\t]*config[ \\t]+\\([A-Z0-9_]+\\)" line)
              (let ((config-name (match-string 1 line)))
                (message "    → CONFIG detected: %s" config-name)
                (when current-config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                    (when option
                      (message "    → Finishing previous config: %s" (car option))
                      (push option options))))
                (setq current-config (list line)
                      in-config t
                      config-type 'config)
                (when in-choice
                  (message "    → Adding to choice options: %s" config-name)
                  (push config-name choice-options))))

             ;; Handle other config lines
             ((and in-config (not (string-match "^[ \\t]*$" line)))
              (message "    → Adding line to current config")
              (push line current-config)))))

        ;; Handle any remaining config at end
        (when current-config
          (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
            (when option
              (message "Final config: %s" (car option))
              (push option options))))

        (nreverse options)))))

;; Test with the debug version
(let ((options (debug-parse-kconfig-file "/nas/src/RedHat/gitlab/kernel-ark/linus/kernel/Kconfig.hz")))
  (message "\n=== FINAL RESULTS ===")
  (message "Total options: %d" (length options))
  (dolist (option options)
    (message "  %s: type=%s" (car option) (plist-get (cdr option) :type))))

(message "\n=== DONE ===")