#!/usr/bin/env emacs --script
;; Debug the conditional parsing flow step by step

(load-file "linconf.el")

(defun debug-conditional-parsing (file)
  "Debug version with detailed conditional state tracking."
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
            (in-if-block nil)
            (if-conditions '())
            (line-num 0))

        (dolist (line lines)
          (setq line-num (1+ line-num))
          (let ((trimmed-line (string-trim line)))

            ;; Debug key lines
            (when (or (string-match "^if\\s-" line)
                      (string-match "^endif" line)
                      (string-match "^[ \t]*config[ \t]+" line)
                      (string-match "^[ \t]*menuconfig[ \t]+" line))
              (message "[%d] %s" line-num trimmed-line)
              (message "    State: if-block=%s conditions=%s config=%s options-count=%d"
                       in-if-block if-conditions (when current-config (length current-config)) (length options)))

            (cond
             ;; Handle if blocks
             ((string-match "^if\\s-+\\(.+\\)" line)
              (let ((condition (match-string 1 line)))
                (message "    → IF: %s" condition)
                (push condition if-conditions)
                (setq in-if-block t)))

             ;; Handle endif blocks
             ((string-match "^endif" line)
              (message "    → ENDIF")
              (when if-conditions
                (pop if-conditions))
              (setq in-if-block (> (length if-conditions) 0)))

             ;; Handle config entries (the critical part!)
             ((string-match "^[ \t]*config[ \t]+\\([A-Z0-9_]+\\)" line)
              (let ((config-name (match-string 1 line)))
                (message "    → CONFIG: %s (conditional: %s)" config-name in-if-block)

                ;; Process previous config if exists
                (when current-config
                  (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
                    (when option
                      (message "      ✓ Added previous option: %s" (car option))
                      (push option options))))

                ;; Start new config
                (setq current-config (list line)
                      in-config t
                      config-type 'config)
                (when in-choice
                  (push config-name choice-options))))

             ;; Add non-empty lines to current config
             ((and in-config (not (string-match "^[ \t]*$" line))
                   (not (string-match "^\\(config\\|menuconfig\\|choice\\|endchoice\\|menu\\|endmenu\\|source\\)" line)))
              (push line current-config)))))

        ;; Process final config
        (when current-config
          (let ((option (linconf-parse-kconfig-option (nreverse current-config) config-type)))
            (when option
              (message "Final option: %s" (car option))
              (push option options))))

        (message "\n=== FINAL RESULTS ===")
        (message "Total options parsed: %d" (length options))
        (dolist (option options)
          (message "  %s: %s" (car option) (plist-get (cdr option) :type)))

        options))))

;; Test with IP_VS file
(debug-conditional-parsing "/nas/src/RedHat/gitlab/kernel-ark/linus/net/netfilter/ipvs/Kconfig")