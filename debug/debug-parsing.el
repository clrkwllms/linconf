(load "/nas/src/linconf-emacs/linconf.el")

(defun debug-parse-kconfig-file (file)
  "Debug version of linconf-parse-kconfig-file with detailed logging."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (split-string (buffer-string) "\n"))
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
            (message "Line %d: '%s' (in-choice: %s)" line-num line in-choice)
            (cond
             ;; Handle config entries
             ((string-match "^config[ \t]+\\([A-Z0-9_]+\\)" line)
              (message "  -> Found config: %s" (match-string 1 line))
              (when in-choice
                (message "  -> Adding to choice options: %s" (match-string 1 line))
                (push (match-string 1 line) choice-options)
                (message "  -> Choice options now: %s" choice-options)))
             
             ;; Handle choice start
             ((string-match "^choice" line)
              (message "  -> Starting choice block")
              (setq current-choice (list line)
                    choice-options '()
                    in-choice t))
             
             ;; Handle choice end
             ((string-match "^endchoice" line)
              (message "  -> Ending choice block with options: %s" choice-options))))
          )
        
        (message "Final choice options: %s" choice-options)
        options))))

(let ((temp-file (make-temp-file "debug-choice")))
  (unwind-protect
      (progn
        (with-temp-file temp-file
          (insert "choice\n"
                  "\tprompt \"Choose scheduler\"\n"
                  "\n"
                  "config SCHED_CFS\n"
                  "\tbool \"CFS\"\n"
                  "\n"
                  "config SCHED_RT\n"
                  "\tbool \"RT\"\n"
                  "\n"
                  "endchoice\n"))
        (debug-parse-kconfig-file temp-file))
    (delete-file temp-file)))