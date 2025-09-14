(defun debug-linconf-parse-kconfig-file (file)
  "Debug version of linconf-parse-kconfig-file with extensive logging."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (split-string (buffer-string) "\\n"))
            (options '())
            (current-config '())
            (in-config nil))
        (message "Total lines: %d" (length lines))
        (dolist (line lines)
          (message "Processing line: %S, in-config: %s" line in-config)
          (cond
           ((string-match "^config " line)
            (message "Found config line: %s" line)
            (when current-config
              (message "Processing previous config: %S" current-config)
              (let ((option (linconf-parse-kconfig-option (nreverse current-config))))
                (message "Previous option result: %S" option)
                (when option
                  (push option options))))
            (setq current-config (list line)
                  in-config t)
            (message "Started new config, current-config: %S" current-config))
           ((and in-config (string-match "^[ \t]+" line))
            (message "Adding indented line to config: %s" line)
            (push line current-config)
            (message "current-config now: %S" current-config))
           ((and in-config (not (string-match "^[ \t]*$" line)))
            (message "Non-indented line, ending config: %s" line)
            (when current-config
              (let ((option (linconf-parse-kconfig-option (nreverse current-config))))
                (message "Ending config option result: %S" option)
                (when option
                  (push option options))))
            (setq current-config nil
                  in-config nil))))
        (message "Final current-config: %S" current-config)
        (when current-config
          (message "Processing final config block")
          (let ((option (linconf-parse-kconfig-option (nreverse current-config))))
            (message "Final option result: %S" option)
            (when option
              (push option options))))
        (message "Final options: %S" options)
        (nreverse options)))))

(load "/nas/src/linconf-emacs/linconf.el")

(let ((temp-file (make-temp-file "debug-kconfig")))
  (unwind-protect
      (progn
        (with-temp-file temp-file
          (insert "config TEST_OPTION\n"
                  "\tbool \"Test option\"\n"
                  "\tdefault y\n"))
        (debug-linconf-parse-kconfig-file temp-file))
    (delete-file temp-file)))