#!/usr/bin/emacs --script

;; Minimal test of BPF_JIT parsing

(defun test-parse-bpf-jit ()
  (let ((lines '("config BPF_JIT"
                 "\tbool \"Enable BPF Just In Time compiler\""
                 "\tdepends on BPF")))
    (let ((name nil)
          (type nil))
      (dolist (line lines)
        (message "Processing: %s" line)
        (cond
         ;; Handle config declaration
         ((string-match "^config \\([A-Z0-9_]+\\)" line)
          (setq name (match-string 1 line))
          (message "Found config: %s" name))
         ;; Handle type declarations
         ((string-match "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)" line)
          (setq type (intern (match-string 1 line)))
          (message "Found type: %s" type))))
      (message "Final result: name=%s, type=%s" name type)
      (when name
        (cons name (list :type type))))))

(test-parse-bpf-jit)