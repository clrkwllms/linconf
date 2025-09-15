#!/usr/bin/emacs --script

(load-file "linconf.el")

;; Add debug to the parsing function
(defun linconf-parse-kconfig-option-debug (lines entry-type)
  "Debug version of linconf-parse-kconfig-option."
  (let ((name nil)
        (type nil)
        (debug-lines '()))
    (dolist (line lines)
      (when (string-match "BPF_JIT" line)
        (push (format "Processing line: %s" line) debug-lines))
      (cond
       ;; Handle config or menuconfig declaration
       ((string-match "^\\(menu\\)?config \\([A-Z0-9_]+\\)" line)
        (setq name (match-string 2 line))
        (when (string= name "BPF_JIT")
          (push (format "Found config %s" name) debug-lines)))
       ;; Handle type declarations
       ((string-match "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\(?: \"\\([^\"]*\\)\"\\)?" line)
        (setq type (intern (match-string 1 line)))
        (when (string= name "BPF_JIT")
          (push (format "Set type to: %s" type) debug-lines)))))
    (when (and name (string= name "BPF_JIT"))
      (message "BPF_JIT debug: %s" (mapconcat 'identity (nreverse debug-lines) "; "))
      (message "BPF_JIT final type: %s" type))
    (when name
      (cons name (list :type type)))))

;; Test with actual BPF_JIT config
(let ((test-lines '("config BPF_JIT"
                   "\tbool \"Enable BPF Just In Time compiler\""
                   "\tdepends on BPF")))
  (let ((result (linconf-parse-kconfig-option-debug test-lines 'config)))
    (message "Test result: %s" result)))

;; Also check what's in the actual kernel file
(let ((bpf-kconfig-file "/usr/src/kernels/6.15.10-200.fc42.x86_64/kernel/bpf/Kconfig"))
  (when (file-exists-p bpf-kconfig-file)
    (with-temp-buffer
      (insert-file-contents bpf-kconfig-file)
      (goto-char (point-min))
      (when (search-forward "config BPF_JIT" nil t)
        (let ((start (line-beginning-position))
              (end (progn
                     (forward-line 5) ; Get a few lines
                     (point))))
          (message "Actual BPF_JIT definition:")
          (message "%s" (buffer-substring start end)))))))