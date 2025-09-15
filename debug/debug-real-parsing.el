#!/usr/bin/emacs --script

(load-file "linconf.el")

;; Monkey patch the parse function to add debug for BPF_JIT
(defun debug-linconf-parse-kconfig-option (lines entry-type)
  "Debug version that prints info for BPF_JIT"
  (let ((result (linconf-parse-kconfig-option lines entry-type)))
    (when (and result (string= (car result) "BPF_JIT"))
      (message "DEBUG: BPF_JIT parsed result: %s" result))
    result))

;; Replace the function temporarily
(fset 'original-linconf-parse-kconfig-option (symbol-function 'linconf-parse-kconfig-option))
(fset 'linconf-parse-kconfig-option 'debug-linconf-parse-kconfig-option)

(setq linconf-kernel-source-path "/usr/src/kernels/6.15.10-200.fc42.x86_64/")

;; Load just a small part to see BPF_JIT
(let ((inhibit-message t))
  (linconf-parse-kconfig-file "/usr/src/kernels/6.15.10-200.fc42.x86_64/kernel/bpf/Kconfig"))

;; Check what got stored
(let ((info (gethash "BPF_JIT" linconf-kconfig-options)))
  (if info
      (message "BPF_JIT stored: type=%s, full=%s" (plist-get info :type) info)
    (message "BPF_JIT not found in hash table")))