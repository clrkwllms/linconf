#!/usr/bin/emacs --script

(load-file "linconf.el")

;; Clear the hash table and test just one option
(clrhash linconf-kconfig-options)

;; Manually parse just BPF_JIT
(let ((bpf-jit-lines '("config BPF_JIT"
                       "\tbool \"Enable BPF Just In Time compiler\""
                       "\tdepends on BPF")))
  (let ((result (linconf-parse-kconfig-option bpf-jit-lines 'config)))
    (message "Parse result: %s" result)
    (when result
      (puthash (car result) (cdr result) linconf-kconfig-options))))

;; Now test validation
(let ((info (gethash "BPF_JIT" linconf-kconfig-options)))
  (if info
      (message "Stored BPF_JIT: type=%s" (plist-get info :type))
    (message "BPF_JIT not found")))

;; Test validation
(let ((result (linconf-validate-option-value "BPF_JIT" "y")))
  (message "Validation result: valid=%s, error=%s" (car result) (cdr result)))