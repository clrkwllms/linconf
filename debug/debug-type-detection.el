#!/usr/bin/emacs --script

(load-file "linconf.el")

(setq linconf-kernel-source-path "/usr/src/kernels/6.15.10-200.fc42.x86_64/")

;; Silently load Kconfig data
(let ((inhibit-message t))
  (linconf-load-kconfig-data))

;; Check a few problem options
(let ((problem-options '("BPF_JIT" "TEST_KMOD" "HASH_KUNIT_TEST" "NF_CONNTRACK" "HID")))
  (dolist (option problem-options)
    (let ((info (gethash option linconf-kconfig-options)))
      (if info
          (message "%s: type=%s" option (plist-get info :type))
        (message "%s: NOT FOUND" option)))))

;; Test validation of BPF_JIT=y specifically
(let ((result (linconf-validate-option-value "BPF_JIT" "y")))
  (message "BPF_JIT=y validation: valid=%s, error=%s" (car result) (cdr result)))