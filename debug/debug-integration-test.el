;;; debug-integration-test.el --- Integration test for source file tracking -*- lexical-binding: t; -*-

(require 'linconf)

(message "=== Source File Tracking Integration Test ===")

;; Test with a temporary file
(let ((test-file "/tmp/test-integration-kconfig"))
  (with-temp-file test-file
    (insert "config BPF_SYSCALL\n")
    (insert "\tbool \"Enable bpf() system call\"\n")
    (insert "\tselect IRQ_WORK\n")
    (insert "\tselect TASKS_TRACE_RCU\n")
    (insert "\thelp\n")
    (insert "\t  Enable the bpf() system call that allows to manipulate eBPF\n")
    (insert "\t  programs and maps via file descriptors.\n"))

  ;; Set up kernel source path
  (setq linconf-kernel-source-path "/kernel/source")

  ;; Parse the file
  (let ((options (linconf-parse-kconfig-file test-file)))
    (if options
        (let* ((option (car options))
               (name (car option))
               (plist (cdr option))
               (source-file (plist-get plist :source-file))
               (relative-path (linconf-relative-source-path source-file)))
          (message "✓ Parsed option: %s" name)
          (message "✓ Source file: %s" source-file)
          (message "✓ Relative path: %s" relative-path)

          ;; Simulate the help text functionality
          (puthash name plist linconf-kconfig-options)
          (message "✓ Option stored in hash table")

          ;; Test what the help would show
          (message "✓ Help would display: Source: %s" relative-path))
      (message "✗ Failed to parse options")))

  ;; Cleanup
  (delete-file test-file))

(message "✓ Integration test completed")

;;; debug-integration-test.el ends here