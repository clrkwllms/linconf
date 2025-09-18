;;; debug-source-tracking-demo.el --- Demonstration of source file tracking -*- lexical-binding: t; -*-

;; This file demonstrates the complete Kconfig source file tracking functionality

(require 'linconf)

(message "=== LinConf Source File Tracking Feature Demo ===")

;; Create a sample Kconfig file
(let ((demo-file "/tmp/demo-kconfig"))
  (with-temp-file demo-file
    (insert "# Sample Kconfig file for demonstration\n")
    (insert "\n")
    (insert "config NET_BPF_JIT\n")
    (insert "\tbool \"Enable BPF JIT compiler\"\n")
    (insert "\tdepends on NET\n")
    (insert "\thelp\n")
    (insert "\t  Berkeley Packet Filter filtering capabilities are normally\n")
    (insert "\t  handled by an interpreter. This option allows kernel to\n")
    (insert "\t  generate native code when filter is loaded in memory.\n")
    (insert "\n")
    (insert "config BPF_SYSCALL\n")
    (insert "\tbool \"Enable bpf() system call\"\n")
    (insert "\tselect IRQ_WORK\n")
    (insert "\tdefault n\n")
    (insert "\thelp\n")
    (insert "\t  Enable the bpf() system call that allows to manipulate eBPF\n")
    (insert "\t  programs and maps via file descriptors.\n"))

  ;; Set up kernel source path for relative path calculation
  (setq linconf-kernel-source-path "/kernel/source")

  (message "\n1. Parsing Kconfig file with source tracking...")
  (let ((options (linconf-parse-kconfig-file demo-file)))
    (message "   Found %d options" (length options))

    ;; Load options into the global hash table
    (dolist (option options)
      (puthash (car option) (cdr option) linconf-kconfig-options))

    ;; Demonstrate the features
    (message "\n2. Demonstrating source file information:")
    (dolist (option options)
      (let* ((name (car option))
             (plist (cdr option))
             (source-file (plist-get plist :source-file))
             (relative-path (linconf-relative-source-path source-file))
             (option-type (plist-get plist :type)))
        (message "   • Option: %s (type: %s)" name option-type)
        (message "     Source file: %s" source-file)
        (message "     Relative path: %s" relative-path)))

    ;; Simulate the help text display
    (message "\n3. Simulating help text display:")
    (when options
      (let* ((first-option (car options))
             (name (car first-option))
             (plist (cdr first-option))
             (help-text (plist-get plist :help))
             (option-type (plist-get plist :type))
             (source-file (plist-get plist :source-file))
             (relative-path (linconf-relative-source-path source-file)))
        (message "\n   Help text for CONFIG_%s would show:" name)
        (message "   ==========================================")
        (message "   CONFIG_%s" name)
        (message "   Type: %s" option-type)
        (message "   Source: %s" relative-path)
        (message "   ")
        (message "   %s" (or help-text "No help text available."))
        (message "   =========================================="))))

  ;; Cleanup
  (delete-file demo-file))

(message "\n✓ Source file tracking demonstration completed!")
(message "\nKey Features Implemented:")
(message "• Source file paths are tracked for each parsed option")
(message "• Relative paths are displayed (relative to kernel source root)")
(message "• Help text shows source file information")
(message "• Backward compatibility maintained with existing code")
(message "• Functional programming principles followed")

;;; debug-source-tracking-demo.el ends here