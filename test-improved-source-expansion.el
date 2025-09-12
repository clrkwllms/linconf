(load "/nas/src/linconf-emacs/linconf.el")

(defun test-improved-source-expansion ()
  "Test the improved source pattern expansion with various patterns."
  (let ((temp-dir (make-temp-file "improved-kconfig-test" t)))
    (unwind-protect
        (progn
          ;; Create complex test directory structure
          (make-directory (expand-file-name "arch/x86" temp-dir) t)
          (make-directory (expand-file-name "arch/arm64" temp-dir) t)
          (make-directory (expand-file-name "drivers/block" temp-dir) t)
          (make-directory (expand-file-name "drivers/net/ethernet" temp-dir) t)
          (make-directory (expand-file-name "drivers/gpu" temp-dir) t)
          
          ;; Create test Kconfig files
          (with-temp-file (expand-file-name "Kconfig" temp-dir)
            (insert "# Main Kconfig with various source patterns\n"
                    "source \"arch/$(SRCARCH)/Kconfig\"\n"
                    "source \"arch/$(HEADER_ARCH)/Kconfig.debug\"\n"
                    "source \"drivers/*/Kconfig\"\n"
                    "source \"drivers/**/Kconfig\"\n"
                    "if X86\n"
                    "source \"arch/x86/Kconfig.cpu\"\n"
                    "endif\n"))
          
          ;; Create arch files
          (with-temp-file (expand-file-name "arch/x86/Kconfig" temp-dir)
            (insert "# x86 arch Kconfig\n"
                    "config X86_FEATURE\n"
                    "\tbool \"X86 feature\"\n"))
          
          (with-temp-file (expand-file-name "arch/x86/Kconfig.debug" temp-dir)
            (insert "# x86 debug Kconfig\n"
                    "config X86_DEBUG\n"
                    "\tbool \"X86 debug\"\n"))
          
          (with-temp-file (expand-file-name "arch/x86/Kconfig.cpu" temp-dir)
            (insert "# x86 CPU Kconfig\n"
                    "config X86_CPU_FEATURE\n"
                    "\tbool \"CPU feature\"\n"))
          
          ;; Create driver files  
          (with-temp-file (expand-file-name "drivers/block/Kconfig" temp-dir)
            (insert "# Block drivers\n"
                    "config BLOCK_DEV\n"
                    "\tbool \"Block device\"\n"))
          
          (with-temp-file (expand-file-name "drivers/net/Kconfig" temp-dir)
            (insert "# Network drivers\n"
                    "config NET_DEV\n"
                    "\tbool \"Network device\"\n"))
          
          (with-temp-file (expand-file-name "drivers/net/ethernet/Kconfig" temp-dir)
            (insert "# Ethernet drivers\n"
                    "config ETHERNET_DEV\n"
                    "\tbool \"Ethernet device\"\n"))
          
          (with-temp-file (expand-file-name "drivers/gpu/Kconfig" temp-dir)
            (insert "# GPU drivers\n"
                    "config GPU_DEV\n"
                    "\tbool \"GPU device\"\n"))
          
          ;; Test variable expansion
          (message "Testing variable expansion...")
          (let ((build-vars (linconf-get-kernel-build-vars temp-dir)))
            (message "Build vars: %s" build-vars))
          
          (message "\nTesting path expansions:")
          (let ((test-paths '("arch/$(SRCARCH)/Kconfig"
                             "arch/$(HEADER_ARCH)/Kconfig.debug" 
                             "drivers/*/Kconfig"
                             "drivers/**/Kconfig")))
            (dolist (path test-paths)
              (message "Pattern: %s" path)
              (let ((expanded (linconf-expand-kconfig-variables path temp-dir)))
                (message "  Expanded: %s" expanded)
                (let ((files (linconf-expand-glob-pattern path temp-dir)))
                  (message "  Found %d files: %s" (length files) files)))))
          
          ;; Test source directive parsing
          (message "\nTesting source directive parsing...")
          (let ((sources (linconf-parse-source-directives 
                         (expand-file-name "Kconfig" temp-dir) 
                         temp-dir)))
            (message "Found %d source files:" (length sources))
            (dolist (source sources)
              (message "  - %s" source)))
          
          ;; Test full file collection
          (message "\nTesting complete file collection...")
          (let ((all-files (linconf-collect-kconfig-files temp-dir)))
            (message "Total Kconfig files found: %d" (length all-files))
            (dolist (file all-files)
              (message "  - %s" (file-relative-name file temp-dir))))
          
          ;; Test parsing all files
          (message "\nTesting parsing of all files...")
          (clrhash linconf-kconfig-options)
          (let ((total-options 0))
            (dolist (file (linconf-collect-kconfig-files temp-dir))
              (let ((options (linconf-parse-kconfig-file file)))
                (message "Found %d options in %s" 
                         (length options) 
                         (file-relative-name file temp-dir))
                (dolist (option options)
                  (puthash (car option) (cdr option) linconf-kconfig-options)
                  (setq total-options (1+ total-options)))))
            (message "\nTotal configuration options found: %d" total-options)))
      
      ;; Cleanup
      (delete-directory temp-dir t))))

(test-improved-source-expansion)