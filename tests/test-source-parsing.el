(load "/nas/src/linconf-emacs/linconf.el")

(defun test-improved-source-parsing ()
  "Test the improved source directive parsing with glob patterns and variables."
  (let ((temp-dir (make-temp-file "kconfig-test" t)))
    (unwind-protect
        (progn
          ;; Create test directory structure
          (make-directory (expand-file-name "drivers" temp-dir))
          (make-directory (expand-file-name "drivers/block" temp-dir))
          (make-directory (expand-file-name "drivers/net" temp-dir))
          (make-directory (expand-file-name "arch/x86" temp-dir) t)
          
          ;; Create test Kconfig files
          (with-temp-file (expand-file-name "Kconfig" temp-dir)
            (insert "# Main Kconfig with various source patterns\n"
                    "source \"drivers/*/Kconfig\"\n"
                    "source \"arch/$SRCARCH/Kconfig\"\n"
                    "if X86\n"
                    "source \"arch/x86/Kconfig.debug\"\n"
                    "endif\n"))
          
          (with-temp-file (expand-file-name "drivers/block/Kconfig" temp-dir)
            (insert "# Block drivers Kconfig\n"
                    "config BLOCK_DEV\n"
                    "\tbool \"Block device support\"\n"))
          
          (with-temp-file (expand-file-name "drivers/net/Kconfig" temp-dir)
            (insert "# Network drivers Kconfig\n"
                    "config NET_DEV\n"
                    "\tbool \"Network device support\"\n"))
          
          (with-temp-file (expand-file-name "arch/x86/Kconfig" temp-dir)
            (insert "# x86 arch Kconfig\n"
                    "config X86_FEATURE\n"
                    "\tbool \"X86 specific feature\"\n"))
          
          (with-temp-file (expand-file-name "arch/x86/Kconfig.debug" temp-dir)
            (insert "# x86 debug Kconfig\n"
                    "config X86_DEBUG\n"
                    "\tbool \"X86 debugging\"\n"))
          
          ;; Test the improved parsing
          (message "Testing source directive parsing...")
          (let ((sources (linconf-parse-source-directives 
                         (expand-file-name "Kconfig" temp-dir) 
                         temp-dir)))
            (message "Found %d source files:" (length sources))
            (dolist (source sources)
              (message "  - %s" source))
            
            ;; Test collecting all files
            (message "\nTesting file collection...")
            (let ((all-files (linconf-collect-kconfig-files temp-dir)))
              (message "Found %d total Kconfig files:" (length all-files))
              (dolist (file all-files)
                (message "  - %s" file))
              
              ;; Parse all files and count options
              (message "\nTesting option parsing...")
              (clrhash kconfig-options)
              (let ((total-options 0))
                (dolist (file all-files)
                  (let ((options (kconfig-parse-kconfig-file file)))
                    (message "Found %d options in %s" (length options) (file-name-nondirectory file))
                    (dolist (option options)
                      (puthash (car option) (cdr option) kconfig-options)
                      (setq total-options (1+ total-options)))))
                (message "\nTotal options found: %d" total-options)
                (message "Options in hash table: %d" (hash-table-count kconfig-options))))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(test-improved-source-parsing)