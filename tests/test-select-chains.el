(load "/nas/src/linconf-emacs/linconf.el")

(defun test-select-chain-resolution ()
  "Test select chain resolution with complex scenarios."
  (let ((temp-file (make-temp-file "select-chain-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "# Test file for select chain resolution\n"
                    "\n"
                    "config ARCH_X86\n"
                    "\tdef_bool y\n"
                    "\tselect ARCH_SUPPORTS_DEBUG\n"
                    "\tselect GENERIC_CPU_DEVICES\n"
                    "\tselect HAVE_ARCH_TRACEHOOK\n"
                    "\n"
                    "config SMP\n"
                    "\tbool \"Symmetric multi-processing support\"\n"
                    "\tselect USE_GENERIC_SMP_HELPERS\n"
                    "\tselect IRQ_WORK\n"
                    "\n"
                    "config X86_64\n"
                    "\tdef_bool y\n"
                    "\tdepends on 64BIT\n"
                    "\tselect ARCH_SUPPORTS_MSI if PCI\n"
                    "\tselect HAVE_EFFICIENT_UNALIGNED_ACCESS\n"
                    "\tselect SWIOTLB\n"
                    "\n"
                    "config PCI\n"
                    "\tbool \"PCI support\"\n"
                    "\tselect PCI_DOMAINS\n"
                    "\tselect GENERIC_PCI_IOMAP\n"
                    "\n"
                    "# Selected options (usually infrastructure)\n"
                    "config ARCH_SUPPORTS_DEBUG\n"
                    "\tbool\n"
                    "\n"
                    "config GENERIC_CPU_DEVICES\n"
                    "\tbool\n"
                    "\tselect CPU_DEV_TREE\n"
                    "\n"
                    "config CPU_DEV_TREE\n"
                    "\tbool\n"
                    "\n"
                    "config HAVE_ARCH_TRACEHOOK\n"
                    "\tbool\n"
                    "\n"
                    "config USE_GENERIC_SMP_HELPERS\n"
                    "\tbool\n"
                    "\n"
                    "config IRQ_WORK\n"
                    "\tbool\n"
                    "\n"
                    "config ARCH_SUPPORTS_MSI\n"
                    "\tbool\n"
                    "\n"
                    "config HAVE_EFFICIENT_UNALIGNED_ACCESS\n"
                    "\tbool\n"
                    "\n"
                    "config SWIOTLB\n"
                    "\tbool\n"
                    "\tselect DMA_DIRECT_OPS\n"
                    "\n"
                    "config DMA_DIRECT_OPS\n"
                    "\tbool\n"
                    "\n"
                    "config PCI_DOMAINS\n"
                    "\tbool\n"
                    "\n"
                    "config GENERIC_PCI_IOMAP\n"
                    "\tbool\n"))
          
          (message "Testing select chain resolution with complex scenarios...")
          
          ;; Parse the test file and build select chains
          (let ((options (kconfig-parse-kconfig-file temp-file)))
            (message "\n1. PARSING RESULTS:")
            (message "   Found %d total options" (length options))
            
            ;; Clear and rebuild select chains
            (clrhash linconf-select-chains)
            (clrhash kconfig-config-values)
            
            ;; Process options and build select chains
            (dolist (option options)
              (puthash (car option) (cdr option) kconfig-options)
              
              (let ((option-name (car option))
                    (select-statements (plist-get (cdr option) :select)))
                (dolist (select-stmt select-statements)
                  (if (consp select-stmt)
                      (linconf-add-select-statement option-name (car select-stmt) (cdr select-stmt))
                    (linconf-add-select-statement option-name select-stmt)))))
            
            ;; Set up some basic config values
            (kconfig-set-config-value "64BIT" t)
            (kconfig-set-config-value "PCI" t)
            
            (message "\n2. SELECT CHAIN ANALYSIS:")
            (message "   Total select relationships built: %d" (hash-table-count linconf-select-chains))
            
            ;; Show select chains for key options
            (let ((key-options '("ARCH_X86" "SMP" "X86_64" "PCI")))
              (dolist (option key-options)
                (let ((selects (linconf-get-select-statements option)))
                  (when selects
                    (message "   %s selects: %s" option
                             (mapconcat (lambda (pair)
                                          (if (cdr pair)
                                              (format "%s if %s" (car pair) (cdr pair))
                                            (car pair)))
                                        selects ", "))))))
            
            (message "\n3. CHAIN RESOLUTION TESTS:")
            
            ;; Test 1: Simple chain
            (message "   Test 1: Enable ARCH_X86")
            (let ((changes (linconf-simulate-config-change "ARCH_X86" t)))
              (message "     Expected 3 auto-selects, got: %d" (length changes)))
            
            ;; Test 2: Multi-level chain
            (message "\n   Test 2: Enable PCI")
            (let ((changes (linconf-simulate-config-change "PCI" t)))
              (message "     Expected 2 auto-selects, got: %d" (length changes)))
            
            ;; Test 3: Conditional select
            (message "\n   Test 3: Enable X86_64 (with PCI=y)")
            (kconfig-set-config-value "PCI" t) ; Ensure PCI is enabled
            (let ((changes (linconf-simulate-config-change "X86_64" t)))
              (message "     Expected 3 auto-selects (including conditional), got: %d" (length changes)))
            
            ;; Test 4: Deep chain (GENERIC_CPU_DEVICES -> CPU_DEV_TREE)
            (message "\n   Test 4: Deep chain test via ARCH_X86")
            (clrhash kconfig-config-values) ; Reset values
            (kconfig-set-config-value "ARCH_X86" t)
            (linconf-apply-select-chains "ARCH_X86" t)
            (let ((cpu-dev-tree-value (kconfig-get-config-value "CPU_DEV_TREE")))
              (message "     CPU_DEV_TREE should be auto-enabled: %s" 
                       (if (eq cpu-dev-tree-value t) "YES" "NO")))
            
            (message "\n4. DEPENDENCY INFO TESTS:")
            
            ;; Test dependency info for a complex option
            (let ((info (linconf-get-dependency-info "X86_64")))
              (message "   X86_64 dependency info:")
              (message "     Depends on: %s" (or (plist-get info :depends) "none"))
              (message "     Selects: %s" (or (plist-get info :selects) "none"))
              (message "     Selected by: %s" (or (plist-get info :selected-by) "none")))
            
            ;; Test reverse dependency lookup
            (let ((reverse (linconf-find-reverse-selects "ARCH_SUPPORTS_DEBUG")))
              (message "\n   ARCH_SUPPORTS_DEBUG is selected by:")
              (dolist (selector reverse)
                (message "     %s%s" (car selector) 
                         (if (cdr selector) (format " if %s" (cdr selector)) ""))))
            
            (message "\n5. INTEGRATION TEST:")
            (message "   Testing complete workflow...")
            
            ;; Reset everything
            (clrhash kconfig-config-values)
            
            ;; Enable a high-level option and trace all changes
            (message "   Enabling SMP...")
            (linconf-set-config-with-chains "SMP" t)
            
            (message "   Final config state:")
            (maphash (lambda (key value)
                       (when (eq value t)
                         (message "     %s = y" key)))
                     kconfig-config-values)
            
            (message "\n✓ Select chain resolution tests completed!")))
      
      ;; Cleanup
      (delete-file temp-file))))

(test-select-chain-resolution)