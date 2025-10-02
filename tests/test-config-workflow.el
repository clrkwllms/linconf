(load "/nas/src/linconf-emacs/linconf.el")

(defun test-config-workflow ()
  "Test complete .config file read/modify/write workflow."
  (let ((test-dir (make-temp-file "config-test" t))
        (test-kconfig (make-temp-file "test-kconfig"))
        (test-config nil))
    (unwind-protect
        (progn
          (setq test-config (expand-file-name ".config" test-dir))
          
          ;; Create a simple test Kconfig
          (with-temp-file test-kconfig
            (insert "config TEST_OPTION_A\n"
                    "\tbool \"Test option A\"\n"
                    "\tdefault y\n"
                    "\n"
                    "config TEST_OPTION_B\n"
                    "\tbool \"Test option B\"\n"
                    "\tdepends on TEST_OPTION_A\n"
                    "\n"
                    "config TEST_OPTION_C\n"
                    "\ttristate \"Test option C\"\n"
                    "\tselect TEST_OPTION_D\n"
                    "\n"
                    "config TEST_OPTION_D\n"
                    "\tbool \"Test option D\"\n"))
          
          ;; Create an initial .config file
          (with-temp-file test-config
            (insert "# Test configuration file\n"
                    "CONFIG_TEST_OPTION_A=y\n"
                    "# CONFIG_TEST_OPTION_B is not set\n"
                    "CONFIG_TEST_OPTION_C=m\n"
                    "CONFIG_TEST_OPTION_D=y\n"))
          
          (message "Testing complete .config workflow...")
          
          ;; Test 1: Load Kconfig and .config
          (message "\n1. LOADING PHASE:")
          ;; Set the kernel source path for testing
          (setq linconf-kernel-source-path test-dir)
          ;; Manually parse the test kconfig file
          (clrhash kconfig-options)
          (clrhash kconfig-config-values)
          (clrhash linconf-select-chains)
          (let ((options (kconfig-parse-kconfig-file test-kconfig)))
            (dolist (option options)
              (puthash (car option) (cdr option) kconfig-options)))
          (setq kconfig-loaded t)
          ;; Load the .config file
          (linconf-load-config-file test-config)
          
          ;; Verify loaded config values
          (message "   Loaded config values:")
          (let ((test-values '("TEST_OPTION_A" "TEST_OPTION_B" "TEST_OPTION_C" "TEST_OPTION_D")))
            (dolist (option test-values)
              (let ((value (kconfig-get-config-value option)))
                (message "     %s = %s" option
                         (cond ((eq value t) "y")
                               ((eq value 'm) "m") 
                               ((eq value nil) "not set")
                               (t (format "%s" value)))))))
          
          ;; Test 2: Modify configuration
          (message "\n2. MODIFICATION PHASE:")
          (message "   Enabling TEST_OPTION_B...")
          (linconf-set-config-with-chains "TEST_OPTION_B" t)
          
          (message "   Changing TEST_OPTION_C from m to y...")
          (linconf-set-config-with-chains "TEST_OPTION_C" t)
          
          ;; Verify modifications
          (message "   Modified config values:")
          (let ((test-values '("TEST_OPTION_A" "TEST_OPTION_B" "TEST_OPTION_C" "TEST_OPTION_D")))
            (dolist (option test-values)
              (let ((value (kconfig-get-config-value option)))
                (message "     %s = %s" option
                         (cond ((eq value t) "y")
                               ((eq value 'm) "m")
                               ((eq value nil) "not set")
                               (t (format "%s" value)))))))
          
          ;; Test 3: Write modified .config
          (message "\n3. WRITING PHASE:")
          (let ((backup-config (concat test-config ".backup")))
            (copy-file test-config backup-config)
            (message "   Original .config backed up to %s" backup-config))
          
          (message "   Writing modified .config...")
          (linconf-write-config-file test-config)
          
          ;; Test 4: Verify written file
          (message "\n4. VERIFICATION PHASE:")
          (message "   Contents of written .config:")
          (with-temp-buffer
            (insert-file-contents test-config)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (dolist (line lines)
                (message "     %s" line))))
          
          ;; Test 5: Re-load and verify
          (message "\n5. RE-LOAD TEST:")
          (clrhash kconfig-config-values)
          (message "   Cleared internal state, reloading...")
          (linconf-load-config-file test-config)
          
          (message "   Re-loaded config values:")
          (let ((test-values '("TEST_OPTION_A" "TEST_OPTION_B" "TEST_OPTION_C" "TEST_OPTION_D")))
            (dolist (option test-values)
              (let ((value (kconfig-get-config-value option)))
                (message "     %s = %s" option
                         (cond ((eq value t) "y")
                               ((eq value 'm) "m")
                               ((eq value nil) "not set") 
                               (t (format "%s" value)))))))
          
          ;; Test 6: Save/restore functionality
          (message "\n6. SAVE/RESTORE TEST:")
          (let ((snapshot (linconf-save-config-snapshot)))
            (message "   Saved config snapshot with %d entries" (hash-table-count snapshot))
            
            ;; Make some changes
            (kconfig-set-config-value "TEST_OPTION_A" nil)
            (kconfig-set-config-value "TEST_OPTION_B" nil)
            (message "   Made temporary changes...")
            
            ;; Restore 
            (linconf-restore-config-snapshot snapshot)
            (message "   Restored from snapshot")
            
            ;; Verify restoration
            (let ((restored-a (kconfig-get-config-value "TEST_OPTION_A"))
                  (restored-b (kconfig-get-config-value "TEST_OPTION_B")))
              (message "     TEST_OPTION_A restored to: %s" 
                       (if (eq restored-a t) "y" "not set"))
              (message "     TEST_OPTION_B restored to: %s"
                       (if (eq restored-b t) "y" "not set"))))
          
          (message "\n✓ Complete .config workflow test completed successfully!"))
      
      ;; Cleanup
      (when (file-exists-p test-kconfig)
        (delete-file test-kconfig))
      (when (file-directory-p test-dir)
        (delete-directory test-dir t)))))

(test-config-workflow)