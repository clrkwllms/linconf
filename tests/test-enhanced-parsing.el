(load "/nas/src/linconf-emacs/linconf.el")

(defun test-enhanced-kconfig-parsing ()
  "Test the enhanced Kconfig parsing with menuconfig, choice, and menu blocks."
  (let ((temp-file (make-temp-file "enhanced-kconfig-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "# Test file with various Kconfig constructs\n"
                    "\n"
                    "# Regular config option\n"
                    "config REGULAR_OPTION\n"
                    "\tbool \"Regular boolean option\"\n"
                    "\tdefault y\n"
                    "\thelp\n"
                    "\t  This is a regular config option.\n"
                    "\n"
                    "# Menuconfig option\n"
                    "menuconfig SUBSYSTEM_SUPPORT\n"
                    "\tbool \"Enable subsystem support\"\n"
                    "\tdefault n\n"
                    "\thelp\n"
                    "\t  Enable support for the subsystem.\n"
                    "\n"
                    "menu \"Subsystem Options\"\n"
                    "\tdepends on SUBSYSTEM_SUPPORT\n"
                    "\n"
                    "config SUBSYSTEM_FEATURE_A\n"
                    "\tbool \"Feature A\"\n"
                    "\tdefault y\n"
                    "\n"
                    "config SUBSYSTEM_FEATURE_B\n"
                    "\ttristate \"Feature B (can be module)\"\n"
                    "\tdefault m\n"
                    "\n"
                    "endmenu\n"
                    "\n"
                    "# Choice block\n"
                    "choice\n"
                    "\tprompt \"Choose CPU scheduler\"\n"
                    "\tdefault SCHED_CFS\n"
                    "\n"
                    "config SCHED_CFS\n"
                    "\tbool \"Completely Fair Scheduler\"\n"
                    "\thelp\n"
                    "\t  Use CFS scheduler.\n"
                    "\n"
                    "config SCHED_RT\n"
                    "\tbool \"Real-time scheduler\"\n"
                    "\thelp\n"
                    "\t  Use RT scheduler.\n"
                    "\n"
                    "endchoice\n"
                    "\n"
                    "# Another regular config\n"
                    "config FINAL_OPTION\n"
                    "\tstring \"Final option value\"\n"
                    "\tdefault \"test\"\n"))
          
          (message "Testing enhanced Kconfig parsing...")
          (let ((options (kconfig-parse-kconfig-file temp-file)))
            (message "Found %d total options:" (length options))
            
            (dolist (option options)
              (let ((name (car option))
                    (plist (cdr option)))
                (message "Option: %s" name)
                (message "  Type: %s" (plist-get plist :type))
                (message "  Default: %s" (plist-get plist :default))
                (when (plist-get plist :menuconfig)
                  (message "  [MENUCONFIG]"))
                (when (plist-get plist :choices)
                  (message "  Choices: %s" (plist-get plist :choices)))
                (message "")))
            
            ;; Verify specific options were found
            (let ((regular (assoc "REGULAR_OPTION" options))
                  (menuconfig (assoc "SUBSYSTEM_SUPPORT" options))
                  (feature-a (assoc "SUBSYSTEM_FEATURE_A" options))
                  (feature-b (assoc "SUBSYSTEM_FEATURE_B" options))
                  (sched-cfs (assoc "SCHED_CFS" options))
                  (sched-rt (assoc "SCHED_RT" options))
                  (final (assoc "FINAL_OPTION" options)))
              
              (message "Verification:")
              (message "- Regular option found: %s" (if regular "YES" "NO"))
              (message "- Menuconfig option found: %s" (if menuconfig "YES" "NO"))
              (when menuconfig
                (message "  - Is menuconfig: %s" 
                         (if (eq (plist-get (cdr menuconfig) :type) 'menuconfig) "YES" "NO")))
              (message "- Feature A (in menu) found: %s" (if feature-a "YES" "NO"))
              (message "- Feature B (in menu) found: %s" (if feature-b "YES" "NO"))
              (message "- Choice option CFS found: %s" (if sched-cfs "YES" "NO"))
              (message "- Choice option RT found: %s" (if sched-rt "YES" "NO"))
              (message "- Final option found: %s" (if final "YES" "NO"))
              
              ;; Look for choice group
              (let ((choice-groups (seq-filter 
                                   (lambda (opt) 
                                     (eq (plist-get (cdr opt) :type) 'choice))
                                   options)))
                (message "- Choice groups found: %d" (length choice-groups))
                (dolist (choice choice-groups)
                  (message "  Choice: %s with options: %s" 
                           (car choice)
                           (plist-get (cdr choice) :choices))))
              
              (message "\nTotal unique config options found: %d" (length options)))))
      ;; Cleanup
      (delete-file temp-file))))

(test-enhanced-kconfig-parsing)