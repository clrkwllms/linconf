#!/usr/bin/env emacs --script
;; Debug architecture resolution and variable expansion for missing Kconfig options

(load-file "linconf.el")

(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")

;; Test 1: Check what architecture is detected from the kernel source vs config file
(let ((config-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (message "=== Architecture Detection Test ===")
  (message "From kernel source: %s" (linconf-detect-architecture linconf-kernel-source-path))
  (message "From config file: %s" (linconf-detect-architecture-from-config config-file))

  ;; Test 2: Check build vars with different methods
  (message "\n=== Build Variables Test ===")
  (let ((build-vars-source (linconf-get-kernel-build-vars linconf-kernel-source-path)))
    (message "Build vars from source tree:")
    (dolist (var build-vars-source)
      (message "  %s = %s" (car var) (cdr var))))

  ;; Test 3: Check if arch-specific Kconfig is properly resolved
  (message "\n=== Path Resolution Test ===")
  (let* ((arch-kconfig-path "arch/$(SRCARCH)/Kconfig")
         (expanded-path (linconf-expand-kconfig-variables arch-kconfig-path linconf-kernel-source-path))
         (full-path (expand-file-name expanded-path linconf-kernel-source-path)))
    (message "Original path: %s" arch-kconfig-path)
    (message "Expanded path: %s" expanded-path)
    (message "Full path: %s" full-path)
    (message "File exists: %s" (file-exists-p full-path)))

  ;; Test 4: Check if kernel/Kconfig.hz would be found
  (message "\n=== HZ Kconfig Resolution Test ===")
  (let* ((hz-path "kernel/Kconfig.hz")
         (full-hz-path (expand-file-name hz-path linconf-kernel-source-path)))
    (message "HZ Kconfig path: %s" full-hz-path)
    (message "HZ File exists: %s" (file-exists-p full-hz-path))

    ;; Check if arch/x86/Kconfig includes it
    (let ((x86-kconfig (expand-file-name "arch/x86/Kconfig" linconf-kernel-source-path)))
      (when (file-exists-p x86-kconfig)
        (with-temp-buffer
          (insert-file-contents x86-kconfig)
          (goto-char (point-min))
          (if (search-forward "kernel/Kconfig.hz" nil t)
              (message "Found HZ inclusion in arch/x86/Kconfig: line %d" (line-number-at-pos))
            (message "HZ inclusion NOT found in arch/x86/Kconfig")))))))

(message "\n=== DONE ===")