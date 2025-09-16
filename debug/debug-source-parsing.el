#!/usr/bin/env emacs --script
;; Debug source directive parsing to find why kernel/Kconfig.hz is not included

(load-file "linconf.el")

(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")

;; Test source directive parsing specifically for arch/x86/Kconfig
(message "=== Source Directive Parsing Test ===")

(let ((x86-kconfig "/nas/src/RedHat/gitlab/kernel-ark/linus/arch/x86/Kconfig"))
  (message "Testing source parsing from: %s" x86-kconfig)

  ;; Get source directives from arch/x86/Kconfig
  (let ((sources (linconf-parse-source-directives x86-kconfig linconf-kernel-source-path)))
    (message "Found %d source directives:" (length sources))
    (dolist (source sources)
      (message "  %s" source)
      ;; Check if kernel/Kconfig.hz is in the list
      (when (string-match "kernel/Kconfig.hz" source)
        (message "    ✓ Found kernel/Kconfig.hz!")
        (message "    File exists: %s" (file-exists-p source))))))

;; Also test the collect-kconfig-files function to see full file list
(message "\n=== Full File Collection Test ===")
(let ((all-files (linconf-collect-kconfig-files linconf-kernel-source-path))
      (hz-found nil))
  (message "Total Kconfig files found: %d" (length all-files))
  (dolist (file all-files)
    (when (string-match "kernel/Kconfig.hz" file)
      (setq hz-found t)
      (message "✓ Found kernel/Kconfig.hz in file list: %s" file)))
  (unless hz-found
    (message "✗ kernel/Kconfig.hz NOT found in file collection!")))

;; Direct test: manually check if kernel/Kconfig.hz can be parsed
(message "\n=== Direct Hz File Parsing Test ===")
(let ((hz-file (expand-file-name "kernel/Kconfig.hz" linconf-kernel-source-path)))
  (message "Direct parse of: %s" hz-file)
  (message "File exists: %s" (file-exists-p hz-file))
  (when (file-exists-p hz-file)
    (let ((hz-options (linconf-parse-kconfig-file hz-file)))
      (message "Options found in Hz file: %d" (length hz-options))
      (dolist (option hz-options)
        (message "  %s: %s" (car option) (plist-get (cdr option) :type))))))

(message "\n=== DONE ===")