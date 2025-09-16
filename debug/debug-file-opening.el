#!/usr/bin/emacs --script
;;; debug-file-opening.el --- Debug what happens when opening config files

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test what happens when we simulate opening a file
(let ((test-config "/nas/src/RedHat/gitlab/kernel-ark/os-build/redhat/configs/kernel-6.17.0-x86_64.config"))
  (when (file-exists-p test-config)
    (message "Simulating file opening...")

    ;; Clear state
    (setq linconf-detected-architecture nil)

    ;; Simulate what happens when find-file opens a .config file
    (with-temp-buffer
      (message "1. Created buffer")

      (insert-file-contents test-config)
      (message "2. Inserted file contents")

      (linconf-mode)
      (message "3. Activated linconf-mode")
      (message "   Detected architecture: %s" linconf-detected-architecture)
      (message "   Mode line arch: '%s'" (linconf-mode-line-architecture))

      ;; Test if we need to trigger architecture detection manually
      (message "4. Testing manual architecture detection...")
      (goto-char (point-min))
      (when (looking-at "^#[ \t]*\\([a-zA-Z0-9_]+\\)")
        (let ((arch-comment (match-string 1)))
          (message "   Found comment: %s" arch-comment)
          (let ((detected-arch (linconf-detect-architecture-from-config test-config)))
            (message "   Detected arch: %s" detected-arch))))

      ;; Test calling parse function
      (message "5. Calling linconf-parse-config-file...")
      (linconf-parse-config-file test-config)
      (message "   After parsing - detected architecture: %s" linconf-detected-architecture)
      (message "   Mode line arch: '%s'" (linconf-mode-line-architecture)))))