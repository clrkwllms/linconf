#!/usr/bin/env emacs --script
;; Test if the HZ choice parsing fix works

(load-file "linconf.el")

(message "=== HZ Choice Parsing Fix Test ===")

;; Test direct parsing of kernel/Kconfig.hz
(let ((hz-file "/nas/src/RedHat/gitlab/kernel-ark/linus/kernel/Kconfig.hz"))
  (message "Parsing file: %s" hz-file)
  (let ((options (linconf-parse-kconfig-file hz-file)))
    (message "Total options parsed: %d" (length options))
    (dolist (option options)
      (let ((name (car option))
            (props (cdr option)))
        (message "  %s: type=%s" name (plist-get props :type))))

    ;; Check specifically for HZ options
    (let ((hz-options '("HZ_100" "HZ_250" "HZ_300" "HZ_1000")))
      (message "\n=== HZ Options Status ===")
      (dolist (hz-option hz-options)
        (let ((found (assoc hz-option options)))
          (if found
              (message "✓ %s: %s" hz-option (plist-get (cdr found) :type))
            (message "✗ %s: NOT FOUND" hz-option)))))))

(message "\n=== DONE ===")