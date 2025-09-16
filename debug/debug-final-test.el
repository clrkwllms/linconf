#!/usr/bin/env emacs --script
;; Final comprehensive test of the choice parsing fix

(load-file "linconf.el")

(message "=== Final Choice Parsing Test ===")

(let ((hz-file "/nas/src/RedHat/gitlab/kernel-ark/linus/kernel/Kconfig.hz"))
  (message "Parsing: %s" hz-file)

  ;; Parse the file
  (let ((options (linconf-parse-kconfig-file hz-file)))
    (message "Total options found: %d" (length options))

    ;; List all options
    (message "\nAll options:")
    (dolist (option options)
      (let ((name (car option))
            (props (cdr option)))
        (message "  %s: type=%s" name (plist-get props :type))))

    ;; Check for HZ options specifically
    (message "\nLooking for HZ choice options:")
    (let ((hz-found 0))
      (dolist (option options)
        (when (string-match "HZ_[0-9]+" (car option))
          (setq hz-found (1+ hz-found))
          (message "  ✓ %s" (car option))))
      (message "Total HZ options found: %d" hz-found))

    ;; Check choice groups
    (message "\nChoice groups:")
    (dolist (option options)
      (when (eq (plist-get (cdr option) :type) 'choice)
        (let ((choices (plist-get (cdr option) :choices)))
          (message "  %s: choices=%s" (car option) choices))))))

(message "\n=== DONE ===")