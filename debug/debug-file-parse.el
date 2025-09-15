#!/usr/bin/emacs --script
;;; debug-file-parse.el --- Debug parsing of TEST_KMOD from actual file

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Parse the actual Kconfig.debug file and find TEST_KMOD
(let ((options (linconf-parse-kconfig-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/lib/Kconfig.debug")))
  (message "Parsed %d options from Kconfig.debug" (length options))
  (dolist (option options)
    (when (string= (car option) "TEST_KMOD")
      (message "Found TEST_KMOD:")
      (message "  Type: %s" (plist-get (cdr option) :type))
      (message "  Depends: %s" (plist-get (cdr option) :depends))
      (message "  Select: %s" (plist-get (cdr option) :select))
      (message "  Help: %s" (substring (or (plist-get (cdr option) :help) "No help") 0 50)))))