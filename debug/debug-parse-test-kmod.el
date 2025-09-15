#!/usr/bin/emacs --script
;;; debug-parse-test-kmod.el --- Debug TEST_KMOD parsing step by step

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Test parsing of TEST_KMOD directly
(let ((test-lines '("config TEST_KMOD"
                    "\ttristate \"kmod stress tester\""
                    "\tdepends on m"
                    "\tselect TEST_LKM"
                    "\thelp"
                    "\t  Test the kernel's module loading mechanism")))
  (let ((result (linconf-parse-kconfig-option test-lines 'config)))
    (message "Parsed result: %s" result)
    (when result
      (message "Name: %s" (car result))
      (message "Type: %s" (plist-get (cdr result) :type))
      (message "Depends: %s" (plist-get (cdr result) :depends))
      (message "Select: %s" (plist-get (cdr result) :select))
      (message "Help: %s" (plist-get (cdr result) :help)))))

;; Also test with full file parsing to see if context matters
(message "\n--- Testing with file parsing ---")
(let* ((test-content "config TEST_KMOD\n\ttristate \"kmod stress tester\"\n\tdepends on m\n\tselect TEST_LKM\n\thelp\n\t  Test the kernel's module loading mechanism\n")
       (options (linconf-parse-kconfig-content test-content)))
  (dolist (option options)
    (when (string= (car option) "TEST_KMOD")
      (message "Full parse - Name: %s" (car option))
      (message "Full parse - Type: %s" (plist-get (cdr option) :type))
      (message "Full parse - Depends: %s" (plist-get (cdr option) :depends)))))
