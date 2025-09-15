#!/usr/bin/emacs --script
;;; debug-regex-match.el --- Check which lines match the type regex

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; The type regex from the parser
(defvar type-regex "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\(?: \"\\([^\"]*\\)\"\\)?")

;; Test lines from TEST_KMOD
(let ((test-lines '("config TEST_KMOD"
                    "\ttristate \"kmod stress tester\""
                    "\tdepends on m"
                    "\tselect TEST_LKM"
                    "\thelp"
                    "\t  Test the kernel's module loading mechanism: kmod. kmod implements"
                    "\t  support to load modules using the Linux kernel's usermode helper."
                    "\t  This test provides a series of tests against kmod."
                    ""
                    "\t  Although technically you can either build test_kmod as a module or"
                    "\t  into the kernel we disallow building it into the kernel since"
                    "\t  it stress tests request_module() and this will very likely cause"
                    "\t  some issues by taking over precious threads available from other"
                    "\t  module load requests, ultimately this could be fatal."
                    ""
                    "\t  To run tests run:"
                    ""
                    "\t  tools/testing/selftests/kmod/kmod.sh --help"
                    ""
                    "\t  If unsure, say N."
                    "")))
  (message "Checking each line against type regex:")
  (dolist (line test-lines)
    (when (string-match type-regex line)
      (message "MATCH: Line '%s' -> type: %s" line (match-string 1 line)))))