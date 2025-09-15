#!/usr/bin/emacs --script
;;; debug-parser-trace.el --- Trace the parsing of TEST_KMOD step by step

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Override the parsing function to add debug tracing for TEST_KMOD
(defvar original-parse-kconfig-option (symbol-function 'linconf-parse-kconfig-option))

(defun traced-linconf-parse-kconfig-option (lines entry-type)
  "Traced version of linconf-parse-kconfig-option."
  (let ((contains-test-kmod (cl-some (lambda (line) (string-match "TEST_KMOD" line)) lines)))
    (when contains-test-kmod
      (message "TRACE: Parsing lines containing TEST_KMOD:")
      (dolist (line lines)
        (message "  Line: %s" line)))

    (let ((result (funcall original-parse-kconfig-option lines entry-type)))
      (when (and result (string= (car result) "TEST_KMOD"))
        (message "TRACE: TEST_KMOD parsed result:")
        (message "  Name: %s" (car result))
        (message "  Type: %s" (plist-get (cdr result) :type))
        (message "  Depends: %s" (plist-get (cdr result) :depends)))
      result)))

(fset 'linconf-parse-kconfig-option 'traced-linconf-parse-kconfig-option)

;; Parse the Kconfig.debug file
(message "Parsing Kconfig.debug with tracing...")
(let ((options (linconf-parse-kconfig-file "/nas/src/RedHat/gitlab/kernel-ark/os-build/lib/Kconfig.debug")))
  (message "Parsing complete."))