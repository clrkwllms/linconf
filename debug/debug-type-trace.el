#!/usr/bin/emacs --script
;;; debug-type-trace.el --- Trace type variable changes in parsing

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Create a modified version of linconf-parse-kconfig-option with detailed tracing
(defun debug-linconf-parse-kconfig-option (lines entry-type)
  "Debug version of linconf-parse-kconfig-option with type tracing."
  (let ((name nil)
        (type nil)
        (help nil)
        (depends nil)
        (select nil)
        (default nil)
        (range nil)
        (choices nil)
        (in-help nil)
        (is-menuconfig (eq entry-type 'menuconfig))
        (is-comment (eq entry-type 'comment))
        (comment-text nil))

    ;; Check if this is for TEST_KMOD
    (let ((is-test-kmod (cl-some (lambda (line) (string-match "TEST_KMOD" line)) lines)))
      (when is-test-kmod
        (message "TRACE: Starting parse of TEST_KMOD, initial type: %s" type))

      (dolist (line lines)
        (when is-test-kmod
          (message "TRACE: Processing line: %s" line)
          (message "TRACE: Type before processing: %s" type))

        (cond
         ;; Handle comment declaration
         ((string-match "^comment\\s-+\"\\([^\"]+\\)\"" line)
          (setq name (format "COMMENT_%s" (replace-regexp-in-string "[^A-Z0-9_]" "_" (upcase (match-string 1 line)))))
          (setq comment-text (match-string 1 line))
          (setq type 'comment)
          (when is-test-kmod (message "TRACE: Set type to comment")))

         ;; Handle config or menuconfig declaration
         ((string-match "^\\(menu\\)?config \\([A-Z0-9_]+\\)" line)
          (setq name (match-string 2 line))
          (when is-test-kmod (message "TRACE: Found config name: %s" name)))

         ;; Handle type declarations
         ((string-match "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\(?: \"\\([^\"]*\\)\"\\)?" line)
          (setq type (intern (match-string 1 line)))
          (when is-test-kmod (message "TRACE: Set type to: %s" type)))

         ;; Handle def_bool and def_tristate (type + default combined)
         ((string-match "^[ \t]+def_\\(bool\\|tristate\\)\\s-+\\(.+\\)" line)
          (setq type (intern (match-string 1 line)))
          (setq default (match-string 2 line))
          (when is-test-kmod (message "TRACE: Set type to: %s (def_ variant)" type)))

         ;; Handle dependencies
         ((string-match "^[ \t]+depends on \\(.+\\)" line)
          (setq depends (match-string 1 line))
          (when is-test-kmod (message "TRACE: Set depends to: %s" depends)))

         ;; Handle ranges
         ((string-match "^[ \t]+range \\([0-9]+\\) \\([0-9]+\\)" line)
          (setq range (cons (string-to-number (match-string 1 line))
                            (string-to-number (match-string 2 line))))
          (when is-test-kmod (message "TRACE: Set range to: %s" range)))

         ;; Handle help text
         ((string-match "^[ \t]+help" line)
          (setq in-help t)
          (when is-test-kmod (message "TRACE: Entering help section")))

         ;; Continue processing other patterns as needed...
         ))

      (when is-test-kmod
        (message "TRACE: Final type before return: %s" type)
        (message "TRACE: Final name: %s" name))

      (when name
        (cons name (list :type (cond
                               (is-comment 'comment)
                               (is-menuconfig 'menuconfig)
                               (t type))
                         :help help
                         :depends depends
                         :select (nreverse select)
                         :default default
                         :range range
                         :choices (nreverse choices)
                         :menuconfig is-menuconfig
                         :comment is-comment
                         :comment-text comment-text))))))

;; Override the function
(fset 'linconf-parse-kconfig-option 'debug-linconf-parse-kconfig-option)

;; Parse the actual lines from the file
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
  (message "Testing with actual lines:")
  (let ((result (linconf-parse-kconfig-option test-lines 'config)))
    (message "Result: %s" result)))