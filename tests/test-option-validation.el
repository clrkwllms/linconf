;;; test-option-validation.el --- Test Option Type Validation -*- lexical-binding: t; -*-

;; Test file for the Option Type Validation implementation

(progn (load-file "kconfig.el") (load-file "linconf.el"))

(defun test-validation-setup ()
  "Setup test environment with some sample Kconfig options."
  (clrhash kconfig-options)

  ;; Add test options
  (puthash "TEST_BOOL" '(:type bool :help "Test boolean option") kconfig-options)
  (puthash "TEST_TRISTATE" '(:type tristate :help "Test tristate option") kconfig-options)
  (puthash "TEST_STRING" '(:type string :help "Test string option") kconfig-options)
  (puthash "TEST_INT" '(:type int :range (0 . 100) :help "Test integer option") kconfig-options)
  (puthash "TEST_HEX" '(:type hex :range (#x10 . #xFF) :help "Test hex option") kconfig-options)
  (puthash "TEST_UNKNOWN" '(:type unknown :help "Test unknown type") kconfig-options))

(defun run-validation-tests ()
  "Run comprehensive validation tests."
  (test-validation-setup)

  (let ((test-results '())
        (failed-tests 0))

    ;; Test bool validation
    (let ((result (kconfig-validate-option-value "TEST_BOOL" "y")))
      (push (list "bool y" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_BOOL" "n")))
      (push (list "bool n" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_BOOL" "m")))
      (push (list "bool m (should fail)" (not (car result)) (cdr result)) test-results)
      (unless (not (car result)) (setq failed-tests (1+ failed-tests))))

    ;; Test tristate validation
    (let ((result (kconfig-validate-option-value "TEST_TRISTATE" "y")))
      (push (list "tristate y" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_TRISTATE" "m")))
      (push (list "tristate m" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_TRISTATE" "invalid")))
      (push (list "tristate invalid (should fail)" (not (car result)) (cdr result)) test-results)
      (unless (not (car result)) (setq failed-tests (1+ failed-tests))))

    ;; Test string validation
    (let ((result (kconfig-validate-option-value "TEST_STRING" "\"valid string\"")))
      (push (list "string quoted" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_STRING" "unquoted")))
      (push (list "string unquoted" (car result) (cdr result)) test-results))

    ;; Test int validation
    (let ((result (kconfig-validate-option-value "TEST_INT" "50")))
      (push (list "int in range" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_INT" "150")))
      (push (list "int out of range (should fail)" (not (car result)) (cdr result)) test-results)
      (unless (not (car result)) (setq failed-tests (1+ failed-tests))))

    (let ((result (kconfig-validate-option-value "TEST_INT" "abc")))
      (push (list "int non-numeric (should fail)" (not (car result)) (cdr result)) test-results)
      (unless (not (car result)) (setq failed-tests (1+ failed-tests))))

    ;; Test hex validation
    (let ((result (kconfig-validate-option-value "TEST_HEX" "0x50")))
      (push (list "hex in range" (car result) (cdr result)) test-results))

    (let ((result (kconfig-validate-option-value "TEST_HEX" "0x200")))
      (push (list "hex out of range (should fail)" (not (car result)) (cdr result)) test-results)
      (unless (not (car result)) (setq failed-tests (1+ failed-tests))))

    (let ((result (kconfig-validate-option-value "TEST_HEX" "xyz")))
      (push (list "hex invalid format (should fail)" (not (car result)) (cdr result)) test-results)
      (unless (not (car result)) (setq failed-tests (1+ failed-tests))))

    ;; Test unknown option
    (let ((result (kconfig-validate-option-value "NONEXISTENT" "y")))
      (push (list "unknown option (should warn)" (car result) (cdr result)) test-results))

    ;; Print results
    (with-current-buffer (get-buffer-create "*Validation Test Results*")
      (erase-buffer)
      (insert "Option Type Validation Test Results\n")
      (insert "====================================\n\n")
      (insert (format "Total tests: %d\n" (length test-results)))
      (insert (format "Failed tests: %d\n\n" failed-tests))

      (dolist (result (reverse test-results))
        (let ((test-name (car result))
              (passed (cadr result))
              (message (caddr result)))
          (insert (format "%s %s: %s\n"
                         (if passed "✓" "✗")
                         test-name
                         (or message "OK")))))

      (goto-char (point-min)))

    (display-buffer "*Validation Test Results*")
    (message "Validation tests complete: %d/%d passed"
             (- (length test-results) failed-tests)
             (length test-results))))

;; Run the tests
(run-validation-tests)

;;; test-option-validation.el ends here