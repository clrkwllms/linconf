;;; test-dependency-validation.el --- Tests for LinConf dependency validation -*- lexical-binding: t -*-

(require 'ert)
(load-file "linconf.el")

(defun linconf-setup-dependency-test-data ()
  "Set up test data for dependency validation tests."
  (clrhash linconf-kconfig-options)
  (clrhash linconf-config-values)

  (puthash "X86" '(:type bool :help "x86 architecture") linconf-kconfig-options)
  (puthash "X86_64" '(:type bool :depends "X86" :help "x86_64 support") linconf-kconfig-options)
  (puthash "PCI" '(:type bool :depends "X86" :help "PCI support") linconf-kconfig-options)
  (puthash "USB" '(:type tristate :depends "PCI" :help "USB support") linconf-kconfig-options)
  (puthash "USB_STORAGE" '(:type tristate :depends "USB" :help "USB mass storage") linconf-kconfig-options)
  (puthash "NETWORKING" '(:type bool :help "Networking support") linconf-kconfig-options)
  (puthash "NET_ETHERNET" '(:type bool :depends "NETWORKING" :help "Ethernet support") linconf-kconfig-options)
  (puthash "COMPLEX_DEP" '(:type bool :depends "X86 && (PCI || NETWORKING)" :help "Complex dependency") linconf-kconfig-options)

  (linconf-set-config-value "X86" t)
  (linconf-set-config-value "PCI" t)
  (linconf-set-config-value "NETWORKING" nil))

(ert-deftest test-dependency-expression-evaluator ()
  "Test enhanced dependency expression evaluator."
  (linconf-setup-dependency-test-data)

  (should (eq (linconf-evaluate-condition "X86") t))
  (should (eq (linconf-evaluate-condition "!NETWORKING") t))
  (should (eq (linconf-evaluate-condition "X86 && PCI") t))
  (should (eq (linconf-evaluate-condition "X86 && NETWORKING") nil))
  (should (eq (linconf-evaluate-condition "X86 || NETWORKING") t))
  (should (eq (linconf-evaluate-condition "(X86 && PCI) || NETWORKING") t))
  (should (eq (linconf-evaluate-condition "(X86 && NETWORKING) && PCI") nil))
  (should (eq (linconf-evaluate-condition "!(X86 && PCI)") nil)))

(ert-deftest test-dependency-validation-satisfied ()
  "Test dependency validation for satisfied dependencies."
  (linconf-setup-dependency-test-data)

  (let ((result (linconf-validate-dependencies "X86_64")))
    (should (eq (car result) t))
    (should (null (cdr result))))

  (let ((result (linconf-validate-dependencies "PCI")))
    (should (eq (car result) t))
    (should (null (cdr result)))))

(ert-deftest test-dependency-validation-unsatisfied ()
  "Test dependency validation for unsatisfied dependencies."
  (linconf-setup-dependency-test-data)

  (let ((result (linconf-validate-dependencies "NET_ETHERNET")))
    (should (eq (car result) nil))
    (should (string-match "Dependencies not satisfied" (cdr result)))))

(ert-deftest test-complex-dependency-validation ()
  "Test validation of complex dependency expressions."
  (linconf-setup-dependency-test-data)

  (let ((result (linconf-validate-dependencies "COMPLEX_DEP")))
    (should (eq (car result) t))
    (should (null (cdr result)))))

(ert-deftest test-circular-dependency-detection ()
  "Test circular dependency detection."
  (clrhash linconf-kconfig-options)

  (puthash "OPT_A" '(:type bool :depends "OPT_B" :help "Option A") linconf-kconfig-options)
  (puthash "OPT_B" '(:type bool :depends "OPT_C" :help "Option B") linconf-kconfig-options)
  (puthash "OPT_C" '(:type bool :depends "OPT_A" :help "Option C") linconf-kconfig-options)

  (let ((circular-deps (linconf-detect-circular-dependencies)))
    (should (> (length circular-deps) 0))
    (should (member "OPT_A" (mapcar #'car circular-deps)))))

(ert-deftest test-dependency-chain-extraction ()
  "Test extraction of options from dependency expressions."
  (let ((options (linconf-extract-options-from-condition "X86 && (PCI || USB)")))
    (should (member "X86" options))
    (should (member "PCI" options))
    (should (member "USB" options))
    (should (= (length options) 3))))

(ert-deftest test-unsatisfied-dependencies-detection ()
  "Test detection of unsatisfied dependencies."
  (linconf-setup-dependency-test-data)

  (let ((unsatisfied (linconf-get-unsatisfied-dependencies "NET_ETHERNET")))
    (should (= (length unsatisfied) 1))
    (should (string= (car unsatisfied) "NETWORKING")))

  (let ((satisfied (linconf-get-unsatisfied-dependencies "PCI")))
    (should (null satisfied))))

(ert-deftest test-dependency-path-finding ()
  "Test finding dependency paths between options."
  (linconf-setup-dependency-test-data)

  (let ((path (linconf-find-dependency-path "USB_STORAGE" "X86")))
    (should (not (null path)))
    (should (member "USB_STORAGE" path))
    (should (member "USB" path))
    (should (member "PCI" path))
    (should (member "X86" path))))

(ert-deftest test-three-valued-logic ()
  "Test three-valued logic operations (t, nil, unknown)."
  (should (eq (linconf-logical-or t 'unknown) t))
  (should (eq (linconf-logical-or nil 'unknown) 'unknown))
  (should (eq (linconf-logical-and t 'unknown) 'unknown))
  (should (eq (linconf-logical-and nil 'unknown) nil))
  (should (eq (linconf-logical-not 'unknown) 'unknown)))

(defun run-dependency-validation-tests ()
  "Run all dependency validation tests and report results."
  (interactive)
  (let ((test-list '(test-dependency-expression-evaluator
                     test-dependency-validation-satisfied
                     test-dependency-validation-unsatisfied
                     test-complex-dependency-validation
                     test-circular-dependency-detection
                     test-dependency-chain-extraction
                     test-unsatisfied-dependencies-detection
                     test-dependency-path-finding
                     test-three-valued-logic))
        (passed 0)
        (total 0))
    (setq total (length test-list))
    (dolist (test test-list)
      (condition-case err
          (progn
            (ert-run-test (ert-get-test test))
            (setq passed (1+ passed)))
        (error
         (message "FAILED: %s - %s" test (error-message-string err)))))
    (message "Dependency validation tests complete: %d/%d passed" passed total)))

(when noninteractive
  (run-dependency-validation-tests))

;;; test-dependency-validation.el ends here