;;; test-source-paths.el --- Test source file path tracking functionality -*- lexical-binding: t; -*-

;; Test source file path tracking in LinConf

(require 'ert)
(load-file "../linconf.el")

(ert-deftest test-linconf-relative-source-path ()
  "Test source path display formatting."
  (let ((linconf-kernel-source-path "/usr/src/linux-headers-6.1.0"))
    ;; Test absolute path within kernel source
    (should (string= "arch/x86/Kconfig"
                     (linconf-relative-source-path "/usr/src/linux-headers-6.1.0/arch/x86/Kconfig")))

    ;; Test absolute path outside kernel source
    (should (string= "other.config"
                     (linconf-relative-source-path "/some/other/path/other.config")))

    ;; Test relative path (should return as-is)
    (should (string= "drivers/net/Kconfig"
                     (linconf-relative-source-path "drivers/net/Kconfig")))

    ;; Test filename only
    (should (string= "Kconfig"
                     (linconf-relative-source-path "Kconfig")))))

(ert-deftest test-source-path-storage ()
  "Test that source paths are stored in option metadata."
  (let ((linconf-kconfig-options (make-hash-table :test 'equal)))
    ;; Simulate parsing an option with source file
    (let ((option-plist '(:type bool :help "Test option" :source-file "arch/x86/Kconfig")))
      (puthash "TEST_OPTION" option-plist linconf-kconfig-options)

      ;; Verify source file is stored
      (let ((stored-plist (gethash "TEST_OPTION" linconf-kconfig-options)))
        (should stored-plist)
        (should (string= "arch/x86/Kconfig" (plist-get stored-plist :source-file)))))))

(ert-deftest test-show-source-file-function ()
  "Test the interactive source file display function exists."
  (should (fboundp 'linconf-show-source-file))
  (should (commandp 'linconf-show-source-file)))

(defun run-source-path-tests ()
  "Run all source path tests."
  (interactive)
  (ert-run-tests-batch "test-.*source.*"))

(provide 'test-source-paths)
;;; test-source-paths.el ends here