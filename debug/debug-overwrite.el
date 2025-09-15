#!/usr/bin/emacs --script
;;; debug-overwrite.el --- Debug type overwriting issue

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; Override puthash to trace TEST_KMOD overwrites
(defvar original-puthash (symbol-function 'puthash))

(defun traced-puthash (key value table)
  (when (and (string= key "TEST_KMOD") (eq table linconf-kconfig-options))
    (let ((existing (gethash key table))
          (new-type (when (listp value) (plist-get value :type))))
      (message "TRACE: Setting TEST_KMOD in hash table")
      (message "  New type: %s" new-type)
      (when existing
        (message "  Existing type: %s" (plist-get existing :type))
        (message "  Overwriting: %s" (if new-type "YES" "NO")))))
  (funcall original-puthash key value table))

(fset 'puthash 'traced-puthash)

;; Set kernel source path
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/os-build")

;; Load data to trace overwrites
(let ((inhibit-message nil))
  (message "Starting Kconfig loading...")
  (linconf-load-kconfig-data))

;; Check final result
(let ((test-kmod-info (gethash "TEST_KMOD" linconf-kconfig-options)))
  (message "\nFinal TEST_KMOD info:")
  (message "  Type: %s" (plist-get test-kmod-info :type))
  (message "  Depends: %s" (plist-get test-kmod-info :depends)))