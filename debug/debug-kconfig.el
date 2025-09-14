(defun debug-kconfig-parsing ()
  "Debug function to test Kconfig parsing"
  (interactive)
  (let ((temp-file (make-temp-file "debug-kconfig")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "config TEST_OPTION\n"
                    "\tbool \"Test option\"\n"
                    "\tdefault y\n"))
          
          (message "File contents:")
          (with-temp-buffer
            (insert-file-contents temp-file)
            (message "%S" (buffer-string)))
          
          (message "Testing individual lines:")
          (let ((lines '("config TEST_OPTION" "\tbool \"Test option\"" "\tdefault y")))
            (let ((option (linconf-parse-kconfig-option lines)))
              (message "Individual parsing result: %S" option)))
          
          (message "Parsing result:")
          (let ((options (linconf-parse-kconfig-file temp-file)))
            (message "Found %d options: %S" (length options) options)))
      (delete-file temp-file))))

(load "/nas/src/linconf-emacs/linconf.el")
(debug-kconfig-parsing)