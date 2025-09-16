#!/usr/bin/emacs --script
;;; debug-tmpfs-regex.el --- Test TMPFS_INODE64 against type regex

(load (expand-file-name "../linconf.el" (file-name-directory load-file-name)))

;; The type regex from the parser
(defvar type-regex "^[ \t]+\\(bool\\|tristate\\|string\\|int\\|hex\\)\\b\\(?: \"\\([^\"]*\\)\"\\)?")

;; Test lines from TMPFS_INODE64
(let ((test-lines '("config TMPFS_INODE64"
                    "\tbool \"Use 64-bit ino_t by default in tmpfs\""
                    "\tdepends on TMPFS && 64BIT"
                    "\tdefault n"
                    "\thelp"
                    "\t  tmpfs has historically used only inode numbers as wide as an unsigned"
                    "\t  int. In some cases this can cause wraparound, potentially resulting"
                    "\t  in multiple files with the same inode number on a single device. This"
                    "\t  option makes tmpfs use the full width of ino_t by default, without"
                    "\t  needing to specify the inode64 option when mounting.")))
  (message "Checking each line against type regex:")
  (dolist (line test-lines)
    (when (string-match type-regex line)
      (message "MATCH: Line '%s' -> type: %s" line (match-string 1 line)))))