(load "/nas/src/linconf-emacs/linconf.el")

;; Debug the specific line causing issues
(let ((test-line "config SCHED_RT"))
  (message "Test line: '%s'" test-line)
  (message "Length: %d" (length test-line))
  (message "Characters: %s" (mapcar (lambda (c) (format "%c (%d)" c c)) 
                                    (string-to-list test-line)))
  
  ;; Test various regex patterns
  (message "Match '^config[ \\t]+\\\\([A-Z0-9_]+\\\\)': %s" 
           (if (string-match "^config[ \t]+\\([A-Z0-9_]+\\)" test-line)
               (format "YES, group 1: '%s'" (match-string 1 test-line))
             "NO"))
  
  (message "Match '^config\\\\s-+\\\\([A-Z0-9_]+\\\\)': %s" 
           (if (string-match "^config\\s-+\\([A-Z0-9_]+\\)" test-line)
               (format "YES, group 1: '%s'" (match-string 1 test-line))
             "NO"))
  
  (message "Match '^config\\\\s-*\\\\([A-Z0-9_]+\\\\)': %s" 
           (if (string-match "^config\\s-*\\([A-Z0-9_]+\\)" test-line)
               (format "YES, group 1: '%s'" (match-string 1 test-line))
             "NO")))