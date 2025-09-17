#!/usr/bin/env emacs --script
;; Test parsing IP_VS_IPV6 config block directly

(load-file "linconf.el")

(message "=== Direct IP_VS_IPV6 Config Parsing Test ===")

;; Simulate the exact config lines that would be collected
(let ((config-lines '("config	IP_VS_IPV6"
                      "	bool \"IPv6 support for IPVS\""
                      "	depends on IPV6 = y || IP_VS = IPV6"
                      "	select NF_DEFRAG_IPV6"
                      "	help"
                      "	  Add IPv6 support to IPVS."
                      ""
                      "	  Say Y if unsure.")))

  (message "Config lines to parse:")
  (dolist (line config-lines)
    (message "  '%s'" line))

  (message "\nParsing with linconf-parse-kconfig-option:")
  (let ((result (linconf-parse-kconfig-option config-lines 'config)))
    (if result
        (progn
          (message "✓ Successfully parsed:")
          (message "  Name: %s" (car result))
          (message "  Type: %s" (plist-get (cdr result) :type))
          (message "  Help: %s" (plist-get (cdr result) :help)))
      (message "✗ Failed to parse!"))))

(message "\n=== DONE ===")