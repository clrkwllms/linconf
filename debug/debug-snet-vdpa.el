#!/usr/bin/env emacs --script
;; Debug SNET_VDPA parsing issue with leading whitespace

(load-file "linconf.el")

(message "=== SNET_VDPA Config Parsing Test ===")

;; Test the exact lines from drivers/vdpa/Kconfig
(let ((config-lines '(" config SNET_VDPA"
                      "\ttristate \"SolidRun's vDPA driver for SolidNET\""
                      "\tdepends on PCI_MSI && PCI_IOV && (HWMON || HWMON=n)"
                      ""
                      "\t# This driver MAY create a HWMON device."
                      "\thelp"
                      "\t  vDPA driver for SolidRun's network adapter.")))

  (message "Testing config lines with leading space:")
  (dolist (line config-lines)
    (message "  '%s'" line))

  (message "\nTesting current regex pattern:")
  (let ((current-regex "^\\(menu\\)?config[ \t]+\\([A-Z0-9_]+\\)")
        (proposed-regex "^[ \t]*\\(menu\\)?config[ \t]+\\([A-Z0-9_]+\\)"))

    (message "Current regex: %s" current-regex)
    (message "Config line: '%s'" (car config-lines))
    (if (string-match current-regex (car config-lines))
        (message "✓ Current regex matches: %s" (match-string 2 (car config-lines)))
      (message "✗ Current regex does NOT match"))

    (message "\nProposed regex: %s" proposed-regex)
    (if (string-match proposed-regex (car config-lines))
        (message "✓ Proposed regex matches: %s" (match-string 2 (car config-lines)))
      (message "✗ Proposed regex does NOT match")))

  (message "\nTesting linconf-parse-kconfig-option:")
  (let ((result (linconf-parse-kconfig-option config-lines 'config)))
    (if result
        (progn
          (message "✓ Successfully parsed:")
          (message "  Name: %s" (car result))
          (message "  Type: %s" (plist-get (cdr result) :type)))
      (message "✗ Failed to parse!"))))

(message "\n=== DONE ===")