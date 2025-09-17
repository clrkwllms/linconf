#!/usr/bin/env emacs --script
;; Debug what's actually being parsed from IP_VS Kconfig

(load-file "linconf.el")

(let ((ipvs-file "/nas/src/RedHat/gitlab/kernel-ark/linus/net/netfilter/ipvs/Kconfig"))
  (message "=== All parsed options from IP_VS Kconfig ===")
  (let ((options (linconf-parse-kconfig-file ipvs-file)))
    (message "Total options: %d" (length options))
    (dolist (option options)
      (let ((name (car option))
            (props (cdr option)))
        (message "  %s: type=%s" name (plist-get props :type))))))