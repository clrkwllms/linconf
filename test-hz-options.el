#!/usr/bin/env emacs --script
(load-file "linconf.el")
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")
(linconf-load-kconfig-data)
(let ((hz-opts '("HZ_100" "HZ_250" "HZ_300" "HZ_1000")))
  (dolist (opt hz-opts)
    (if (gethash opt linconf-kconfig-options)
        (message "✓ %s found: %s" opt (plist-get (gethash opt linconf-kconfig-options) :type))
      (message "✗ %s missing" opt))))