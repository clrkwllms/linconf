#!/usr/bin/env emacs --script
(progn (load-file "kconfig.el") (load-file "linconf.el"))
(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")
(kconfig-load-kconfig-data)
(let ((hz-opts '("HZ_100" "HZ_250" "HZ_300" "HZ_1000")))
  (dolist (opt hz-opts)
    (if (gethash opt kconfig-options)
        (message "✓ %s found: %s" opt (plist-get (gethash opt kconfig-options) :type))
      (message "✗ %s missing" opt))))