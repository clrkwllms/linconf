#!/usr/bin/env emacs --script
;; Test if the IP_VS conditional parsing fix works

(progn (load-file "kconfig.el") (load-file "linconf.el"))

(message "=== IP_VS Conditional Parsing Test ===")

;; Test direct parsing of IP_VS Kconfig file
(let ((ipvs-file "/nas/src/RedHat/gitlab/kernel-ark/linus/net/netfilter/ipvs/Kconfig"))
  (message "Parsing file: %s" ipvs-file)
  (let ((options (kconfig-parse-kconfig-file ipvs-file)))
    (message "Total options parsed: %d" (length options))

    ;; Check specifically for missing IP_VS options from the report
    (let ((missing-options '("IP_VS_IPV6" "IP_VS_DEBUG" "IP_VS_TAB_BITS"
                             "IP_VS_PROTO_TCP" "IP_VS_PROTO_UDP" "IP_VS_PROTO_AH_ESP"
                             "IP_VS_PROTO_ESP" "IP_VS_PROTO_AH" "IP_VS_PROTO_SCTP"
                             "IP_VS_RR" "IP_VS_WRR" "IP_VS_LC" "IP_VS_WLC"
                             "IP_VS_FO" "IP_VS_OVF" "IP_VS_LBLC" "IP_VS_LBLCR"
                             "IP_VS_DH" "IP_VS_SH" "IP_VS_MH" "IP_VS_SED"
                             "IP_VS_NQ" "IP_VS_TWOS" "IP_VS_FTP" "IP_VS_NFCT" "IP_VS_PE_SIP")))
      (message "\n=== Missing IP_VS Options Status ===")
      (let ((found-count 0))
        (dolist (opt missing-options)
          (let ((found (assoc opt options)))
            (if found
                (progn
                  (setq found-count (1+ found-count))
                  (message "✓ %s: %s" opt (plist-get (cdr found) :type)))
              (message "✗ %s: NOT FOUND" opt))))
        (message "\nSummary: %d/%d options found" found-count (length missing-options))))))

(message "\n=== DONE ===")