#!/usr/bin/env emacs --script
;; Test full validation system with conditional parsing fix

(load-file "linconf.el")

(setq linconf-kernel-source-path "/nas/src/RedHat/gitlab/kernel-ark/linus")

(message "=== Full Validation Test with Conditional Parsing Fix ===")

;; Load all Kconfig options
(linconf-load-kconfig-data)

;; Check specifically for IP_VS options in the full system
(let ((ipvs-options '("IP_VS_IPV6" "IP_VS_DEBUG" "IP_VS_TAB_BITS"
                      "IP_VS_PROTO_TCP" "IP_VS_PROTO_UDP" "IP_VS_PROTO_AH_ESP"
                      "IP_VS_PROTO_ESP" "IP_VS_PROTO_AH" "IP_VS_PROTO_SCTP"
                      "IP_VS_RR" "IP_VS_WRR" "IP_VS_LC" "IP_VS_WLC"
                      "IP_VS_FO" "IP_VS_OVF" "IP_VS_LBLC" "IP_VS_LBLCR"
                      "IP_VS_DH" "IP_VS_SH" "IP_VS_MH" "IP_VS_SED"
                      "IP_VS_NQ" "IP_VS_TWOS" "IP_VS_FTP" "IP_VS_NFCT" "IP_VS_PE_SIP")))

  (message "\n=== IP_VS Options Status in Full Kconfig System ===")
  (let ((found-count 0))
    (dolist (opt ipvs-options)
      (if (gethash opt linconf-kconfig-options)
          (progn
            (setq found-count (1+ found-count))
            (message "✓ %s: %s" opt (plist-get (gethash opt linconf-kconfig-options) :type)))
        (message "✗ %s: NOT FOUND" opt)))
    (message "\nIP_VS Summary: %d/%d options found in full system" found-count (length ipvs-options)))

  ;; Also check total option count
  (message "\nTotal Kconfig options loaded: %d" (hash-table-count linconf-kconfig-options)))

(message "\n=== DONE ===")