(package (irclogs (0) (20100409))
  (depends (srfi)
           (wak-riastreams)
           (wak-foof-loop)
           (wak-irregex)
           (wak-fmt)
           (wak-prometheus)
           (spells)
           (xitomatl)
           (sbank))
  (libraries (("scheme" "irclogs") -> "irclogs")
             (("scheme" "irclogs.sls") -> "irclogs.sls")))


;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
