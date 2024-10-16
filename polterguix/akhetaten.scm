(define-module (polhuang systems akhetaten)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (polhuang systems base)
  )

(system-config
 #:home
 (home-environment
  (services (service bluetooth-service-type)
            $base-services)))
