(define-module (baba deployment kratos)
  #:use-module (baba system kratos)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh))

(list (machine
       (operating-system %system/kratos)
       (environment managed-host-environment-type)
       (configuration (machine-ssh-configuration
		       (host-name "148.251.185.84")
		       (system "x86_64-linux")
		       (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMhar4I4D9+e2JaOc1xnDITBLvh9joXRyhTcMjzXmj8Q")
		       (user "root")
		       (port 22)))))
