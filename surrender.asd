;;;; surrender.asd

(cl:in-package #:cl-user)

(asdf:defsystem #:surrender
  :description "The infrastructure programming environment"
  :author "Anthony Green <green@moxielogic.org>"
  :maintainer "Anthony Green <green@moxielogic.org>"
  :license "GPLv3"
  :version "0.1"
  :depends-on (#:trivial-ssh
	       #:cl-template
	       #:thread-pool
	       #:uuid
	       #:md5
	       #:cl-fad
	       #:ec2
	       #:cl-azure
	       #:cl-openstack-client
               #:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "surrender")))

