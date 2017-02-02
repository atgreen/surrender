;;;; surrender.asd

(cl:in-package #:cl-user)

(asdf:defsystem #:surrender
  :description "The infrastructure programming environment"
  :author "Anthony Green <green@moxielogic.org>"
  :maintainer "Anthony Green <green@moxielogic.org>"
  :license "GPLv3"
  :version "0.1"
  :depends-on (#:cl-azure
	       #:cl-fad
	       #:cl-openstack-client
	       #:cl-template
	       #:ec2
               #:hunchentoot
	       #:ironclad
	       #:md5
	       #:thread-pool
	       #:trivial-shell
	       #:trivial-ssh
	       #:uuid)
  :serial t
  :components ((:file "package")
               (:file "surrender")))

