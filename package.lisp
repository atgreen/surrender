;;;; package.lisp

(defpackage #:surrender
  (:use #:cl)
  (:export
   #:*default-ssh-password*
   #:*default-ssh-username*
   #:*localhost*
   #:copy-file
   #:hostname
   #:get-ssh-password
   #:get-ssh-username
   #:host
   #:load-encrypted
   #:packages
   #:quit
   #:redhat-subscription-register
   #:with-inventory))
