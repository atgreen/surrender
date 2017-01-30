(ql:quickload :surrender)

(surrender:with-inventory '("localhost")

  ;; Manage the packages we want on this host.
  (surrender:packages
   (:present "gcc" "sbcl" "emacs" "make" "patch"
	     "rear" "git" "dejagnu" "subversion" "cockpit")
   (:absent "php" "clisp" "vim" "jenkins" "nodejs"
	    "steam" "gnucash" "gcc-c++"))

  ;; Manage services we need on this host.
  (surrender:services
   (:enabled "httpd" "nfs")))

  


