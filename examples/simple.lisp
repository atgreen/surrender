(ql:quickload :surrender)

;; Define credentials, which may just include *default-ssh-username*
;; and *default-ssh-password*.
(surrender:load-encrypted "secret.lisp")

(surrender:with-inventory (surrender:*localhost*)

  ;; Manage the packages we want on this host.
  (surrender:packages

   (:present "ant"
             "autoconf"
             "automake"
             "bison"
             "boost-devel"
             "ccache"
             "createrepo"
             "cvs"
             "dejagnu"
	     "emacs"
	     "emacs-ledger"
	     "emacs-bbdb"
             "flex"
	     "gcc"
	     "gcc-c++"
             "gdb"
             "gettext"
             "git"
             "gmp-devel"
             "gtkwave"
             "iverilog"
             "libidn-devel"
             "libtool"
             "make"
             "mlocate"
	     "nfs-utils"
             "patch"
             "pcre-devel"
             "python-devel"
             "readline-devel"
             "rpm-build"
             "sbcl"
             "sqlite-devel"
             "subversion"
             "tcplay"
             "texinfo"
	     "tuned"
             "valgrind"
             "xorg-x11-fonts-100dpi"
             "xorg-x11-fonts-ISO8859-1-100dpi"
             "xorg-x11-fonts-ISO8859-14-100dpi"
             "xorg-x11-fonts-ISO8859-2-100dpi"
             "xorg-x11-fonts-ISO8859-9-100dpi"
             "xorg-x11-server-utils"
             "xorg-x11-xauth"
             "zlib-devel")

   (:absent "php" "clisp" "vim" "jenkins" "nodejs" "httpd"
	    "steam" "gnucash" "qt-devel" "mock" "v8"))

  (surrender:services
   (:disabled "nfs")
   (:stopped "nfs")
   (:started "crashplan" "tuned")
   (:enabled "crashplan" "tuned")))

(surrender:quit)
