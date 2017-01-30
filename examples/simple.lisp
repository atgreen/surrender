(ql:quickload :surrender)

;; The next 4 lines are temporary hacks
(in-package :surrender)
(defvar *SSH-USER* "USER")
(defvar *SSH-PASSWORD* "PASS")
(in-package :cl)

(surrender:with-inventory '("localhost")

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
             "v8-devel"
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
	    "steam" "gnucash" "qt-devel" "mock")))

(surrender:quit)
