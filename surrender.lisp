;;; --------------------------------------------------------------------------
;;; surrender.lisp - an infrastructure programming environment
;;;
;;; Copyright (c) 2016 Anthony Green.
;;; 
;;; The above named program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; version 2 as published by the Free Software Foundation.
;;; 
;;; The above named program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this work; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;;; 02110-1301, USA.
;;; --------------------------------------------------------------------------

(cl:in-package #:surrender)

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defun asset-path (file) (merge-pathnames file *base-pathname*))

(defvar *spread* 5)

;;; --------------------------------------------------------------------------
;; Use this until trivial-ssh is fixed.
;; https://github.com/eudoxia0/trivial-ssh/issues/4

(defun my-upload-file (conn local remote)
  (with-open-file (file-stream (namestring local)
                               :direction :input
                               :element-type '(unsigned-byte 8))
    (libssh2:with-scp-output (upload-stream conn
                                            (namestring remote)
                                            (file-length file-stream))
      (uiop:copy-stream-to-stream file-stream upload-stream :element-type '(unsigned-byte 8)))))
;;; --------------------------------------------------------------------------

(defun load-encrypted-1 (filename pathname)
  (let* ((absolute-filename
	  (namestring (merge-pathnames filename pathname)))
	 (gpg-filename
	  (namestring (make-pathname :defaults
				     (concatenate 'string absolute-filename ".gpg"))))
	 (asc-filename
	  (namestring (make-pathname :defaults
				     (concatenate 'string absolute-filename ".asc")))))
    (flet ((decode-load (filename)
	     (let ((source (trivial-shell:shell-command
			    (concatenate 'string "gpg2 --use-agent -o - --decrypt " filename))))
	       (load (make-string-input-stream source)))))
      (cond
	((probe-file absolute-filename)
	 (load absolute-filename))
	((probe-file gpg-filename)
	 (decode-load gpg-filename))
	((probe-file asc-filename)
	 (decode-load asc-filename))
	(t
	 (error "Can't find encrypted file ~A(|.gpg|.asc)~%" filename))))))

(defmacro load-encrypted (filename)
  `(load-encrypted-1 ,filename ,(or *compile-file-truename* *load-truename* *default-pathname-defaults*)))

;;; --------------------------------------------------------------------------

(defclass host ()
  ((hostname :initarg :hostname :initform nil :accessor hostname)
   (ipv4-addr :initarg :ipv4-addr :initform nil :accessor ipv4-addr)
   (ipv6-addr :initarg :ipv6-addr :initform nil :accessor ipv6-addr)
   (ssh-username :initarg :ssh-username :initform nil :accessor ssh-username)
   (ssh-password :initarg :ssh-password :initform nil :accessor ssh-password)))

(defvar *default-ssh-username* nil)
(defvar *default-ssh-password* nil)

(defvar *localhost* (make-instance 'host
				   :hostname "localhost"))

(defmethod get-ssh-username ((host surrender:host))
  (let ((username (ssh-username host)))
    (cond
      (username
       username)
      (*default-ssh-username*
       *default-ssh-username*)
      (t
       (error "No ssh username defined for host ~A, and no *default-ssh-username*~%" host)))))

(defmethod get-ssh-password ((host surrender:host))
  (let ((password (ssh-password host)))
    (cond
      (password
       password)
      (*default-ssh-password*
       *default-ssh-password*)
      (t
       (error "No ssh password defined for host ~A, and no *default-ssh-password*~%" host)))))

;;; --------------------------------------------------------------------------

(defmacro with-inventory (inventory &rest body)
  `(let* ((inventory-list (list ,@inventory))
	  (tpool (thread-pool:make-thread-pool *spread*))
	  (count (length inventory-list))
	  (count-lock (bordeaux-threads:make-lock)))

     ;; Start the thread pool.  Tasks are executed in sequence within
     ;; per-host threads
     (thread-pool:start-pool tpool)
     
     ;; Need to fork and run as many as *spread* allows.
     (mapcar
      
      #'(lambda (surrender/host)
	  (thread-pool:add-to-pool
	   tpool
	   #'(lambda ()
	       (ssh:with-connection
		   (surrender/conn (surrender:hostname surrender/host) (ssh:pass (surrender:get-ssh-username surrender/host)
										 (surrender:get-ssh-password surrender/host)))
		 
		 ;; Generate the target test scripts and run functions
		 (progn
		   ,@body)
		 )
	       (bordeaux-threads:with-lock-held (count-lock)
		 (decf count)))))
      
      inventory-list)
     
     ;; Spin until all tasks are complete across all hosts.
     (loop until (equal count 0)
	do (bordeaux-threads:thread-yield))

     ;; All done.  
     (thread-pool:stop-pool tpool)))

(defun gen-task-id () (format nil "~A" (uuid:make-v4-uuid)))

(defmacro redhat-subscription-register ()
  (let ((task-id (gen-task-id)))
    `(with-open-file (template-file (asset-path "tasks/redhat-subscription-register/test-redhat-subscription-register.clt"))
;       (setf ((gethash ,task-id) action-table)
;	     #'(lambda (host)
;		 ))
       (format analysis-script
	       (let ((template (make-string (file-length template-file))))
		 (read-sequence template template-file)
		 (funcall (cl-template:compile-template template) (list :task-id ,task-id)))))))

(defmacro copy-file (&rest fileinfo)
  (let ((md5 (md5:md5sum-file (car fileinfo))))
    ))

(defmacro packages (&rest package-list)
  (let ((task-id (gen-task-id))
	(present-list nil)
	(absent-list nil)
	(update-list nil))
    (loop for package-form in package-list do
	 (cond
	   ((eq (car package-form) :present)
	    (setq present-list (cdr package-form)))
	   ((eq (car package-form) :update)
	    (setq update-list (cdr package-form)))
	   ((eq (car package-form) :absent)
	    (setq absent-list (cdr package-form)))))
    `(progn
       (with-open-file (run-script #p"/tmp/analysis"
				   :direction :output
				   :if-exists :supersede
				   :external-format '(:utf-8 :eol-style :crlf))
	 (with-open-file (template-file (asset-path "tasks/packages/packages.clt"))
	   (format run-script
		   (let ((template (make-string (file-length template-file))))
		     (read-sequence template template-file)
		     (funcall (cl-template:compile-template template) (list :task-id ,task-id
									    :present-list ',present-list
									    :update-list ',update-list
									    :absent-list ',absent-list))))))
       (my-upload-file surrender/conn
		       #p"/tmp/analysis"
		       #p"/tmp/analysis-copy")
       (ssh:with-command (surrender/conn iostream "chmod +x /tmp/analysis-copy && /tmp/analysis-copy 2>&1 | tee /tmp/analysis-copy.output")
	 (when iostream
	   (loop for line = (read-line iostream nil)
	      while line do
		(format t "~A~%" line)))))))

(defmacro services (&rest service-list)
  (let ((task-id (gen-task-id))
	(enabled-list nil)
	(disabled-list nil)
	(stopped-list nil)
	(started-list nil))
    (loop for service-form in service-list do
	 (cond
	   ((eq (car service-form) :enabled)
	    (setq enabled-list (cdr service-form)))
	   ((eq (car service-form) :disabled)
	    (setq disabled-list (cdr service-form)))
	   ((eq (car service-form) :stopped)
	    (setq stopped-list (cdr service-form)))
	   ((eq (car service-form) :started)
	    (setq started-list (cdr service-form)))))
    `(progn
       (with-open-file (run-script #p"/tmp/analysis"
				   :direction :output
				   :if-exists :supersede
				   :external-format '(:utf-8 :eol-style :crlf))
	 (with-open-file (template-file (asset-path "tasks/services/services.clt"))
	   (format run-script
		   (let ((template (make-string (file-length template-file))))
		     (read-sequence template template-file)
		     (funcall (cl-template:compile-template template) (list :task-id ,task-id
									    :enabled-list ',enabled-list
									    :disabled-list ',disabled-list
									    :started-list ',started-list
									    :stopped-list ',stopped-list))))))
       (my-upload-file surrender/conn
		       #p"/tmp/analysis"
		       #p"/tmp/analysis-copy")
       (ssh:with-command (surrender/conn iostream "chmod +x /tmp/analysis-copy && /tmp/analysis-copy 2>&1 | tee /tmp/analysis-copy.output")
	 (when iostream
	   (loop for line = (read-line iostream nil)
	      while line do
		(format t "~A~%" line)))))))

(defun quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:exit :code code)
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from <hebi...@math.uni.wroc.pl>
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code)))
