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

(defmacro with-inventory (inventory &rest body)
  `(let ((tpool (thread-pool:make-thread-pool *spread*))
	 (count (length ,inventory))
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
		   (surrender/conn surrender/host (ssh:pass *SSH-USER* *SSH-PASSWORD*))
		 
		 ;; Generate the target test scripts and run functions
		 (progn
		   ,@body)
		 )
	       (bordeaux-threads:with-lock-held (count-lock)
		 (decf count)))))
      
      ,inventory)
     
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

