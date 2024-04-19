(load "~/quicklisp/setup.lisp")

(ql:quickload :hunchentoot)

(defvar *ERRORS*)

(defun print-thread-info ()
  (let* ((curr-thread (bt:current-thread))
	 (curr-thread-name (bt:thread-name curr-thread))
	 (all-threads (bt:all-threads))
;	 (all-but-current (filter! (lambda (thread) (not (= thread curr-thread))) all-threads))
	 )
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads)
    (loop for x in (bt:all-threads)
	do   (print x))
 ;   (format t "All but current threads:~% ~{~a~%~}~%" all-but-current)
    )
  nil)

(defun print-and-format (name)
  (error "Could not finish request.") ;; Throwing error
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (print-thread-info)
  ;; (handler-case (print-and-format name)
  ;;   (error ()
  ;;     (setf *ERRORS* '(print-and-format name))
  ;;     (sb-ext:save-lisp-and-die "calc-error")))
  )  ;; create image on error

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4243))
