(defvar *ERRORS*)

(defun main ()
  (let* ((args sb-ext:*posix-argv*)
	 (form (rest (read-from-string (format nil "~&(~{~A~^ ~})~%" args)))))
    (print (handler-case (eval form)
	     (error ()
	       (format t "Error! ~&")
	       (setf *ERRORS* form)
	       (sb-ext:save-lisp-and-die "calc-error"))))))

(sb-ext:save-lisp-and-die "calc"
  :executable t
  :toplevel 'main)
