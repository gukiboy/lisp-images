
(defun fibonacci-recur
  (start current until)
  (if (>= current until)
      current
      (fibonacci-recur current (+ start current) until)))

(defun fibonacci
  (until)
  (fibonacci-recur 0 1 until))

(fibonacci 99)








