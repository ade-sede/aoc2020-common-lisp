(ql:quickload :split-sequence)

;; TODO
;; Try to do the same using loop DSL instead of dolist
;; Some refactoring to increase readability


(defvar +passports+
  (flet ((blank-p (s)
		   (not (find #\space s :test-not #'eql))))
	(flet ((read-passports (lst)
			 (let ((tmp)
				   (accumulator))
			   (dolist (item lst)
				 (if (blank-p item)
					 (progn
					   ;; Once a white line is found
					   ;; Concatenate all lines into one
					   ;; Split the string on whitespace
					   (setf tmp (split-sequence:split-sequence
								  #\space
								  (mapcar #'(lambda (ll)
											  (format nil "~{~s ~}" ll)))))
					   (push tmp accumulator)
					   (setf tmp nil))
					 (push item tmp)))
			   (push tmp accumulator)
			   accumulator)))
	  (read-passports (uiop:read-file-lines "04.input")))))

(print +passports+)

