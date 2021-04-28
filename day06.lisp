(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :split-sequence)

(defun blank-p (s)
  (not (find #\space s :test-not #'eql)))

(defun split(delimiter-p lst)
  (loop :for beg = (position-if-not delimiter-p lst)
		  :then (position-if-not delimiter-p lst :start (1+ end))
		:for end = (and beg (position-if delimiter-p lst :start beg))
		:when beg :collect (subseq lst beg end)
		  :while end))


;; Part1
(print
 (apply #'+
		(mapcar #'length
				(mapcar #'remove-duplicates
						(mapcar #'(lambda (l)
									(format nil "~{~a~}" l))
								(split #'blank-p (uiop:read-file-lines "06.input")))))))



;; I got kinda lost along this one with all the mapcars 
;; Used part of this answer to help me along
;; https://github.com/atgreen/advent-of-code-2020/blob/main/06.lisp
;; Part2
(print (apply '+
	   (mapcar
		#'(lambda (group)
		  (length (reduce #'intersection group)))
		(mapcar #'(lambda (group)
				  (mapcar #'(lambda (answer)
							(coerce answer 'list))
						  (cl-ppcre:split "\\n" group)))
				(mapcar #'(lambda (l)
							(format nil "~{~a~%~}" l))
						(split #'blank-p (uiop:read-file-lines "06.input")))))))
