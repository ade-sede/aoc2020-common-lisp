;;; ---  Example  ---
;;; FBFBBFF      RLR
;;; 0101100      101
;;;  row 44      col 5


;; Assuming no foul input
(defun char-to-bit (c)
  (cond ((char= c #\F) 0)
		((char= c #\L) 0)
		((char= c #\B) 1)
		((char= c #\R) 1)))

(defun parse-binary-string (s)
  (apply #'+ 
		 (loop :for i :from 0 :below (length s)
			   :collect (if (= (char-to-bit (aref s i))
							   1)
							(expt 2 i)
							0))))

(defvar +parsed-boarding-passess+
		  (mapcar #'(lambda (boarding-pass)
					  (let ((row (parse-binary-string (reverse (subseq boarding-pass 0 7))))
							(col (parse-binary-string (reverse (subseq boarding-pass 7 10)))))
						(+ col (* 8 row))))
				  (uiop:read-file-lines "05.input")))

;; Part1
(print (reduce #'max +parsed-boarding-passess+))

;; Part2
(print (1+ (car (last (loop :for i
			 :from (reduce #'min +parsed-boarding-passess+)
			   :to (reduce #'max +parsed-boarding-passess+)

			 :for j in (sort +parsed-boarding-passess+ #'<)
			 :until (/= j i)
			 :collect j)))))
