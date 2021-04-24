(ql:quickload :cl-ppcre)

(defvar +rules-and-passwords+ (uiop:read-file-lines "02.input"))

(defun parse-rule-and-password (s)
  (cl-ppcre:register-groups-bind
	  (min max key password)
	  ("(\\d*)-(\\d*) (.): (.*)" s)
	(list min max key password)))



(defun part1 ()
(print (count-if #'(lambda (rule-and-password)
			  (let ((min (parse-integer (first rule-and-password)))
					(max (parse-integer (second rule-and-password)))
					(key (character (third rule-and-password)))
					(password (fourth rule-and-password)))
				(flet ((in-range (n)
						 (and (<= n max) (<= min n))))
				  (in-range (count-if #'(lambda (c) (char= c key)) password)))))
			  (mapcar #'parse-rule-and-password +rules-and-passwords+))))
