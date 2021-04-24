(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

;; TODO
;; Remove duplication in valid1 and valid2 when defining variables
;; Maybe try to write a more idiomatic version, less defun and more flet
;; Try to do this without using automatical regexes coming from nowhere ?

(defvar +rules-and-passwords+ (uiop:read-file-lines "02.input"))

(defun parse-rule-and-password (s)
  (cl-ppcre:register-groups-bind
	  (min max key password)
	  ("(\\d*)-(\\d*) (.): (.*)" s)
	(list min max key password)))

(defmacro xor (l r)
  `(let ((left ,l)
		(right ,r))
	(and (or left right) (not (and left right)))))

(defun valid1 (rule-and-password)
  (let ((min (parse-integer (first rule-and-password)))
		(max (parse-integer (second rule-and-password)))
		(key (character (third rule-and-password)))
		(password (fourth rule-and-password)))
	(flet ((in-range (n)
			 (and (<= n max) (<= min n))))
	  (in-range (count-if #'(lambda (c) (char= c key)) password)))))

(defun valid2 (rule-and-password)
  (let ((pos1 (parse-integer (first rule-and-password)))
		(pos2 (parse-integer (second rule-and-password)))
		(key (character (third rule-and-password)))
		(password (fourth rule-and-password)))
	(flet ((key-at-the-right-place (s)
			 (xor (char= key (char s (- pos1 1)))
				  (char= key (char s (- pos2 1))))))
	  (key-at-the-right-place password))))


(defun part1 ()
  (print (count-if #'valid1 (mapcar #'parse-rule-and-password +rules-and-passwords+))))

(defun part2 ()
  (print (count-if #'valid2 (mapcar #'parse-rule-and-password +rules-and-passwords+))))
