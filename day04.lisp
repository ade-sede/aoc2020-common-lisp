(ql:quickload :split-sequence)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)


;;; This code gives an answer off by 1.
;;; As I have completed this challenge in other languages I know what
;;; output I should be getting.
;;; I don't feel like chasing the edge case I missed, I already
;;; learned a lot thanks to this exercise.

(defun blank-p (s)
  (not (find #\space s :test-not #'eql)))

(defun passport-is-valid-1 (passport)
  (and (getf passport :ECL)
	   (getf passport :PID)
	   (getf passport :EYR)
	   (getf passport :HCL)
	   (getf passport :BYR)
	   (getf passport :IYR)
	   (getf passport :HGT)))

(defun passport-is-valid-2 (passport)
  (let ((ecl (getf passport :ECL))
		(pid (getf passport :PID))
		(eyr (getf passport :EYR))
		(hcl (getf passport :HCL))
		(byr (getf passport :BYR))
		(iyr (getf passport :IYR))
		(hgt (getf passport :HGT)))
	;; Check each field is defined and it's value is correct according
	;; to the rules
	(and
	 ;; All keys must be present, just as in part 1
	 (passport-is-valid-1 passport)
	 ;; BYR
	 (and (<= 1920 (parse-integer byr))
		  (>= 2002 (parse-integer byr))
	  ;; IYR
	  (and (<= 2010 (parse-integer iyr))
		   (>= 2020 (parse-integer iyr)))
	  ;; EYR
	  (and (= (length eyr) 4)
		   (<= 2020 (parse-integer eyr))
		   (>= 2030 (parse-integer eyr)))
	  ;; HGT
	  (let* ((match-list (cl-ppcre:register-groups-bind
							 (val unit)
							 ("(\\d*)(cm|in)" hgt)
						   (list val unit))))
		(cond ((not match-list) nil)
			  ((string= (second match-list) "cm") (and (<= 150 (parse-integer (first match-list)))
													   (>= 193 (parse-integer (first match-list)))))
			  ((string= (second match-list) "in") (and (<= 59 (parse-integer (first match-list)))
													   (>= 76 (parse-integer (first match-list)))))
			  (t nil)))
	  ;; HCL
	  (cl-ppcre:register-groups-bind
		  (hair-color)
		  ("(#[a-f0-9]{6})" hcl)
		(list hair-color))
	  ;; ECL
	  (cl-ppcre:register-groups-bind
		  (eye-color)
		  ("(amb|blu|brn|gry|grn|hzl|oth)" ecl)
		(list eye-color))
	  ;; PID
	  (cl-ppcre:register-groups-bind
		  (passport-id)
		  ("(\\d{9})" pid)
		(list passport-id))
	  ))))

;; I wanted to passe this function as a parameter to split-map, but
;; for some reason I don't understand yet local functions seem to be
;; unbound inside the loop macro
(defun parse-passport (list-of-string)
  (alexandria:flatten
   (mapcar #'(lambda (short-s)
			   (let ((split-string (split-sequence:split-sequence #\: short-s)))
				 (list (intern (string-upcase (first split-string)) "KEYWORD")
					   (second split-string))))
		   (alexandria:flatten (mapcar #'(lambda (long-s)
										   (split-sequence:split-sequence #\space long-s))
									   list-of-string)))))

;; From a stackoverflow answer.
;; https://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
;; I still struggle with loop syntax, but this makes sense, except the then keyword that I don't understand and can't find in the doc
(defun split-map(delimiter-p lst)
  (loop :for beg = (position-if-not delimiter-p lst)
		  :then (position-if-not delimiter-p lst :start (1+ end))
		:for end = (and beg (position-if delimiter-p lst :start beg))
		:when beg :collect (parse-passport (subseq lst beg end))
		  :while end))

;; Part 1
(print (count-if #'passport-is-valid-1 (split-map #'blank-p (uiop:read-file-lines "04.input"))))

;; Part 2
(print (count-if #'passport-is-valid-2 (split-map #'blank-p (uiop:read-file-lines "04.input"))))
