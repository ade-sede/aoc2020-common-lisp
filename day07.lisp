;;; From a quick glance at my sample input, it seems a bag can be
;;; referenced before it is defined.
;;; It can also be referenced after it's been defined, therefore
;;; parsing from the end is not enough and symbols will need to be
;;; resolved as we go.
;;;
;;; Assuming there are no traps (which is usually the case in AOC):
;;; - All objects referenced are defined somewhere
;;; - There is only one definition per object
;;; - No self referencing objects
;;;
;;; I will parse each rule to a list:
;;; ("bag-name" ((n "dim violet") (y "light purple")))
;;;
;;; There is no need to actually generate the corresponding tree
;;; Knowing the rules is enough.

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :alexandria)


(defun parse-definition (s)
  (cl-ppcre:register-groups-bind
	  (bag content)
	  ("(\\w* \\w*).*contain (.*)." s)
	(list bag 
		  (if (string/= content "no other bags")
			  (mapcar #'(lambda (bag-reference)
						  (cl-ppcre:register-groups-bind
							  (count bag)
							  ("\\s*(\\d*) (\\w* \\w*).*" bag-reference)
							(list (parse-integer count) bag)
							))
					  (split-sequence:split-sequence #\, content))
			  (list nil)))))

(defvar +luggages-definitions+
  (mapcar #'(lambda (definition)
			  (parse-definition definition))
		  (uiop:read-file-lines "07.input")))

(print +luggages-definitions+)

(defun get-bag-definition (bag-name)
  (find-if #'(lambda (definition)
			   (string= bag-name (first definition)))
		   +luggages-definitions+))

(defun bag-contains (bag target-bag)
  (if (not (first (second bag)))
	  0
	  (apply #'+
			 (loop :for inner-bag :in (second bag)
				   :collect (if (string= (second inner-bag) target-bag)
								(* 1 (first inner-bag))
								(* (first inner-bag) (bag-contains (get-bag-definition (second inner-bag)) target-bag)))))))

(defun bag-depth (bag)
  (if (not (first (second bag)))
	  0
	  (apply #'+
			 (loop :for inner-bag :in (second bag)
				   :collect (* (first inner-bag) (1+ (bag-depth (get-bag-definition (second inner-bag)))))))))

;; Part 1
(print
 (count-if #'(lambda (bag)
			   (< 0 (bag-contains bag "shiny gold")))
		   +luggages-definitions+))

;; Part 2
(print (bag-depth (get-bag-definition "shiny gold")))
