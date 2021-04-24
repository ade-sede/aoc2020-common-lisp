;; TODO
;; 1. Implement function to generate all combinations over N in a
;; given list.
;; 2. Implement map logic, rather than looping once to generate
;; combinations and once to apply.

(defparameter +numbers+ (mapcar #'parse-integer (uiop:read-file-lines "01.input")))

(defun pair-combination (lst)
  "Given a list, return a list of all the element pairs possible

   Example:
   (pair-combination '(1 2 3))
   ((1 3) (1 2) (2 3)"
  (let ((head (car lst))
		(rest (cdr lst)))
	(if (or (not head) (not rest))
		(return-from pair-combination nil))
	(let ((combinations-rest (pair-combination rest))
		  (combinations-current nil)
		  (combinations-total nil))
	  (dolist (tail rest)
		(push (list head tail) combinations-current))
	  (setf combinations-total (append combinations-current combinations-rest))
	  (return-from pair-combination combinations-total))))

(defun trio-combination (lst)
  "Given a list, return a list of all the element trios possible

   Example:
   (pair-combination '(1 2 3))
   ((1 2 3))"
  (let ((head (car lst))
		(rest (cdr lst)))
	(if (or (not head) (not rest))
		(return-from trio-combination nil))
	(let ((combinations-rest (trio-combination rest))
		  (combinations-current nil)
		  (combinations-total nil))
	  (dolist (pairs (pair-combination rest))
		(push (push head pairs) combinations-current))
	  (setf combinations-total (append combinations-current combinations-rest))
	  (return-from trio-combination combinations-total))))

(defun pair-additioning-up-to (n lst)
  (dolist (p lst)
	(when (= (+ (first p) (car (last p))) n)
	  (return-from pair-additioning-up-to p))))

(defun trio-additioning-up-to (n lst)
	(dolist (trio lst)
	  (when (= (+ (first trio) (second trio) (car (last trio))) n)
		(return-from trio-additioning-up-to trio))))


(defun part1 ()
  (apply #'* (pair-additioning-up-to 2020 (pair-combination +numbers+))))

(defun part2 ()
  (apply #'* (trio-additioning-up-to 2020 (trio-combination +numbers+))))

(print (part1))
(print (part2))
