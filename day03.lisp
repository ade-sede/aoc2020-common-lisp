(defvar +map+ (uiop:read-file-lines "03.input"))
(defvar +len+ (length (first +map+)))

(defun solve (x map x-increment y-increment)
  (if (not (car map))
	  0
	  (+ (if (char= #\# (char (car map) x)) 1 0)
		 (solve (mod (+ x x-increment) +len+)
				(nthcdr y-increment map)
				x-increment
				y-increment))))

(print (solve 0 +map+ 3 1))
(print (apply #'* (list (solve 0 +map+ 1 1)
						(solve 0 +map+ 3 1)
						(solve 0 +map+ 5 1)
						(solve 0 +map+ 7 1)
						(solve 0 +map+ 1 2))))
