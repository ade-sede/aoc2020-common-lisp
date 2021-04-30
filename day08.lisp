(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defvar +initial-program+)
(setf +initial-program+ (let ((input (uiop:read-file-lines "08.input")))
	(loop for i from 0 to (length input)
		  :collect (alexandria:flatten (list i
						  (split-sequence:split-sequence #\space
														 (first (nthcdr i input))))))))

(defvar *trace*)
(setf *trace* ())

(defvar *accumulator*)
(setf *accumulator* 0)

(defvar *natural-end*)
(setf *natural-end* nil)

(defun get-instruction (program instr-id)
  (find-if #'(lambda (l)
			   (= (first l) instr-id))
		   program))

(defun execute (program instr-id)
  (cond
		((= instr-id (- (length +initial-program+) 2))
		 (progn
		   ;; (format t "Exit - program reached end-instruction ~a~%" instr-id)
		   ;; (format t "Accumulator when exiting: ~a~%" *accumulator*)
		   (setf *natural-end* t)
		   (return-from execute *accumulator*)))
		((find instr-id *trace*)
		 (progn
		   ;; (format t "Exit - program tried to jump to ~a~%" instr-id)
		   ;; (format t "Accumulator when exiting: ~a~%" *accumulator*)
		   (return-from execute *accumulator*)))

		(t (progn
			   (push instr-id *trace*)
			   (let* ((instr (get-instruction program instr-id))
					  (instr-code (second instr))
					  (instr-arg (parse-integer (third instr))))
				 (cond
				   ((string= instr-code "nop")
					(execute program (1+ instr-id)))
				   ((string= instr-code "jmp")
					(execute program (+ instr-id instr-arg)))
				   ((string= instr-code "acc")
					(progn
					  (setf *accumulator* (+ *accumulator* instr-arg))
					  (execute program (1+ instr-id))))))))))

;; Part 1
;; Part 2 outputs a lot.
;; To see output ofr part 1 comment part2
(print (execute +initial-program+ 0))


;; Part 2
;; Relevant output is the last line, because it stops as soon as it is
;; resolved.
(dolist (instr +initial-program+)
  (if (not *natural-end*)
	  (progn
		(setf *trace* ())
		(setf *accumulator* 0)
		(setf *natural-end* nil)
		(let ((new-instr (copy-list instr))
			  (copied-program (copy-list +initial-program+)))
		  (cond ((and (string= (second instr) "nop")
					  (/= 0 (parse-integer (third instr))))
				 (setf (nth 1 new-instr) "jmp"))
				((string= (second instr) "jmp")
				 (setf (nth 1 new-instr) "nop")))
		  (setf (nth (first instr) copied-program) new-instr)
		  (print (execute copied-program 0))))))
