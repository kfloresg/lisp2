(defparameter *KB* (make-hash-table :test #'equal))
(defparameter *input-key* "some")

;Takes in a fact, stores the complete fact in hashtable, and then stores the same fact under all variants
(defun store-fact (wff) 
	(setf(gethash wff *KB*) T)
	(cond 
	  ((equal (list-length wff) 2) (add-all-variants wff 1))
	  ((equal (list-length wff) 3) (add-all-variants wff 2))
	  (t))
)

;From assignment handout
(defun -keys (wff)
	(cond ((atom wff) wff) 
	      (t (cons (cons '- (cdr wff))
		       (mapcar #'(lambda (x) (cons (car wff) x))
			       (-keys (cdr wff))))))
)

;Creates a list of variants given the truth statement, truth statement has already been parsed to a 
;be a list at this point using the function string-to-list
(defun --keys(wff) 
  	(setq n (- (list-length wff) 1))
	(setq lst '())
	(loop for i from 0 to n doing
	      (setq temp (copy-list wff))
	      (setf (nth i temp) '-)
	      (setq lst (adjoin temp lst :test 'equal) ) 
	      (loop for j from 0 to n doing 
		    (setq temp2 (copy-list temp))
		    (setf (nth j temp2) '-)
		    (setq lst (adjoin temp2 lst :test 'equal))
		    ))
	lst)

;add all the variants for the statement
(defun add-all-variants (wff num) 
  (cond 
    ((equal num 1) (add-multiple (-keys wff)))
    ((equal num 2) (add-multiple (--keys wff)))
    (t)))

;adds the statement and every variant to the knowledge by iterating through the list
(defun add-multiple (wff)
 	(setq n (- (list-length wff) 1))
	(loop for i from 0 to n do
	      (setq temp (nth i wff))
	      (setf (gethash temp *KB*) T) 
	      ))

;function to answer what question, sets the value of global parameter answer to value from KB or unknown 
(defun answer-whq (wff)
  (setq answer (gethash wff *KB*))
  (if (equal answer nil)
    (format nil "UNKNOWN") 
    (format nil "~a" answer)))

(defun answer-question (wff)
  (setq answer (gethash wff *KB*)) 
  (cond 
    ((not (equal nil answer)) (format nil "~a" answer))
    ((and (equal nil answer) (negation wff)) (format nil "(not (~a))" wff))
    ((equal answer nil) (format nil "UNKNOWN"))
    (t (format nil "UNKNOWN"))))

;gets a list of possible keys given a keyword, then runs each of those keys through anser-whq
(defun get-possible (key)
  (setq templist '())
  (loop for k being the hash-keys of *KB*
	do (if (member key k)
	     (print k)
	     nil))
  (print templist) 
)


;function checks if the question has a negation, if there are only two elements it will check the predicate 
;using the function check-predicates. If longer statement, checks if Robbie is in the statement 
(defun negation (wff) 
  (if (equal (list-length wff) 2) (check-predicates wff) 
    (and (not (eq '- (car wff)))(member 'Robbie wff))))

;function to check the predicate of the statement, 
(defun check-predicates (wff)
  (loop for i in *KB* 
	do (and (not (equalp - (car wff))) (equalp (car i) (car wff)))))

;function to split predicates by their undescores
(defun split-atom-at-underscores (atm)
  (setq str (substitute #\SPACE #\_ (write-to-string atm)))
  ;(print str)
  (read-from-string str)
)
  
(defun answer-question-nicely (wff)
  (setq answer (gethash wff *KB*))
  (cond
    ((equal answer t) (print-affirmative wff))
    (t (format t "The answer is not known")))
)

;if the answer is known to Robbie then print the answer in English format 
(defun print-affirmative (wff)
  (cond 
    ((equal (list-length wff) 2) (print-single wff))
    ((equal (list-length wff) 3) (print-multiple wff))
    (t (format t "There was a problem"))))

;prints the answer (when it is known), of a statement with one atom 
(defun print-single (wff)
  (setq temp (append '(is) (cons (car wff) '())))
  (setq temp (append (last wff) temp))
  (print temp))

;prints the answer (when it is known), with multiple atoms
(defun print-multiple (wff)
  (setq temp (cons (car wff) '()))
  (setq tmplst (cons (nth 1 wff) '()))
  (setq temp (append tmplst temp))
  (setq tmp2 (cons (nth 2 wff) '()))
  (setq temp (append temp tmp2))
  (print temp))

(defun repl-function (f) 
  (format t "Enter arguments to test for ~a from the key list above (enter q to exit): " f)
  (finish-output nil) 
  (setq in (read)) 
  (if (equalp (format nil "~a" in) "q")
    nil (if (format-function f in) nil (repl-function f))))

(defun format-function (f a) 
  (format t "(~a ~a) => ~a ~%" f a (funcall f a)))

;Testing functions 
(store-fact '(likes Robbie Samantha))
(store-fact '(owns Alice Snoopy))
(store-fact '(owns Alice Freddy))
(store-fact '(dog Snoopy))
(store-fact '(dog Freddy))
(store-fact '(clever Robbie))
(store-fact '(clever Alice))
(store-fact '(likes Alice Snoopy))
(store-fact '(likes Alice Robbie))

(format t "~% Example testing the function answer-whq")
(format t "~% Iput to function answer-whq '- Robbie Samantha' =>")
(print (answer-whq '(- Robbie Samantha)))

(format t "~% Example Testing function answer-question-nicely") 
(format t "~% Input to function 'likes Robbie Samantha") 
(answer-question-nicely '(likes Robbie Samantha))

(format t "~% ------- Starting REPL for testing answer-whq ------ ~%")
(format t "~% Enter a possible question for example likes, or robbie, and so on (one single word)")
(setq *input-key* (read))
(format t "~% Avaiable keys based on your input")
(get-possible *input-key*)
(repl-function 'answer-whq)


(format t "~% --- Starting Testing for answer-question --- ~%")
(format t "~% Enter a possible question for example likes, or robbie, and so on (one single word)")
(setq *input-key* (read))
(format t "~% Available keys based on your input")
(get-possible *input-key*)
(repl-function 'answer-whq)


(format t "~% ------ Starting Testing for answer-question-nicely ----- ~%")
(format t "~% Enter a possible question for example likes, or robbie and so on (one single word)")
(setq *input-key* (read))
(format t "~% Available keys based on your input")
(get-possible *input-key*)
(repl-function 'answer-question-nicely) 

