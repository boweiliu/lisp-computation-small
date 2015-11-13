(defun f1 (ac2 xz2 ac3 xz3)
  ( + ( * 4 ac2 xz2) (* 12 ac2 xz3) (* 12 ac3 xz2) (* 27 ac3 xz3) ) )
(defun objective ( a c x z)
  ( f1
   ( + ( * a a ) (* a c ) (* c c) )
   ( + ( * x x ) (* x z ) (* z z) )
   ( * a c (+ a c ) )
   ( * x z (+ x z ) )))
(defun constraints (eps)
  (lambda (a c x z) ( and
    (leq a 0 c)
    (leq x 0 z)
    (leq (* -2 c) a (* -0.5 c))
    (leq (* -2 z) x (* -0.5 z))
    (leq (- c x) (+ 1 (- (/ 1 3) (* 4 eps))))
    (leq (- z a) (+ 1 (- (/ 1 3) (* 4 eps))))
    (leq (+ a x) (+ (/ -2 3) (* 2 eps)))
    (leq (+ c z) (- (/ 4 3) (* 4 eps)))
    (leq (+ 1 a) z))))
(defun range (step-sz)
  `(( -10 0 ,step-sz)  ; a
    (0 10 ,step-sz)    ; c
    (0 10 ,step-sz)    ; x
    (-10 0 ,step-sz))) ; z

(defun fuzz-val () 1.0d-8)
(defun leq2 (e1 e2) (< e1 (+ e2 (fuzz-val))))
(defun leq (s1 &rest s2)
  (cond ((null s2) t)
	(t (and	(leq2 s1 (car s2))
	        (apply 'leq s2)))))

(defun gen-range (bot top step)
  (cond ((not (leq bot top)) ())
	(t (cons bot (gen-range (+ bot step) top step)))))
(defun exp-prod-hlpr (l1 l2)
  (mapcan (lambda (e1) ( mapcar (lambda (e2) (cons e1 e2)) l2 ) ) l1))
(defun exp-prod (lists) (reduce #'exp-prod-hlpr lists
				:from-end t
				:initial-value '(())))
(defun gen-tuples (ranges)
  (exp-prod (mapcar (lambda (x) (apply #'gen-range x)) ranges)))
(gen-tuples (range .2))
