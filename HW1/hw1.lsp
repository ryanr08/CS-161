; Function that takes in a single integer argument n and returns the Nth Padovan number 
(defun PAD (n)
    (if (< n 3)
	1
	(+ (PAD(- n 1)) (PAD(- n 2)) (PAD(- n 3)))
    )
)


; test cases for PAD function (tests values 0 - 9 as well as some larger values)
(format t "~% PAD 0: ~d " (PAD 0))
(format t "~% PAD 1: ~d " (PAD 1))
(format t "~% PAD 2: ~d " (PAD 2))
(format t "~% PAD 3: ~d " (PAD 3))
(format t "~% PAD 4: ~d " (PAD 4))
(format t "~% PAD 5: ~d " (PAD 5))
(format t "~% PAD 6: ~d " (PAD 6))
(format t "~% PAD 7: ~d " (PAD 7))
(format t "~% PAD 8: ~d " (PAD 8))
(format t "~% PAD 9: ~d " (PAD 9))
(format t "~% PAD 10: ~d " (PAD 10))
;(format t "~% PAD 30: ~d " (PAD 30))

; Function that takes in a single numeric argument N, and returns the number of additions required by the PAD function to compute the Nth PAD number
(let ((c 0))
(defun SUMS (n)
    (cond ((< n 3) 0)
	  ((= n 3) 2)
	  (t (+ (SUMS(- n 1)) (SUMS(- n 2)) (SUMS(- n 3)) 2))
    )
)
)

; test cases for SUMS function
(format t "~% SUMS 0: ~d " (SUMS 0))
(format t "~% SUMS 1: ~d " (SUMS 1))
(format t "~% SUMS 2: ~d " (SUMS 2))
(format t "~% SUMS 3: ~d " (SUMS 3))
(format t "~% SUMS 4: ~d " (SUMS 4))
(format t "~% SUMS 5: ~d " (SUMS 5))
(format t "~% SUMS 6: ~d " (SUMS 6))
(format t "~% SUMS 7: ~d " (SUMS 7))
(format t "~% SUMS 8: ~d " (SUMS 8))
(format t "~% SUMS 9: ~d " (SUMS 9))

; Function that takes a single argument TREE that represents a tree, and returns an anonymized tree with the same structure (all symbols and #s are replaced by 0)
(defun ANON (TREE)
    (cond ((not Tree) nil)
	  ((atom TREE) 0)
	  (t (cons (ANON(car TREE)) (ANON(cdr TREE))))
    )

)

; test cases for ANON function

(format t "~% ANON '42: ~d " (ANON '42))
(format t "~% ANON 'FOO: ~d " (ANON 'FOO))
(format t "~% ANON '(((L E) F) T): ~d " (ANON '(((L E) F) T)))
(format t "~% ANON '(5 FOO 3.1 -0.2): ~d " (ANON '(5 FOO 3.1 -0.2)))
(format t "~% ANON '(1 (FOO 3.1) -0.2): ~d " (ANON '(1 (FOO 3.1) -0.2)))
(format t "~% ANON '(((1 2) (FOO 3.1)) (BAR -0.2)): ~d " (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
(format t "~% ANON '(R (I (G (H T)))): ~d " (ANON '(R (I (G (H T))))))
