;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
; Return nil only if s contains an element with the value 2 (a box not on any goal space)
(defun goal-test (s)
  (cond
	((not s) t) 
	((atom (car s) )	
		(cond ((isBox (car s)) nil)		; if s is a box space, return false,		
		      (t (goal-test (cdr s)))		; otherwise, check rest of the row
		)
	)
	(s (and (goal-test (car s)) (goal-test (cdr s)))) ; check the current list and pass the rest of the lists to goal-test
  )
);end defun

; Helper function that gets called by get-square, returns the value at column c in the list S
(defun get-row (S c)
	(cond
		; base cases, if S is null or the index is out of bounds, return a wall
		((not S) wall)
		((< c 0) wall)
		((= c 0) (car S))	; if index is zero, return current element
		(t (get-row (cdr S) (- c 1)))	; else, index element into list recursively
	)
)

; helper function for next-state, returns the value at a given position in the state
(defun get-square (S r c)
    (cond 
	  ;; if out of bounds/error, return 1 (value of a wall)
	  ((not S) wall)
	  ((< r 0) wall)
	  ((= 0 r) (get-row (car S) c))      ; if we have found the row, start indexing the column using get-row	 
	  (t (get-square (cdr S) (- r 1) c))		; index rows to find row
    )
)

; Helper function for set-square, sets a value in a row to v
(defun set-row (S c v)
    (cond ((= c 0) (append (list v) (cdr S)))		; if index is reached, modify value there
	  (t (cons (car S) (set-row (cdr S) (- c 1) v)))	; else, keep indexing into the list and ensure the rest of the list is also returned
	)
)

;helper function that sets the value at (r,c) in the state S to v
(defun set-square (S r c v)
    (cond ((= r 0) (cons (set-row (car S) c v) (cdr S)))		; if row is found, call set-row to set the specified value in that row
	  ((> r 0) (cons (car S) (set-square (cdr S) (- r 1) c v)))		; else, continue indexing through list of rows
    )
)

;(print (set-square '((1 4 5 6) (1 3 3 3 3 3)) 0 0 2))
;(print (get-square '((1 3 4 5) (2 3) (3 4 5 6 7) (2 1 3)) 2 3))

(defun try-move (S D)
  ; set function  wide variables, such as location of the keeper and if the keeper is standing on a blank space or star
  (let* ((k (getKeeperPosition S 0)) (c (first k)) (r (second k)) (rm_keeper (cond ((isKeeperStar (get-square S r c)) star) ((isKeeper (get-square S r c)) blank))))
    (cond
		; Attempt to move the keeper up:
		((equal D 'UP)
		  (let* ((up-square (get-square S (- r 1) c)))	; obtain the value of the square above the keeper's
			(cond
				; run through the various cases of possible values where the keeper can actually move: a blank space, box, star, or star on a box
				((isBlank up-square) (set-square (set-square S (- r 1) c keeper) r c rm_keeper))
				((isStar up-square) (set-square (set-square S (- r 1) c keeperstar) r c rm_keeper))
				((isBox up-square)	(cond
							((isBlank (get-square S (- r 2) c)) (set-square (set-square (set-square S (- r 1) c keeper) r c rm_keeper) (- r 2) c box))
							((isStar (get-square S (- r 2) c)) (set-square (set-square (set-square S (- r 1) c keeper) r c rm_keeper) (- r 2) c boxstar))
							(t nil)
				) 
				)
				((isBoxStar up-square)	(cond
							((isBlank (get-square S (- r 2) c)) (set-square (set-square (set-square S (- r 1) c keeperstar) r c rm_keeper) (- r 2) c box))
							((isStar (get-square S (- r 2) c)) (set-square (set-square (set-square S (- r 1) c keeperstar) r c rm_keeper) (- r 2) c boxstar))
							(t nil)
				) 
				)
				(t nil)		; for every other case we simply return null
			)
		  )
		)
		; Attempt to move the keeper down:
		((equal D 'DOWN)
		  (let* ((down-square (get-square S (+ r 1) c)))	; obtain the value of the square below the keeper's
			(cond
				; run through the various cases of possible values where the keeper can actually move: a blank space, box, star, or star on a box
				((isBlank down-square) (set-square (set-square S (+ r 1) c keeper) r c rm_keeper))
				((isStar down-square) (set-square (set-square S (+ r 1) c keeperstar) r c rm_keeper))
				((isBox down-square)	(cond
							((isBlank (get-square S (+ r 2) c)) (set-square (set-square (set-square S (+ r 1) c keeper) r c rm_keeper) (+ r 2) c box))
							((isStar (get-square S (+ r 2) c)) (set-square (set-square (set-square S (+ r 1) c keeper) r c rm_keeper) (+ r 2) c boxstar))
							(t nil)
				) 
				)
				((isBoxStar down-square)	(cond
							((isBlank (get-square S (+ r 2) c)) (set-square (set-square (set-square S (+ r 1) c keeperstar) r c rm_keeper) (+ r 2) c box))
							((isStar (get-square S (+ r 2) c)) (set-square (set-square (set-square S (+ r 1) c keeperstar) r c rm_keeper) (+ r 2) c boxstar))
							(t nil)
				) 
				)
				(t nil)		; for every other case we simply return null
			)
		  )	
		)
		; Attempt to move the keeper left:
		((equal D 'LEFT)
		  (let* ((left-square (get-square S r (- c 1))))	; obtain the value of the square to the left of the keeper's
			(cond
				; run through the various cases of possible values where the keeper can actually move: a blank space, box, star, or star on a box
				((isBlank left-square) (set-square (set-square S r (- c 1) keeper) r c rm_keeper))
				((isStar left-square) (set-square (set-square S r (- c 1) keeperstar) r c rm_keeper))
				((isBox left-square)	(cond
							((isBlank (get-square S r (- c 2))) (set-square (set-square (set-square S r (- c 1) keeper) r c rm_keeper) r (- c 2) box))
							((isStar (get-square S r (- c 2))) (set-square (set-square (set-square S r (- c 1) keeper) r c rm_keeper) r (- c 2) boxstar))
							(t nil)
				) 
				)
				((isBoxStar left-square)	(cond
							((isBlank (get-square S r (- c 2))) (set-square (set-square (set-square S r (- c 1) keeperstar) r c rm_keeper) r (- c 2) box))
							((isStar (get-square S r (- c 2))) (set-square (set-square (set-square S r (- c 1) keeperstar) r c rm_keeper) r (- c 2) boxstar))
							(t nil)
				) 
				)
				(t nil)		; for every other case we simply return null
			)
		  )	
		)
		; Move the keeper right:
		((equal D 'RIGHT)
		  (let* ((right-square (get-square S r (+ c 1))))		; obtain the value of the square to the right of the keeper's
			(cond
				; run through the various cases of possible values where the keeper can actually move: a blank space, box, star, or star on a box
				((isBlank right-square) (set-square (set-square S r (+ c 1) keeper) r c rm_keeper))
				((isStar right-square) (set-square (set-square S r (+ c 1) keeperstar) r c rm_keeper))
				((isBox right-square)	(cond
							((isBlank (get-square S r (+ c 2))) (set-square (set-square (set-square S r (+ c 1) keeper) r c rm_keeper) r (+ c 2) box))
							((isStar (get-square S r (+ c 2))) (set-square (set-square (set-square S r (+ c 1) keeper) r c rm_keeper) r (+ c 2) boxstar))
							(t nil)
				) 
				)
				((isBoxStar right-square)	(cond
							((isBlank (get-square S r (+ c 2))) (set-square (set-square (set-square S r (+ c 1) keeperstar) r c rm_keeper) r (+ c 2) box))
							((isStar (get-square S r (+ c 2))) (set-square (set-square (set-square S r (+ c 1) keeperstar) r c rm_keeper) r (+ c 2) boxstar))
							(t nil)
				) 
				)
				(t nil)		; for every other case we simply return null
			)
		  )		
		)	
    )
  )
)

;(print (getKeeperPosition '((1 3 2 1) (1 4 0 1)) 0))
;(print (set-square (set-square '((1 3 2 1) (1 4 0 1)) (+ 0 0) 1 keeperstar) 0 1 blank))
;(print (try-move '((0 3 2 0) (1 4 0 1)) 'RIGHT))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
	; return a list of possible states from attempting to move the keeper up. down, left, and right (all possible actions in Sokoban)
	(cleanUpList (append (list (try-move s 'UP)) (list (try-move s 'DOWN)) (list (try-move s 'LEFT)) (list (try-move s 'RIGHT))))
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
    0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; loops through the game state and counts the number of squares that are a box
; This is an admissable heuristic because the number of actions to solve the game can never be less
; than the number of boxes not in goal states. Since each box requires at least 1 action to move to
; a goal, then the number of actions left >= the number of boxes left.
(defun h1 (s)
	(cond
		((not s) 0)		; if is is null, return 0
		; if the current value is a square:
		((atom (car s)) (cond
			((isBox (car s)) (+ 1 (h1 (cdr s))))	; if the current value is a box, return 1 + the number of boxes left in the row
			(t (h1 (cdr s)))						; else, only return the number of boxes left in the row
		))
		; else, add the number of boxes in the current row and remaining rows
		(t (+ (h1 (car s)) (h1 (cdr s))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for h105138860
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a state s and an initial value of (0, 0) for (r, c) 
; will return a list of coordinates of boxes in s
(defun get-boxes (s r c)
	(cond
		((not s) nil)		; if is is null, return nil
		; if the current value is a square:
		((atom (car s)) (cond
			((isBox (car s)) (cleanUpList (cons (list r c) (get-boxes (cdr s) r (+ c 1)))))		; if the value is a box , add it to the list
			(t (get-boxes (cdr s) r (+ c 1)))													; otherwise, continue searching
		))
		
		(t (append (get-boxes (car s) r c) (get-boxes (cdr s) (+ r 1) c)))
	)
)

; takes a state s and an initial value of (0, 0) for (r, c) 
; will return a list of coordinates of stars in s
(defun get-stars (s r c)
	(cond
		((not s) nil)		; if is is null, return nil
		; if the current value is a square:
		((atom (car s)) (cond
			((or (isStar (car s)) (isKeeperStar (car s))) (cleanUpList (cons (list r c) (get-stars (cdr s) r (+ c 1)))))	; if the value is a star or a keeper on a star, add it to the list
			(t (get-stars (cdr s) r (+ c 1)))											; otherwise coontinue searching
		))
		
		(t (append (get-stars (car s) r c) (get-stars (cdr s) (+ r 1) c)))
	)
)

; The follow two helper functions were originally to be used in a heuristic that employs BFS to find the 
; shortest possible path from a box to it's nearest goal. However, the idea was scrapped due to the fact
; that it took so long because I could not keep track of previously visited nodes
;
;; (defun BFS_helper (s boxes)
;; 	(cond
;; 		((not boxes) 0)
;; 		(t (+ (BFS s (list (append (car boxes) '(0)))) (BFS_helper s (cdr boxes))))
;; 	)
;; )

;; (defun BFS (s pos)
;; 	(let ((curr (car pos)))
;; 	(cond
;; 		((isWall (get-square s (first curr) (second curr))) (BFS s (cdr pos)))
;; 		((isStar (get-square s (first curr) (second curr))) (third curr))
;; 		(t (BFS s (append 
;; 					(cdr pos) 
;; 					(list (list (+ (first curr) 1) (second curr) (+ (third curr) 1)))
;; 					(list (list (- (first curr) 1) (second curr) (+ (third curr) 1)))
;; 					(list (list (first curr) (+ (second curr) 1) (+ (third curr) 1)))
;; 					(list (list (first curr) (- (second curr) 1) (+ (third curr) 1)))
;; 				  )
;; 			)
;; 		)
;; 	)
;; 	)
;; )

; calculates the absolute value of a number x
(defun absVal (x)
  (cond 
  		((>= x 0) x)		; if x is positive, return x
        (t (- 0 x))			; else, return negative x
  )
)

; calculates the manhatten distance between two points
(defun manhattenDstnc (box star)
	(+ (absVal (- (first box) (first star))) (absVal (- (second box) (second star))))		; manh(x1, y1, x2, y2) = abs(x1 - x2) + abs(y1 - y2) 
)

; Calcuates the manhatten distances between a box and every possible star and returns the smallest value
(defun manh (box stars)
	(cond
		((equal (length stars) 1) (manhattenDstnc box (car stars)))
		(t (min (manhattenDstnc box (car stars)) (manh box (cdr stars))))
	)
)

; calculates the sum of the manhatten distance for all boxes
; Takes in a list of coordinates of boxes and a list of coordinates for stars and returns an integer value >= 0
(defun distances (boxes stars)
	(cond
		((not boxes) 0)
		(t (+ (manh (car boxes) stars) (distances (cdr boxes) stars)))
	)
)

; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
; This is an admissable heuristic that returns the sum of all the manhatten distances
; from each box to its closest possible goal. This ensures the heuristic is admissable.
(defun h105138860 (s)
	(let* ((boxes (get-boxes s 0 0)) (stars (get-stars s 0 0)))		; set boxes equal to a list of all coordinates of all boxes in s, and stars to a list of all coordinates of stars in s
		(distances boxes stars)										; return the sum of the manhatten distances for each box from it's nearest goal
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

; testing various heuristics:
;(load-a-star)
;(time (sokoban p15 #'h1))
;(time (sokoban p21 #'h105138860))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
