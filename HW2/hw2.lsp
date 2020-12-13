;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; Takes in a argument FRINGE that is a list that represents a search tree.
; Returns a list of leaf nodes that represent the order in which BFS visited the nodes from left to right
(defun BFS (FRINGE)
    (cond ((not FRINGE) nil)
	  ((atom (car FRINGE)) (cons (car FRINGE) (BFS(cdr FRINGE)))) ; If car FRINGE is a node, add it to the returned list and call BFS on rest of tree
          (t (BFS(append (cdr FRINGE) (car FRINGE))))  		; Else, move on to next node and call BFS on it
    )   
)

; Test cases for question one
;(format t "~% BFS '(ROOT): ~d " (BFS '(ROOT)))
;(format t "~% BFS '((((L E) F) T)): ~d " (BFS '((((L E) F) T))))
;(format t "~% BFS '((R (I (G (H T))))): ~d " (BFS '((R (I (G (H T)))))))
;(format t "~% BFS '(((A (B)) (D) C)): ~d " (BFS '(((A (B)) (D) C))))
;(format t "~% BFS '((T (H R E) E)): ~d " (BFS '((T (H R E) E))))
;(format t "~% BFS '((A ((C ((E) D)) B))): ~d " (BFS '((A ((C ((E) D)) B)))))


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (cond ((equal S '(T T T T)) T) ; return true if state is equal to final state
    (t nil)			; false otherwise
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (cond ((equal A 'h) ; action is move homer
	(cond ((equal S '(NIL NIL NIL T)) ())
	      ((equal S '(NIL NIL T NIL)) ()) 		; Invalid states
	      ((equal S '(T T T NIL)) ())
	      ((equal S '(T T NIL T)) ()) 
	      (t (list (cons (not (car S)) (cdr S))))   ; if a valid state, return the state
	))
    

    ((equal A 'b)
        (cond ((equal (car S) (car (cdr S))) (list (cons (not (car S)) (cons (not (car (cdr S))) (cdr (cdr S)))))) ; valid state
              (t ())
        ))
    

    ((equal A 'd)
        (cond ((equal S '(NIL NIL NIL NIL)) ())
              ((equal S '(T T T T)) ())				; invalid states
	      ((equal (not (car S)) (car (cdr (cdr S)))) ())
              (t (list (cons (not (car S)) (cons (car (cdr S)) (cons (not (car (cdr (cdr S)))) (cdr (cdr (cdr S))))))))   ; Valid state
        ))
    

    ((equal A 'p)
        (cond ((equal S '(NIL NIL NIL NIL)) ())
              ((equal S '(T T T T)) ())						; invalid states
              ((equal (not (car S)) (car (cdr (cdr (cdr S))))) ())
              (t (list (cons (not (car S)) (cons (car (cdr S)) (cons (car (cdr (cdr S))) (cons (not (car (cdr (cdr (cdr S))))) nil)))))) ; valid states
        ))

    (t ()) ; this should never be reached

    )
)

;(format t "~% ~d " (NEXT-STATE '(T T NIL T) 'h))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  (append (NEXT-STATE S 'h)(NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)) ; put all possible next states into a list
)

;(format t "~% ~d " (SUCC-FN '(NIL T NIL NIL)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
     (cond 
	((not STATES) nil)	; if STATES is empty, return null
	((equal S (car STATES)) T)	; if top state is equal to current state, return true
	(t (ON-PATH S (cdr STATES)))	; otherwise, pop off top state
     )   
)

;(format t "~% ~d " (ON-PATH '(T T NIL NIL) '((NIL T NIL NIL) (T T NIL NIL))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond ((not STATES) ())   	; return nil if STATES is empty
	  (t (let ((result (DFS (car STATES) PATH))) ; set result to the value returned on DFS of first node
			(cond ((not result) (MULT-DFS (cdr STATES) PATH))	; if DFS on first node failed, move on to next nodes
			    (T result)						; if DFS returned the goal state, return path to goal state
			)
	  ))
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond ((FINAL-STATE S) (append PATH (list S)))		; if S is final state, return path to goal state
	  ((ON-PATH S PATH) ())					; if S has already been visited, do not expand
	(t (MULT-DFS (SUCC-FN S) (append PATH (list S))))	; otherwise, run DFS on the successors of S
    )
)
