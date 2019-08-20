(defpackage :jonathan-sumrall 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :jonathan-sumrall)
(defun alpha-beta (game current-depth max-depth
		   is-maxs-turn-p expand terminal-p evaluate
		   alpha beta)

  (if (or (funcall terminal-p game) (> current-depth max-depth))
      (funcall evaluate game is-maxs-turn-p)      
      (if (funcall is-maxs-turn-p game)	  
	  (progn
	  (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf alpha (max alpha (alpha-beta child (incf current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta))))
	  (if (>= alpha beta) beta alpha))	 
	  (progn
	    (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf beta (min beta (alpha-beta child (incf current-depth) max-depth is-maxs-turn-p expand terminal-p evaluate alpha beta))))
	    (if (>= alpha beta) alpha beta)))))

(defun evaluate (game is-maxs-turn-p)  
  (let ((end (game-over game)))
    (if (null end) 
	(let* (
	       (finalScore 0)
	      (numMine 0)
	      (numTheirs 0)
	      (numOfMyMills 0)
	      (numOfTheirMills 0)
	      (mineAdjacent 0)
	      (myColor (if (funcall is-maxs-turn-p game) (turn game) (if (= (turn game) red) red black)))
	      (theirColor (if (= myColor red) black red))
	      )
	  
	  (loop for place from 0 to 23 do
	      (if (= (pos game place) myColor) (incf numMine))

	       (if (= (pos game place) theirColor) (incf numTheirs))

	       (loop for each in (mills place) do
		    (if (mill-filled-p myColor each game) (incf numOfMyMills))
		    (if (mill-filled-p theirColor each game) (incf numOfTheirMills)))

	       (if (or (= (pos game place) myColor) (= (pos game place) empty))
		   (loop for each in (adjacent-positions place) do
			(if (= (pos game each) myColor) 
			    (progn
			    (incf mineAdjacent))))))
	  
	  
	  (decf finalScore (* 6 numTheirs))
	  (incf finalScore (* 1.5 numMine))
	  (incf finalScore (* 1.5 numOfMyMills))
	  (decf finalScore (* 2 numOfTheirMills))
	  (incf finalScore (* 2 mineAdjacent))
	     finalScore)
	(if (= 0 end)  
	    0
	    (* end (turn game) max-wins (if (funcall is-maxs-turn-p game) 1 -1))))))

(defun make-computer-move (game depth &optional verbose)  
  (let ((max (turn game)))
    (let ((move (max-element (moves game)
			     (lambda (g)
			       (alpha-beta g 0 depth 
					   (lambda (gm) (= (turn gm) max)) 
					   (lambda (gm) (moves gm)) 
					   (lambda (gm) (game-over gm))
					   #'evaluate 
					   min-wins
					   max-wins)))))
      (when (or (= (situation move) black-removes)
	      (= (situation move) red-removes))  
	  (when verbose (print-game move))
	  (setf move (max-element (moves move)
				  (lambda (g)
				    (alpha-beta g 0 depth 
						(lambda (gm) (= (turn gm) max)) 
						(lambda (gm) (moves gm)) 
						(lambda (gm) (game-over gm)) 
						#'evaluate 
						min-wins
						max-wins)))))
      move)))