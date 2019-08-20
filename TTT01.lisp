;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; Tic-Toc-Toe Game: Plays and explains the strategy behind each move. 
;;;
;;; Copyright © 2018 Karthik Kumar <karthikkumar.s@protonmail.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.


;;;;;;;;;;;;;;;;;;;;;;;
;; Tic-Toc-Toe Game: ;;
;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *computer* 10)
(defparameter *user* 1)
(defparameter *triplets*
  '((1 2 3) (4 5 6) (7 8 9) ;Horizontal triplets.
	(1 4 7) (2 5 8) (3 6 9) ;Vertical triplets.
	(1 5 9) (3 5 7))) ;Diagonal triplets.



(defun make-board ()
  "Creates a board. Consist of the symbol BOARD followed by 
nine numbers. zero - empty; one - O; ten - X"
  (list 'board 0 0 0 0 0 0 0 0 0))


(defun convert-to-letter (v)
  "zero - empty; one - O; ten - X"
  (cond ((equal v 1) "O")
		((equal v 10) "X")
		(t " ")))


(defun print-row (x y z)
  "Accesses convert-to-letter to print single row."
  (format t "~&  ~A | ~A | ~A"
		  (convert-to-letter x)
		  (convert-to-letter y)
		  (convert-to-letter z)))


(defun print-board (board)
  "Displays the entire board"
  (format t "~%")
  (print-row (nth 1 board)
			 (nth 2 board)
			 (nth 3 board))
  (format t "~& -----------")
  (print-row (nth 4 board)
			 (nth 5 board)
			 (nth 6 board))
  (format t "~& -----------")
  (print-row (nth 7 board)
			 (nth 8 board)
			 (nth 9 board))
  (format t "~%~%"))


(defun make-move (player pos board)
  "destructively changing one of the board positions from zero to 1 (for O)
or a 10 (for X)"
  (setf (nth pos board) player)
  board)


;; (make-move *user* 5 b)
;; (make-move *computer* 3 b)
;; (print-board b)



(defun sum-triplets (board triplets)
  "Calculates the sum of each rows"
  (+ (nth (first triplets) board)
	 (nth (second triplets) board)
	 (nth (third triplets) board)))


(defun compute-sum (board)
  "To fully analyze a board."
  (mapcar #'(lambda (triplets)
			  (sum-triplets board triplets))
		  *triplets*))


(defun winner-p (board)
  "Predicate checks if there is a winner. 
if sum of any triplets is either 3 or 30, then there is a winner."
  (let ((sums (compute-sum board)))
	(or (member (* 3 *computer*) sums)
		(member (* 3 *user*) sums))))



(defun read-a-legal-move (board)
  "Checks whether the entered move is legal."
  (format t "~&Your move: ")
  (let ((pos (read)))
	(cond ((not (and (integerp pos)
					 (<= 1 pos 9)))
		   (format t "~&Invalid input.")
		   (read-a-legal-move board))
		  ((not (zerop (nth pos board)))
		   (format t
				   "~&That space is already occupied.")
		   (read-a-legal-move board))
		  (t pos))))


(defun board-full-p (board)
  "Tests if there are no more empty spaces left on the board"
  (not (member 0 board)))


(defun user-move (board)
  "Give chance to user."
  (let* ((pos (read-a-legal-move board))
		 (new-board (make-move
					 *user*
					 pos
					 board)))
	(print-board new-board)
	(cond ((winner-p new-board)
		   (format t "~&You win!~%"))
		  ((board-full-p new-board)
		   (format t "~&Tie game.~%"))
		  (t (computer-move new-board)))))

(defun computer-move (board)
  "Give chance to computer."
  (let* ((best-move (choose-best-move board))
		 (pos (first best-move))
		 (strategy (second best-move))
		 (new-board (make-move
					 *computer* pos board)))
	(format t "~&My move: ~S" pos)
	(format t "~&My strategy: ~A~%" strategy)
	(print-board new-board)
	(cond ((winner-p new-board)
		   (format t "~&I win!~%"))
		  ((board-full-p new-board)
		   (format t "~&Tie game.~%"))
		  (t (user-move new-board)))))


(defun pick-random-empty-position (board)
  "Picks a random number from one to nine."
  (let ((pos (+ 1 (random 9))))
	(if (zerop (nth pos board))
		pos
		(pick-random-empty-position board))))


(defun random-move-strategy (board)
  "Pick random"
  (list (pick-random-empty-position board)
		"random move"))


(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
			   (zerop (nth pos board)))
		   squares))


(defun win-or-block (board target-sum)
  "Making a decision"
  (let ((triplet (find-if
				  #'(lambda (trip)
					  (equal (sum-triplets board trip)
							 target-sum))
				  *triplets*)))
	(when triplet
	  (find-empty-position board triplet))))


(defun make-three-in-a-row (board)
  "Completing the row for winning"
  (let ((pos (win-or-block board
						   (* 2 *computer*))))
	(and pos (list pos "make three in a row"))))


(defun block-opponent-win (board)
  "Blocking the opponent if they have more than two in a row"
  (let ((pos (win-or-block board
						   (* 2 *user*))))
	(and pos (list pos "block opponent"))))


(defun choose-best-move (board)
  "Choosing best move. Kind of intelligent."
  (or (make-three-in-a-row board)
	  (block-opponent-win board)
	  (random-move-strategy board)))


(defun play-one-game ()
  "Offers the user the choice to go first, and then call either computer-move
or opponent-move as appropriate"
  (if (y-or-n-p "Would you like to go first? ")
	  (user-move (make-board))
	  (computer-move (make-board))))


(play-one-game)