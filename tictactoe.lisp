(defparameter *computer* 10)
(defparameter *user* 1)
(defparameter *cuatroenlinea*
  '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16) ;Horizontal cuatro en linea.
	(1 5 9 13) (2 6 10 14) (3 7 11 15) (4 8 12 16) ;Vertical cuatro en linea.
	(1 6 11 16) (4 7 10 13))) ;Diagonal cuatro en linea.



(defun make-board ()  
  (list 'board 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))


(defun convert-to-letter (v)
    (cond ((equal v 1) "X")
		((equal v 10) "O")
		(t " ")))


(defun print-row (w x y z)
    (format t "~&  ~A | ~A | ~A | ~A"
  			(convert-to-letter w)
		  (convert-to-letter x)
		  (convert-to-letter y)
		  (convert-to-letter z)))


(defun print-board (board)
    (format t "~%")
  (print-row (nth 1 board)
			 (nth 2 board)
			 (nth 3 board)
			 (nth 4 board))
  (format t "~& --------------")
  (print-row (nth 5 board)
			 (nth 6 board)
			 (nth 7 board)
			 (nth 8 board))
  (format t "~& --------------")
  (print-row (nth 9 board)
			 (nth 10 board)
			 (nth 11 board)
			 (nth 12 board))
  (format t "~& --------------")
  (print-row (nth 13 board)
			 (nth 14 board)
			 (nth 15 board)
			 (nth 16 board))
  (format t "~%~%"))

(defun make-move (player pos board)
    (setf (nth pos board) player)
  board)

(defun sumaTotal (board cuatroenlinea)
    (+ (nth (first cuatroenlinea) board)
	 (nth (second cuatroenlinea) board)
	 (nth (third cuatroenlinea) board)
	 (nth (fourth cuatroenlinea) board)))


(defun analizaTablero (board)
    (mapcar #'(lambda (cuatroenlinea)
			  (sumaTotal board cuatroenlinea))
		  *cuatroenlinea*))


(defun winner-p (board)
    (let ((sums (analizaTablero board)))
	(or (member (* 4 *computer*) sums)
		(member (* 4 *user*) sums))))



(defun validaMovLegal (board)
   (format t "~&Tu marcas: ")
  (let ((pos (read)))
	(cond ((not (and (integerp pos)
					 (<= 1 pos 16)))
		   (format t "~&Este formato no es valido utiliza números del 1 al 16.")
		   (validaMovLegal board))
		  ((not (zerop (nth pos board)))
		   (format t
				   "~&Lo siento, este espacio ya fue ocupado, intenta nuevamente.")
		   (validaMovLegal board))
		  (t pos))))


(defun board-full-p (board)
    (not (member 0 board)))


(defun user-move (board)
   (let* ((pos (validaMovLegal board))
		 (new-board (make-move
					 *user*
					 pos
					 board)))
	(print-board new-board)
	(cond ((winner-p new-board)
		   (format t "~&Has vencido!~%"))
		  ((board-full-p new-board)
		   (format t "~&Empate, para jugar de nuevo ejecuta la función jugar.~%"))
		  (t (computer-move new-board)))))

(defun computer-move (board)
    (let* ((best-move (choose-best-move board))
		 (pos (first best-move))
		 (strategy (second best-move))
		 (new-board (make-move
					 *computer* pos board)))
	(format t "~&La computadora marca: ~S" pos)
	(format t "~&Utilizando: ~A~%" strategy)
	(print-board new-board)
	(cond ((winner-p new-board)
		   (format t "~&La computadora reclama la victoria!~%"))
		  ((board-full-p new-board)
		   (format t "~&Empate, para jugar de nuevo ejecuta la función jugar.~%"))
		  (t (user-move new-board)))))


(defun pick-random-empty-position (board)
    (let ((pos (+ 1 (random 16))))
	(if (zerop (nth pos board))
		pos
		(pick-random-empty-position board))))


(defun random-move-strategy (board)
    (list (pick-random-empty-position board)
		"conquistar tablero"))


(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
			   (zerop (nth pos board)))
		   squares))


(defun ganarObloquear (board target-sum)
    (let ((triplet (find-if
				  #'(lambda (trip)
					  (equal (sumaTotal board trip)
							 target-sum))
				  *cuatroenlinea*)))
	(when triplet
	  (find-empty-position board triplet))))


(defun make-three-in-a-row (board)
    (let ((pos (ganarObloquear board
						   (* 3 *computer*))))
	(and pos (list pos "construir cuatro en linea"))))


(defun evitarVictoriaRival (board)
    (let ((pos (ganarObloquear board
						   (* 3 *user*))))
	(and pos (list pos "Bloquear al oponente"))))


(defun choose-best-move (board)
    (or (make-three-in-a-row board)
	  (evitarVictoriaRival board)
	  (random-move-strategy board)))


(defun jugar ()
  (if (y-or-n-p "Quieres conocer las reglas del juego? ")
	  (user-move (make-board))
	  (user-move (make-board))))

(jugar)