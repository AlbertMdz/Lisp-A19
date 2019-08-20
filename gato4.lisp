(defparameter *pc-simbolo* 'o)
(defparameter *player-simbolo* 'x)
(defparameter *max-depth* 15)
(defparameter *menusINFINITY* most-negative-fixnum)
(defparameter *INFINITY* most-positive-fixnum)


(defun tableroGato ()
  (list nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))
(defun tablero (game)(first game))
(defun (setf tablero) (val game) (setf (first game) val))
(defun pintarTablero (board)
	(format t "~%____________ 
[~a][~a][~a][~a]    [ 0][ 1][ 2][ 3]
===============				
[~a][~a][~a][~a]    [ 4][ 5][ 6][ 7]
===============	             
[~a][~a][~a][~a]    [ 8][ 9][10][11]
===============	            
[~a][~a][~a][~a]    [12][13][14][15] ~%"
		(or (nth 0 	board) " ") (or (nth 1 	board) " ") (or (nth 2 	board) " ") (or (nth 3 	board) " ")
		(or (nth 4 	board) " ") (or (nth 5 	board) " ") (or (nth 6 	board) " ") (or (nth 7 	board) " ")
		(or (nth 8 	board) " ") (or (nth 9 	board) " ") (or (nth 10 board) " ") (or (nth 11 board) " ")
		(or (nth 12 board) " ") (or (nth 13 board) " ") (or (nth 14 board) " ") (or (nth 15 board) " ")
	)
)


(defun pintarSimbolo (board simbolo casilla)
	(when (typep casilla 'integer)
		(when (<= casilla (length board))
			(let 	((casilla-a-ocupar (nth casilla board))
					(board-aux (copy-list board)))
					(when (not casilla-a-ocupar)
						(setf (nth casilla board-aux) simbolo)
						board-aux))) ))


(defun mostrarTablero (board)
	(not (member nil board))
)

(defun 3enLinea (board simbolo)
  (or 	(and 
	     	(equal (nth 0 board) simbolo)  
		   	(equal (nth 1 board) simbolo)
		   	(equal (nth 2 board) simbolo)
		   	(equal (nth 3 board) simbolo))
	     
		(and 
			(equal (nth 4 board) simbolo) 
			(equal (nth 5 board) simbolo) 
			(equal (nth 6 board) simbolo)
			(equal (nth 7 board) simbolo))

		(and 
			(equal (nth 8 board) simbolo) 
			(equal (nth 9 board) simbolo) 
			(equal (nth 10 board) simbolo)
			(equal (nth 11 board) simbolo))

		(and 
			(equal (nth 12 board) simbolo) 
			(equal (nth 13 board) simbolo) 
			(equal (nth 14 board) simbolo)
			(equal (nth 15 board) simbolo))

		(and 
			(equal (nth 0 board) simbolo) 
			(equal (nth 4 board) simbolo) 
			(equal (nth 8 board) simbolo)
			(equal (nth 12 board) simbolo))

		(and 
			(equal (nth 1 board) simbolo) 
			(equal (nth 5 board) simbolo) 
			(equal (nth 9 board) simbolo)
			(equal (nth 13 board) simbolo))

		(and 
			(equal (nth 2 board) simbolo) 
			(equal (nth 6 board) simbolo) 
			(equal (nth 10 board) simbolo)
			(equal (nth 14 board) simbolo))

		(and 
			(equal (nth 3 board) simbolo) 
			(equal (nth 7 board) simbolo) 
			(equal (nth 11 board) simbolo)
			(equal (nth 15 board) simbolo))

		(and 
			(equal (nth 0 board) simbolo) 
			(equal (nth 5 board) simbolo) 
			(equal (nth 10 board) simbolo)
			(equal (nth 15 board) simbolo))

		(and 
			(equal (nth 3 board) simbolo) 
			(equal (nth 6 board) simbolo) 
			(equal (nth 9 board) simbolo)
			(equal (nth 12 board) simbolo))
		)
)

(defun funcionEvaluacion (board simbolo)
    (cond 	((3enLinea board simbolo)
				(+ 1 (/ 1 (length (remove nil board)))))
    		((3enLinea board (oponente simbolo))
				(- (+ 1 (/ 1 (length (remove nil board))))))
		  	(t 0)) )

(defun movimientos (board simbolo)
	(loop for casilla from 0 to 15 unless (nth casilla board) 
	 	collect (pintarSimbolo board simbolo casilla)))

(defun oponente (simbolo)
  (if (equal simbolo *player-simbolo*) *pc-simbolo* *player-simbolo*))

(defun negamax(board depth maxDepth alfa beta simbolo)
  (cond (  (or (mostrarTablero board) (3enLinea board *pc-simbolo*) (3enLinea board *player-simbolo*) (>= depth maxDepth));  (deep-enough board depth)
         	(cons (funcionEvaluacion board simbolo) board)
        )
        (t (let ( 	(mejor-movimiento nil)
                 	(mejor-valor *menusINFINITY*)
                 	(sucesores (movimientos board simbolo))
                 	(stop nil))
			(loop for sucesor in sucesores until stop do
				(let* ((nuevo-estado (negamax sucesor (+ depth 1) maxDepth (* -1 beta) (* -1 (max alfa mejor-valor)) (oponente simbolo)))
			          (valor (* -1 (first nuevo-estado))) )
				(when (> valor mejor-valor)
					(setf mejor-valor valor)
					(setf mejor-movimiento sucesor))
				(when (>= mejor-valor beta)
					(setq stop t)) ))
			(cons mejor-valor mejor-movimiento))) ))

(defun gato4x4 ()
	(let ( 	(board (tableroGato))
			(board-aux nil)
			(casilla nil)
			(alfa *menusINFINITY*)
        	(beta *INFINITY*))
		
		(do ()			
			( 	(or (3enLinea board *pc-simbolo*) (3enLinea board *player-simbolo*) (mostrarTablero board))
		        	(pintarTablero board)
			        (cond 	((3enLinea board *pc-simbolo*) (format t "Gala la PC.~%"))
			               	((3enLinea board *player-simbolo*) (format t "Ganaste.~%"))
			               	(t  (format t "Fue empate.~%"))) )			
			(pintarTablero board)			
			(print "Turno del jugador: ")
			(print "----")
			(setf casilla (read))			
			(loop until (setq board-aux (pintarSimbolo board *player-simbolo* casilla))
				do 
				(format t "~%La casilla ~a esta ocupada o no existe~%" casilla)
				(format t "Selecciona una casilla: ")
				(print "----")
				(setf casilla (read))) 

			(setf board board-aux)			
			(when (and (not (3enLinea board *pc-simbolo*)) (not (3enLinea board *player-simbolo*)) (not (mostrarTablero board)) )				
				(format t "Movimiento jugador: ~%")
				(pintarTablero board)
				(setf board (rest (negamax board 0 *max-depth* alfa beta *pc-simbolo*)))
				(format t "Movimiento PC: ~%") 
				)) 
		))
(gato4x4)