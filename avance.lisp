(defparameter *pc-simbolo* 'o)
;; símbolo de player
(defparameter *player-simbolo* 'x)
;; máxima profundidad
(defparameter *max-depth* 16)
;; menos infinito
(defparameter *menusINFINITY* most-negative-fixnum)
;; infinito
(defparameter *INFINITY* most-positive-fixnum)

;;=======================================================================
;; fn-Init-Board 
;; Función para para inicializar el tablero
;;=======================================================================
(defun fn-Init-Board ()
  (list nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

;;=======================================================================
;; fn-Paint-Board
;; Imprime el tablero 4x4 
;;=======================================================================
(defun fn-Paint-Board (board)
	(format t "~%__ _
			~% [~a]==================[~a]==================[~a]
			~% | |                  | |                  | |
	        ~% | |     [~a]==========[~a]===========[~a]    | |
	        ~% | |     | |          | |           | |    | |
	        ~% | |     | |      [~a]-[~a]-[~a]       | |    | |
	        ~% [~a]=====[~a]======[~a]     [~a]======[~a]=====[~a]
	        ~% | |     | |      [~a]-[~a]-[~a]       | |    | |
	        ~% | |     | |          | |           | |    | |
	        ~% | |     [~a]==========[~a]===========[~a]    | |
	        ~% | |                  | |                   | |
	        ~% [~a]==================[~a]==================[~a]~%"
		(or (nth 0 	board) "0") (or (nth 1 	board) "1") (or (nth 2 	board) "2") (or (nth 3 	board) "3")
		(or (nth 4 	board) "4") (or (nth 5 	board) "5") (or (nth 6 	board) "6") (or (nth 7 	board) "7")
		(or (nth 8 	board) "8") (or (nth 9 	board) "9") (or (nth 10 board) "A") (or (nth 11 board) "B")
		(or (nth 12 board) "C") (or (nth 13 board) "E") (or (nth 14 board) "G") (or (nth 15 board) "H")
		(or (nth 16 board) "I") (or (nth 17 board) "R") (or (nth 18 board) "S") (or (nth 19 board) "U")
		(or (nth 20 board) "V") (or (nth 21 board) "W") (or (nth 22 board) "Y") (or (nth 23 board) "Z")
	)
)

;;=======================================================================
;; df-Paint-Board
;; Le da un formato correcto a la casilla seleccionada
;;=======================================================================
(defun fn-Formato-Casilla (casilla)
	(cond 	((equal casilla 'A) (setq casilla 10))
  			((equal casilla 'B) (setq casilla 11))
  			((equal casilla 'C) (setq casilla 12))
  			((equal casilla 'E) (setq casilla 13))
  			((equal casilla 'G) (setq casilla 14))
  			((equal casilla 'H) (setq casilla 15))
  			((equal casilla 'I) (setq casilla 16))
  			((equal casilla 'R) (setq casilla 17))
  			((equal casilla 'S) (setq casilla 18))
  			((equal casilla 'U) (setq casilla 19))
  			((equal casilla 'V) (setq casilla 20))
  			((equal casilla 'W) (setq casilla 21))
  			((equal casilla 'Y) (setq casilla 22))  			
  			((equal casilla 'Z) (setq casilla 23)) )
	casilla
)

;;=======================================================================
;; fn-Realizar-Movimiento
;; Posiciona el símbolo en la casilla
;;=======================================================================
(defun fn-Realizar-Movimiento (board simbolo casilla)
	(when (typep casilla 'integer)
		(when (<= casilla (length board))
			(let 	((casilla-a-ocupar (nth casilla board))
					(board-aux (copy-list board)))
					(when (not casilla-a-ocupar)
						(setf (nth casilla board-aux) simbolo)
						board-aux))) ))

;;=======================================================================
;; fn-Pintar_Tablero?
;; Revisa si existe algún nulo en el tablero
;;=======================================================================
(defun fn-Pintar_Tablero? (board)
	(not (member nil board))
)

;;=======================================================================
;; fn-Gano?
;; Función para averiguar si alguien gana
;;=======================================================================
(defun fn-Gano? (board simbolo)
  (or 	(and 
	     	(equal (nth 0 board) simbolo)  ;fila top
		   	(equal (nth 1 board) simbolo)
		   	(equal (nth 2 board) simbolo))
	     
		(and 
			(equal (nth 3 board) simbolo) ;fila middle top
			(equal (nth 4 board) simbolo) 
			(equal (nth 5 board) simbolo))

		(and 
			(equal (nth 6 board) simbolo) ;fila center top
			(equal (nth 7 board) simbolo) 
			(equal (nth 8 board) simbolo))

		(and 
			(equal (nth 9 board) simbolo) ;fila mitad izquierda
			(equal (nth 10 board) simbolo) 
			(equal (nth 11 board) simbolo))
		(and 
			(equal (nth 12 board) simbolo) ;fila mitad derecha
			(equal (nth 13 board) simbolo) 
			(equal (nth 14 board) simbolo))
		(and 
			(equal (nth 15 board) simbolo) ;fila centro abajo
			(equal (nth 16 board) simbolo) 
			(equal (nth 17 board) simbolo))
		(and 
			(equal (nth 18 board) simbolo) ;fila mitad abajo
			(equal (nth 19 board) simbolo) 
			(equal (nth 20 board) simbolo))
		(and 
			(equal (nth 21 board) simbolo) ;fila abajo abajo
			(equal (nth 22 board) simbolo) 
			(equal (nth 23 board) simbolo))
		(and 
			(equal (nth 0 board) simbolo) ;columna 1 exterior
			(equal (nth 9 board) simbolo) 
			(equal (nth 21 board) simbolo))
		(and 
			(equal (nth 3 board) simbolo) ;columna 2 mitad izquierda
			(equal (nth 10 board) simbolo) 
			(equal (nth 18 board) simbolo))
		(and 
			(equal (nth 6 board) simbolo) ;columna 3 core izquierda
			(equal (nth 11 board) simbolo) 
			(equal (nth 16 board) simbolo))

		(and 
			(equal (nth 1 board) simbolo) ;columna 4 mitad arriba
			(equal (nth 4 board) simbolo) 
			(equal (nth 7 board) simbolo))

		(and 
			(equal (nth 16 board) simbolo) ;columna 4 mitad abajo
			(equal (nth 19 board) simbolo) 
			(equal (nth 22 board) simbolo))

		(and 
			(equal (nth 8 board) simbolo) ;columna 5 core derecha
			(equal (nth 12 board) simbolo) 
			(equal (nth 17 board) simbolo))

		(and 
			(equal (nth 5 board) simbolo) ;columna 6 mitad derecha
			(equal (nth 13 board) simbolo) 
			(equal (nth 20 board) simbolo))

		(and 
			(equal (nth 2 board) simbolo) ;columna 7 extremo derecha
			(equal (nth 14 board) simbolo) 
			(equal (nth 23 board) simbolo))
		)
)

;;=======================================================================
;; fn-Evaluacion
;; Función para evaluar el estado del tablero 
;;=======================================================================
(defun fn-Evaluacion (board simbolo)
    (cond 	((fn-Gano? board simbolo)
				(+ 1 (/ 1 (length (remove nil board)))))
    		((fn-Gano? board (fn-Opuesto simbolo))
				(- (+ 1 (/ 1 (length (remove nil board))))))
		  	(t 0)) )

;;=======================================================================
;; fn-Movimienos
;; Función para generar todos los estados 
;;=======================================================================
(defun fn-Movimienos (board simbolo)
	(loop for casilla from 0 to 23 unless (nth casilla board) ;Unless: true cuando son nil
	 	collect (fn-Realizar-Movimiento board simbolo casilla)))

;;=======================================================================
;; fn-Opuesto
;; Función regresar el símbolo opuesto al enviado como argumento 
;;=======================================================================
(defun fn-Opuesto (simbolo)
  (if (equal simbolo *player-simbolo*) *pc-simbolo* *player-simbolo*))

;;=======================================================================
;; fn-Negamax
;; Función Negamax con poda
;;=======================================================================	
(defun fn-Negamax(board depth maxDepth alfa beta simbolo)
  (cond (  (or (fn-Pintar_Tablero? board) (fn-Gano? board *pc-simbolo*) (fn-Gano? board *player-simbolo*) (>= depth maxDepth));  (deep-enough board depth)
         	(cons (fn-Evaluacion board simbolo) board)
        )
        (t (let ( 	(mejor-movimiento nil)
                 	(mejor-valor *menusINFINITY*)
                 	(sucesores (fn-Movimienos board simbolo))
                 	(stop nil))
        	
        	;Para cada operador
			(loop for sucesor in sucesores until stop do
				(let* ((nuevo-estado (fn-Negamax sucesor (+ depth 1) maxDepth (* -1 beta) (* -1 (max alfa mejor-valor)) (fn-Opuesto simbolo)))
			          (valor (* -1 (first nuevo-estado))) )

				(when (> valor mejor-valor)
					(setf mejor-valor valor)
					(setf mejor-movimiento sucesor))
				(when (>= mejor-valor beta)
					(setq stop t)) ))
			(cons mejor-valor mejor-movimiento))) ))

;;=======================================================================
;; fn-Gato
;; Función principal
;;=======================================================================	
(defun fn-Gato ()
	(let ( 	(board (fn-Init-Board))
			(board-aux nil)
			(casilla nil)
			(alfa *menusINFINITY*)
        	(beta *INFINITY*))
		
		(do ()
			;#|stop cuando alguien gana|#
			( 	(or (fn-Gano? board *pc-simbolo*) (fn-Gano? board *player-simbolo*) (fn-Pintar_Tablero? board))
		        	(fn-Paint-Board board)
			        (cond 	((fn-Gano? board *pc-simbolo*) (format t "Gala la PC.~%"))
			               	((fn-Gano? board *player-simbolo*) (format t "Ganaste.~%"))
			               	(t  (format t "Fue empate.~%"))) )

			;Si nadie ha ganado, entonces:
			(fn-Paint-Board board)

			;Tira el jugador: +++++++++++++++++++++
			(print "Turno del jugador: ")
			(print "----")
			(setf casilla (read))
			(setf casilla (fn-Formato-Casilla casilla)) 

			;Verifica que la casilla seleccionada no se encuentre ocupada
			(loop until (setq board-aux (fn-Realizar-Movimiento board *player-simbolo* casilla))
				do 
				(format t "~%La casilla ~a esta ocupada o no existe~%" casilla)
				(format t "Selecciona una casilla: ")
				(print "----")
				(setf casilla (read))) 

			(setf board board-aux)
			;++++++++++++++++++++++++++++++++++++++

			;Tira la PC: --------------------------
			;Se revisa si alguien ha ganado o si aun existen casillas vacias para poder pintar el talero
			(when (and (not (fn-Gano? board *pc-simbolo*)) (not (fn-Gano? board *player-simbolo*)) (not (fn-Pintar_Tablero? board)) )
				;(print "AUN NADIE GANA O SE PUEDE TIERAR MAS MOVIMIENTOS")
				(format t "Movimiento jugador: ~%")
				(fn-Paint-Board board)
				(setf board (rest (fn-NegaMax board 0 *max-depth* alfa beta *pc-simbolo*)))
				(format t "Movimiento PC: ~%") 
				)) 
		))
			; -------------------------------------

(fn-Gato)