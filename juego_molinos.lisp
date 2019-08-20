;;Alberto Mendoza López
;;AGENTE JUGARDOR DEL MOLINO DE LOS 9
(in-package :cl-user)
(defparameter black 1)
(defparameter red -1)
(defparameter c-vacia 0)
(defparameter negro-elimina 2)
(defparameter rojo-elimina -2)
(defparameter num-piezas 9)
(defparameter casillas 24)
(defparameter maximo-val 1000)
(defparameter minimo-val -1000)
;;;;;;;;;;; CONSTANTES DEL JUEGO

(defparameter *vecinos-casilla*
 (make-array `(,casillas)
	      :initial-contents  '((1 9);; 0
				   (0 2 4)       ;; 1
				   (1 14)        ;; 2
				   (10 4)        ;; 3
				   (1 3 5 7)     ;; 4
				   (4 13)        ;; 5
				   (11 7)        ;; 6
				   (4 6 8)       ;; 7
				   (7 12)        ;; 8
				   (0 10 21)     ;; 9
				   (3 9 11 18)   ;; 10
				   (6 10 15)     ;; 11
				   (8 13 17)     ;; 12
				   (5 12 14 20)  ;; 13
				   (2 13 23)     ;; 14
				   (11 16)       ;; 15
				   (15 19 17)    ;; 16
				   (12 16)       ;; 17
				   (10 19)       ;; 18
				   (16 18 20 22) ;; 19
				   (13 19)       ;; 20
				   (9 22)        ;; 21
				   (19 21 23)    ;; 22
				   (14 22))))    ;; 23
;;LISTA DE ADYACENCIA DE LAS CASILLAS EN EL TABLERO

(defparameter *molinos*
 (make-array `(,casillas)
	      :initial-contents  '(((0 1 2) (0 9 21));; 0
				   ((0 1 2) (1 4 7))          ;; 1
				   ((0 1 2) (2 14 23))        ;; 2
				   ((3 4 5) (3 10 18))        ;; 3
				   ((3 4 5) (1 4 7))          ;; 4
				   ((3 4 5) (5 13 20))        ;; 5
				   ((6 7 8) (6 11 15))        ;; 6
				   ((6 7 8) (1 4 7))          ;; 7
				   ((6 7 8) (8 12 17))        ;; 8
				   ((9 10 11) (0 9 21))       ;; 9
				   ((9 10 11) (3 10 18))      ;; 10
				   ((9 10 11) (6 11 15))      ;; 11
				   ((12 13 14) (8 12 17))     ;; 12
				   ((12 13 14) (5 13 20))     ;; 13
				   ((12 13 14) (2 14 23))     ;; 14
				   ((15 16 17) (6 11 15))     ;; 15
				   ((15 16 17) (16 19 22))    ;; 16
				   ((15 16 17) (8 12 17))     ;; 17
				   ((18 19 20) (3 10 18))     ;; 18
				   ((18 19 20) (16 19 22))    ;; 19
				   ((18 19 20) (5 13 20))     ;; 20
				   ((21 22 23) (0 9 21))      ;; 21
				   ((21 22 23) (16 19 22))    ;; 22
				   ((21 22 23) (2 14 23)))))  ;; 23
;;TRES EN LINEA DEFINIDOS DEL MISMO MODO QUE EL JUEGO DEL GATO

(defun tablero (game)(first game))
(defun (setf tablero) (val game) (setf (first game) val))

(defun situacion (game)(second game))
(defun (setf situacion) (val game) (setf (second game) val))

(defun turno (game) (if (or (= (situacion game) black) (= (situacion game) negro-elimina)) black red))

(defun pieza-nojugada (game player) (if (= player red) (third game) (fourth game)))
(defun (setf pieza-nojugada) (val game player) (if (= player red) (setf (third game) val) (setf (fourth game) val)))

(defun pieza-eliminada (game player) (if (= player red) (fifth game) (sixth game)))
(defun (setf pieza-eliminada) (val game player) (if (= player red) (setf (fifth game) val) (setf (sixth game) val)))

(defun depth (game) (seventh game))
(defun (setf depth) (val game) (setf (seventh game) val))

(defun pos (game p) (aref (tablero game) p))
(defun (setf pos) (val game p) (setf (aref (tablero game) p) val))

(defun fase-dos ()
  (list (make-array `(,casillas) :element-type 'fixnum :initial-element c-vacia)
	black num-piezas num-piezas 0 0 0))

(defun respaldo-juego (game)  
  (cons (copiar-tablero (tablero game))
	(copy-list (rest game))))
;;FUNCIONES BASICAS DEL JUEGO

(defun vecinos-casilla (p)  
  (aref *vecinos-casilla* p))

(defun adyacente-siono (p1 p2)  
  (member p2 (vecinos-casilla p1)))
;;DETERMINAR LA ADYACENCIA DE UNA CASILLA RESPECTO DE OTRA (MOVER)

(defun molinos (p)  
  (aref *molinos* p))

(defun 3enlinea-ocurre (player molino-posicion game)  
  (and (= (pos game (first molino-posicion)) player)
       (= (pos game (second molino-posicion)) player)
       (= (pos game (third molino-posicion)) player)))
  
(defun tresenlinea-consecuencia (player p game)  
  (let ((m (molinos p)))
    (or (3enlinea-ocurre player (first m) game)
	(3enlinea-ocurre player (second m) game))))

(defun movimientos-tablero (game &optional (count nil))
   (let ((bag (if count 0 nil)))
    (cond ((= (situacion game) black)       
	   (if (> (pieza-nojugada game black) 0)  
	       (dotimes (p casillas)        
		 (if (= (pos game p) c-vacia)
		     (if count (incf bag)
			 (let ((g (respaldo-juego game)))
			   (setf (pos g p) black)
			   (decf (pieza-nojugada g black))
			   (if (tresenlinea-consecuencia black p g)
			       (setf (situacion g) negro-elimina)
			       (setf (situacion g) red))
			   (push g bag)))))
	       
		      (dotimes (p casillas)        
			(if (= (pos game p) black)
			    (dolist (pp (vecinos-casilla p))
			      (if (= (pos game pp) c-vacia)
				  (if count (incf bag)
				      (let ((g (respaldo-juego game)))
					(rotatef (pos g p) (pos g pp))
					(if (tresenlinea-consecuencia black pp g)
					    (setf (situacion g) negro-elimina)
					    (setf (situacion g) red))
					(push g bag)))))))))

	  ((= (situacion game) negro-elimina)  
	   (dotimes (p casillas)
	     (if (= (pos game p) red)
		 (if count (incf bag)
		     (let ((g (respaldo-juego game)))
		       (setf (pos g p) c-vacia)
		       (setf (situacion g) red)
		       (incf (pieza-eliminada g red))
		       (push g bag))))))
	   
	  ((= (situacion game) red)       
	   (if (> (pieza-nojugada game red) 0)  
	       (dotimes (p casillas)        
		 (if (= (pos game p) c-vacia)
		     (if count (incf bag)
			 (let ((g (respaldo-juego game)))
			   (setf (pos g p) red)
			   (decf (pieza-nojugada g red))
			   (if (tresenlinea-consecuencia red p g)
			       (setf (situacion g) rojo-elimina)
			       (setf (situacion g) black))
			   (push g bag)))))
	       
	       (dotimes (p casillas)        
		 (if (= (pos game p) red)
		     (dolist (pp (vecinos-casilla p))
		       (if (= (pos game pp) c-vacia)
			   (if count (incf bag)
			       (let ((g (respaldo-juego game)))
				 (rotatef (pos g p) (pos g pp))
				 (if (tresenlinea-consecuencia red pp g)
				     (setf (situacion g) rojo-elimina)
				     (setf (situacion g) black))
				 (push g bag)))))))))
	  
	  ((= (situacion game) rojo-elimina)  
	   (dotimes (p casillas)
	     (if (= (pos game p) black)
		 (if count (incf bag)
		     (let ((g (respaldo-juego game)))
		       (setf (pos g p) c-vacia)
		       (setf (situacion g) black)
		       (incf (pieza-eliminada g black))
		       (push g bag)))))))
    bag))
  
(defun game-over (game)  
  (if (>= (pieza-eliminada game black) (- num-piezas 2))
  red 
      (if (>= (pieza-eliminada game red) (- num-piezas 2))  
	  black
	  (if (= 0 (movimientos-tablero game t))  
	      (if (= (situacion game) red)   
		  black
		  red)
	      nil))))

(defun imprimir-tablero (game)
  (apply #'format t "~%~a~% No. Negras: ~a  No.Rojas: ~a~%~a--------~a--------~a    [ 0]==================[ 1]==================[ 2]~%|        |        |    | |                   | |                   | |~%|  ~a-----~a-----~a  |    | |     [ 3]==========[ 4]===========[ 5]   | |~%|  |     |     |  |    | |     | |           | |            | |    | |~%|  |  ~a--~a--~a  |  |    | |     | |      [ 6]-[ 7]-[ 8]      | |    | |~%~a--~a--~a     ~a--~a--~a    [ 9]====[10]=====[11]      [12]=====[13]====[14]~%|  |  ~a--~a--~a  |  |    | |     | |      [15]-[16]-[17]      | |    | |~%|  |     |     |  |    | |     | |           | |            | |    | |~%|  ~a-----~a-----~a  |    | |     [18]==========[19]===========[20]   | | ~%|        |        |    | |                   | |                   | |~%~a--------~a--------~a    [21]==================[22]==================[23]~%"
	 (cond ((game-over game) (if (= (game-over game) black) "El juego ha terminado,ganan las fichas NEGRAS" "El juego ha terminado,ganan las fichas ROJAS"))
	       ((= (situacion game) black) 
		(if (> (pieza-nojugada game black) 0) "Negro agrega pieza" "Mueve Negro"))
	       ((= (situacion game) red) 
		(if (> (pieza-nojugada game red) 0) "Rojo agrega pieza" "Mueve Rojo"))
	       ((= (situacion game) negro-elimina) "Negro ha formado un molino, ahora remueve")
	       ((= (situacion game) rojo-elimina) "Rojo ha formado un molino, ahora remueve"))
	 (- num-piezas (pieza-eliminada game black)) (- num-piezas (pieza-eliminada game red))
	 (map 'list #'(lambda (elt) (if (= elt black) #\@ (if (= elt red) #\O #\+))) (tablero game))))

(defun cambiar-bando (stream string &rest args)
  (apply #'format stream string args)
  (finish-output stream))

(defun mover-jugador (game &optional depth bandera)  
  (declare (ignore depth))
  (declare (ignore bandera))
  
  (cambiar-bando t "~%Juegas con ~a" (if (= (situacion game) black) "Negras (@)" "Rojas (O)"))
  (let (3enlinea-activado g)
    (if (> (pieza-nojugada game (situacion game)) 0) 
	(loop
	   (cambiar-bando t "~%Coloca en la casilla -> ")
	   (let ((p (read)))
	     (when (or (eq p 'Q) (eq p 'q)) (format t "Break...") (break))
	     (when (and (numberp p) (>= p 0) (< p casillas) (= (pos game p) c-vacia))
	       (setf g (respaldo-juego game))
	       (setf (pos g p) (situacion game))
	       (decf (pieza-nojugada g (situacion game)))
	       (setf 3enlinea-activado (tresenlinea-consecuencia (situacion game) p g))
	       (return)))
	   (cambiar-bando t "No es posible colocar la pieza en esa casilla, [Salir]  q~%"))
	(loop                                 
	   (cambiar-bando t "~%Mueve pieza en la cassilla -> ")
	   (let ((p (read)))
	     (when (or (eq p 'Q) (eq p 'q)) (format t "Break") (break))
	     (when (and (numberp p) (>= p 0) (< p casillas) (= (pos game p) (situacion game)))  ;; I can move from there
	       (cambiar-bando t "A la casilla -> ")
	       (let ((p2 (read)))
		 (when (or (eq p 'Q) (eq p 'q)) (format t "Break") (break))
		 (when (and (numberp p2)
			    (>= p2 0) 
			    (< p2 casillas) 
			    (= (pos game p2) c-vacia)
			    (adyacente-siono p p2))
		   (setf g (respaldo-juego game))
		   (setf (pos g p) c-vacia)
		   (setf (pos g p2) (situacion game))
		   (setf 3enlinea-activado (tresenlinea-consecuencia (situacion game) p2 g))
		   (return)))))
	   (cambiar-bando t "Los valores para mover esta pieza no son correctos, [Salir]   q~%")))
    (when 3enlinea-activado                   
      (loop
	 (cambiar-bando t "~%Quita la pieza en la casilla -> ")
	 (let ((p (read)))
	   (when (or (eq p 'Q) (eq p 'q)) (format t "Break") (break))
	   (when (and (numberp p) (>= p 0) (< p casillas) (= (pos g p) (if (= (situacion g) black) red black)))
	     (setf g (respaldo-juego g))
	     (setf (pos g p) c-vacia)
	     (incf (pieza-eliminada g (if (= (situacion g) black) red black)))
	     (return)))
	 (cambiar-bando t "No es posible remover la pieza en la casilla seleccionada, [Salir]   q~%")))
    (setf (situacion g) (if (= (situacion g) black) red black))
    g))

(defun copiar-tablero (array)  
    (let ((dims (array-dimensions array)))
    (adjust-array
    (make-array dims :element-type (array-element-type array) :displaced-to array)
    dims)))

(defun mejor-jugada (elementos funcion-evaluacion) 
  (when elementos
    (let ((probability-count 1)
	  (mejor-jugada (first elementos))
	  (valor-mejor-elemento (funcall funcion-evaluacion (first elementos))))
      (dolist (element (rest elementos))
	(let ((valor-elemento (funcall funcion-evaluacion element)))
	  (cond ((> valor-elemento valor-mejor-elemento)
		 (setf probability-count 1)
		 (setf mejor-jugada element)
		 (setf valor-mejor-elemento valor-elemento))
		((= valor-elemento valor-mejor-elemento)
		 (incf probability-count)
		 (when (<= (random 1.0) (/ 1 probability-count))
		   (setf mejor-jugada element)
		   (setf valor-mejor-elemento valor-elemento))))))
      mejor-jugada)))

(defun partida (black-func red-func &key (black-depth 20) (red-depth nil) (bandera t) (max-turns 100))  
  (unless red-depth (setf red-depth black-depth))
  (let ((game (fase-dos)) (turno 0))
    (loop
       (incf turno)
       (when (> turno max-turns)
	 (return-from partida 0))
       (when bandera (imprimir-tablero game))
       (let ((over (game-over game)))
	 (when over
	   (when bandera (format t (cond ((equalp over black) "~%~%Negro gana")
					 ((equalp over red) "~%~%Rojo gana")
					 (t "~%~%Empate ~a ~a ~a" red black over))))
	   (return-from partida over)))
       (setf game (funcall black-func game black-depth bandera))
       (when bandera (imprimir-tablero game))
       (let ((over (game-over game)))
	 (when over
	   (when bandera (format t (cond ((equalp over black) "~%~%Negro gana")
					 ((equalp over red) "~%~%Rojo gana")
					 (t "~%~%Empate ~a ~a ~a" red black over))))
           (return-from partida over)))
       (setf game (funcall red-func game red-depth bandera)))))

(defun jugar-molinos-1a1 ()
  (partida #'mover-jugador #'mover-jugador
		   :bandera t))

(defun jugar-molinos (func jugador-negras &key (computer-depth 9))
  (partida (if jugador-negras #'mover-jugador func)
	     (if jugador-negras func #'mover-jugador)
	     :bandera t
	     :black-depth computer-depth
	     :red-depth computer-depth))

(defun jugar-torneo (black-func red-func &key (black-depth 6) (red-depth 6) (bandera t))
  (let (round-1 round-2 round-3)

    (format t "~%~%~%~%-----------------------~a Juega con las negras-----------------------~%~%~%~%" black-func)
    (terpri)
    (setf round-1 (partida black-func red-func :black-depth black-depth :red-depth red-depth :bandera bandera))
    (format t "~%~%~%~%------------------------~a Juega con las negras------------------------~%~%~%~%" red-func)
    (terpri)
    (setf round-2 (partida red-func black-func :black-depth red-depth :red-depth black-depth :bandera bandera))
    (format t "~%~%~%~%------------------------~a Juega con las negras------------------------~%~%~%~%" red-func)
    (terpri)
    (setf round-3 (partida red-func black-func :black-depth red-depth :red-depth black-depth :bandera bandera))    
    (format t "~%~%~%~%------------------------ ~a --------------------------~%"	    
		(format t "~a Gana!" (if (= round-1 round-2 black) black-func red-func)))
    nil))

(export '(copiar-tablero mejor-jugada black red c-vacia negro-elimina rojo-elimina num-piezas casillas maximo-val minimo-val turno tablero situacion pieza-nojugada pieza-eliminada depth pos fase-dos respaldo-juego 
	  vecinos-casilla adyacente-siono molinos 3enlinea-ocurre tresenlinea-consecuencia movimientos-tablero game-over imprimir-tablero mover-jugador 
	  partida jugar-molinos jugar-torneo))

;;PARTIDA ENTRE DOS HUMANOS QUE ESCRIBEN EN EL TECLADO
;;;;;;;;;;;;;PARA PODER JUGAR UN UNO A UNO HUMANO VS HUMANO COMPILAR HASTA ESTA LINEA DE CODIGO
;;(jugar-molinos-1a1)









(defpackage :heuristicafuerte 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MOVER-AGENTE-JUGADOR"))

(in-package :heuristicafuerte)
(defun alpha-beta (game current-depth max-depth
		   evalua-maximo-enturno expand terminal-p evaluar-minvsmax
		   alpha beta)

  (if (or (funcall terminal-p game) (> current-depth max-depth))
      (funcall evaluar-minvsmax game evalua-maximo-enturno)      
      (if (funcall evalua-maximo-enturno game)	  
	  (progn
	  (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf alpha (max alpha (alpha-beta child (incf current-depth) max-depth evalua-maximo-enturno expand terminal-p evaluar-minvsmax alpha beta))))
	  (if (>= alpha beta) beta alpha))	 
	  (progn
	    (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf beta (min beta (alpha-beta child (incf current-depth) max-depth evalua-maximo-enturno expand terminal-p evaluar-minvsmax alpha beta))))
	    (if (>= alpha beta) alpha beta)))))

(defun evaluar-minvsmax (game evalua-maximo-enturno)  
  (let ((end (game-over game)))
    (if (null end) 
	(let* (
	       (valor-final 0)
	      (piezas-agente 0)
	      (piezas-jugador 0)
	      (molinos-agente 0)
	      (molinos-jugador 0)
	      (piezas-adyacentes 0)
	      (color-agente (if (funcall evalua-maximo-enturno game) (turno game) (if (= (turno game) red) red black)))
	      (theirColor (if (= color-agente red) black red))
	      )
	  
	  (loop for lugar-entablero from 0 to 23 do
	      (if (= (pos game lugar-entablero) color-agente) (incf piezas-agente))

	       (if (= (pos game lugar-entablero) theirColor) (incf piezas-jugador))

	       (loop for each in (molinos lugar-entablero) do
		    (if (3enlinea-ocurre color-agente each game) (incf molinos-agente))
		    (if (3enlinea-ocurre theirColor each game) (incf molinos-jugador)))

	       (if (or (= (pos game lugar-entablero) color-agente) (= (pos game lugar-entablero) c-vacia))
		   (loop for each in (vecinos-casilla lugar-entablero) do
			(if (= (pos game each) color-agente) 
			    (progn
			    (incf piezas-adyacentes))))))
	  
	  
	  (decf valor-final (* 6 piezas-jugador))
	  (incf valor-final (* 1.5 piezas-agente))
	  (incf valor-final (* 1.5 molinos-agente))
	  (decf valor-final (* 2 molinos-jugador))
	  (incf valor-final (* 2 piezas-adyacentes))
	     valor-final)
	(if (= 0 end)  
	    0
	    (* end (turno game) maximo-val (if (funcall evalua-maximo-enturno game) 1 -1))))))

(defun mover-agente-jugador (game depth &optional bandera)  
  (let ((max (turno game)))
    (let ((move (mejor-jugada (movimientos-tablero game)
			     (lambda (g)
			       (alpha-beta g 0 depth 
					   (lambda (gm) (= (turno gm) max)) 
					   (lambda (gm) (movimientos-tablero gm)) 
					   (lambda (gm) (game-over gm))
					   #'evaluar-minvsmax 
					   minimo-val
					   maximo-val)))))
      (when (or (= (situacion move) negro-elimina)
	      (= (situacion move) rojo-elimina))  
	  (when bandera (imprimir-tablero move))
	  (setf move (mejor-jugada (movimientos-tablero move)
				  (lambda (g)
				    (alpha-beta g 0 depth 
						(lambda (gm) (= (turno gm) max)) 
						(lambda (gm) (movimientos-tablero gm)) 
						(lambda (gm) (game-over gm)) 
						#'evaluar-minvsmax 
						minimo-val
						maximo-val)))))
      move)))

      ;;(jugar-molinos #'heuristicafuerte:mover-agente-jugador t)

  (defpackage :heuristica2 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MOVER-AGENTE-JUGADOR2"))

(in-package :heuristica2)
(defun alpha-beta (game current-depth max-depth
		   evalua-maximo-enturno expand terminal-p evaluar-minvsmax
		   alpha beta)

  (if (or (funcall terminal-p game) (> current-depth max-depth))
      (funcall evaluar-minvsmax game evalua-maximo-enturno)      
      (if (funcall evalua-maximo-enturno game)	  
	  (progn
	  (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf alpha (max alpha (alpha-beta child (incf current-depth) max-depth evalua-maximo-enturno expand terminal-p evaluar-minvsmax alpha beta))))
	  (if (>= alpha beta) beta alpha))	 
	  (progn
	    (loop for child in (funcall expand game) until (>= alpha beta) do
	      (setf beta (min beta (alpha-beta child (incf current-depth) max-depth evalua-maximo-enturno expand terminal-p evaluar-minvsmax alpha beta))))
	    (if (>= alpha beta) alpha beta)))))

(defun evaluar-minvsmax (game evalua-maximo-enturno)  
  (let ((end (game-over game)))
    (if (null end) 
	(let* (
	       (valor-final 0)
	      (piezas-agente 0)
	      (piezas-jugador 0)
	      (molinos-agente 0)
	      (molinos-jugador 0)
	      (piezas-adyacentes 0)
	      (color-agente (if (funcall evalua-maximo-enturno game) (turno game) (if (= (turno game) red) red black)))
	      (theirColor (if (= color-agente red) black red))
	      )
	  
	  (loop for lugar-entablero from 0 to 23 do
	      (if (= (pos game lugar-entablero) color-agente) (incf piezas-agente))

	       (if (= (pos game lugar-entablero) theirColor) (incf piezas-jugador))

	       (loop for each in (molinos lugar-entablero) do
		    (if (3enlinea-ocurre color-agente each game) (incf molinos-agente))
		    (if (3enlinea-ocurre theirColor each game) (incf molinos-jugador)))

	       (if (or (= (pos game lugar-entablero) color-agente) (= (pos game lugar-entablero) c-vacia))
		   (loop for each in (vecinos-casilla lugar-entablero) do
			(if (= (pos game each) color-agente) 
			    (progn
			    (incf piezas-adyacentes))))))
	  
	  
	  (decf valor-final (* 6 piezas-jugador))
	  (incf valor-final (* 1.5 piezas-agente))
	  (incf valor-final (* 1.5 molinos-agente))
	  (decf valor-final (* 2 molinos-jugador))
	  (incf valor-final (* 2 piezas-adyacentes))
	     valor-final)
	(if (= 0 end)  
	    0
	    (* end (turno game) maximo-val (if (funcall evalua-maximo-enturno game) 1 -1))))))

(defun mover-agente-jugador2 (game depth &optional bandera)  
  (let ((max (turno game)))
    (let ((move (mejor-jugada (movimientos-tablero game)
			     (lambda (g)
			       (alpha-beta g 0 depth 
					   (lambda (gm) (= (turno gm) max)) 
					   (lambda (gm) (movimientos-tablero gm)) 
					   (lambda (gm) (game-over gm))
					   #'evaluar-minvsmax 
					   minimo-val
					   maximo-val)))))
      (when (or (= (situacion move) negro-elimina)
	      (= (situacion move) rojo-elimina))  
	  (when bandera (imprimir-tablero move))
	  (setf move (mejor-jugada (movimientos-tablero move)
				  (lambda (g)
				    (alpha-beta g 0 depth 
						(lambda (gm) (= (turno gm) max)) 
						(lambda (gm) (movimientos-tablero gm)) 
						(lambda (gm) (game-over gm)) 
						#'evaluar-minvsmax 
						minimo-val
						maximo-val)))))
      move)))

;;Juego del molino de los 9 donde el jugador negras, comienza y se enfrenta a una heuristica fuerte 
;;(jugar-molinos #'heuristicafuerte:mover-agente-jugador t)
;;Juego del molino de los 9 donde el jugador negras comienza y se enfrenta a una heuristica alternativa un tanto inferior a la primera.
;;(jugar-molinos #'heuristica2:mover-agente-jugador2 t)
;;Juego del molino de los 9 donde 2 heuristicas juegan al mejor de 2 de 3 partidas ([heuristica 1 juega negras][heuristica 2 juega rojas])
;;(jugar-torneo #'heuristicafuerte:mover-agente-jugador #'heuristica2:mover-agente-jugador2)


(jugar-molinos #'heuristicafuerte:mover-agente-jugador t)