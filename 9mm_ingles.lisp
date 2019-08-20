;;Alberto Mendoza López
;;AGENTE JUGARDOR DEL MOLINO DE LOS 9
(in-package :cl-user)
(defparameter black 1)
(defparameter red -1)
(defparameter empty 0)
(defparameter black-removes 2)
(defparameter red-removes -2)
(defparameter num-pieces 9)
(defparameter num-positions 24)
(defparameter max-wins 1000)
(defparameter min-wins -1000)
;;;;;;;;;;; CONSTANTES DEL JUEGO

(defun board (game)(first game))
(defun (setf board) (val game) (setf (first game) val))

(defun situation (game)(second game))
(defun (setf situation) (val game) (setf (second game) val))
(defun turn (game) (if (or (= (situation game) black) (= (situation game) black-removes)) black red))

(defun unplayed (game player) (if (= player red) (third game) (fourth game)))
(defun (setf unplayed) (val game player) (if (= player red) (setf (third game) val) (setf (fourth game) val)))

(defun removed (game player) (if (= player red) (fifth game) (sixth game)))
(defun (setf removed) (val game player) (if (= player red) (setf (fifth game) val) (setf (sixth game) val)))

(defun depth (game) (seventh game))
(defun (setf depth) (val game) (setf (seventh game) val))

(defun pos (game p) (aref (board game) p))
(defun (setf pos) (val game p) (setf (aref (board game) p) val))

(defun make-game ()
  (list (make-array `(,num-positions) :element-type 'fixnum :initial-element empty)
	black num-pieces num-pieces 0 0 0))

(defun copy-game (game)  
  (cons (copy-array (board game))
	(copy-list (rest game))))
;;FUNCIONES BASICAS DEL JUEGO


(defparameter *adjacent-positions*
 (make-array `(,num-positions)
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

(defun adjacent-positions (p)  
  (aref *adjacent-positions* p))

(defun adjacent-p (p1 p2)  
  (member p2 (adjacent-positions p1)))
;;DETERMINAR LA ADYACENCIA DE UNA CASILLA RESPECTO DE OTRA (MOVER)
(defparameter *mills*
 (make-array `(,num-positions)
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

(defun mills (p)  
  (aref *mills* p))

(defun mill-filled-p (player mill game)  
  (and (= (pos game (first mill)) player)
       (= (pos game (second mill)) player)
       (= (pos game (third mill)) player)))
  
(defun mill-formed-p (player p game)  
  (let ((m (mills p)))
    (or (mill-filled-p player (first m) game)
	(mill-filled-p player (second m) game))))

(defun moves (game &optional (count nil))
   (let ((bag (if count 0 nil)))
    (cond ((= (situation game) black)       
	   (if (> (unplayed game black) 0)  
	       (dotimes (p num-positions)        
		 (if (= (pos game p) empty)
		     (if count (incf bag)
			 (let ((g (copy-game game)))
			   (setf (pos g p) black)
			   (decf (unplayed g black))
			   (if (mill-formed-p black p g)
			       (setf (situation g) black-removes)
			       (setf (situation g) red))
			   (push g bag)))))
	       
		      (dotimes (p num-positions)        
			(if (= (pos game p) black)
			    (dolist (pp (adjacent-positions p))
			      (if (= (pos game pp) empty)
				  (if count (incf bag)
				      (let ((g (copy-game game)))
					(rotatef (pos g p) (pos g pp))
					(if (mill-formed-p black pp g)
					    (setf (situation g) black-removes)
					    (setf (situation g) red))
					(push g bag)))))))))

	  ((= (situation game) black-removes)  
	   (dotimes (p num-positions)
	     (if (= (pos game p) red)
		 (if count (incf bag)
		     (let ((g (copy-game game)))
		       (setf (pos g p) empty)
		       (setf (situation g) red)
		       (incf (removed g red))
		       (push g bag))))))
	   
	  ((= (situation game) red)       
	   (if (> (unplayed game red) 0)  
	       (dotimes (p num-positions)        
		 (if (= (pos game p) empty)
		     (if count (incf bag)
			 (let ((g (copy-game game)))
			   (setf (pos g p) red)
			   (decf (unplayed g red))
			   (if (mill-formed-p red p g)
			       (setf (situation g) red-removes)
			       (setf (situation g) black))
			   (push g bag)))))
	       
	       (dotimes (p num-positions)        
		 (if (= (pos game p) red)
		     (dolist (pp (adjacent-positions p))
		       (if (= (pos game pp) empty)
			   (if count (incf bag)
			       (let ((g (copy-game game)))
				 (rotatef (pos g p) (pos g pp))
				 (if (mill-formed-p red pp g)
				     (setf (situation g) red-removes)
				     (setf (situation g) black))
				 (push g bag)))))))))
	  
	  ((= (situation game) red-removes)  
	   (dotimes (p num-positions)
	     (if (= (pos game p) black)
		 (if count (incf bag)
		     (let ((g (copy-game game)))
		       (setf (pos g p) empty)
		       (setf (situation g) black)
		       (incf (removed g black))
		       (push g bag)))))))
    bag))
  
(defun game-over (game)  
  (if (>= (removed game black) (- num-pieces 2))
  red 
      (if (>= (removed game red) (- num-pieces 2))  
	  black
	  (if (= 0 (moves game t))  
	      (if (= (situation game) red)   
		  black
		  red)
	      nil))))

(defun print-game (game)
  (apply #'format t "~%~a~% No. Negras: ~a  No.Rojas: ~a~%~a-----~a-----~a    [ 0]==================[ 1]==================[ 2]~%| ~a---~a---~a |    | |                   | |                   | |~%| | ~a-~a-~a | |    | |     [ 3]==========[ 4]===========[ 5]   | |~%~a-~a-~a   ~a-~a-~a    | |     | |           | |            | |    | |~%| | ~a-~a-~a | |    | |     | |      [ 6]-[ 7]-[ 8]      | |    | |~%| ~a---~a---~a |    [ 9]====[10]=====[11]      [12]=====[13]====[14]~%~a-----~a-----~a    | |     | |      [15]-[16]-[17]      | |    | |~%                 | |     | |           | |            | |    | |~%                 | |     [18]==========[19]===========[20]   | | ~%                 | |                   | |                   | |~%                 [21]==================[22]==================[23]~%"
	 (cond ((game-over game) (if (= (game-over game) black) "Black wins" "Red wins"))
	       ((= (situation game) black) 
		(if (> (unplayed game black) 0) "Negro agrega pieza" "Mueve Negro"))
	       ((= (situation game) red) 
		(if (> (unplayed game red) 0) "Rojo agrega pieza" "Mueve Rojo"))
	       ((= (situation game) black-removes) "Negro ha formado un molino, ahora remueve")
	       ((= (situation game) red-removes) "Rojo ha formado un molino, ahora remueve"))
	 (- num-pieces (removed game black)) (- num-pieces (removed game red))
	 (map 'list #'(lambda (elt) (if (= elt black) #\@ (if (= elt red) #\O #\+))) (board game))))

(defun flush-format (stream string &rest args)
  (apply #'format stream string args)
  (finish-output stream))

(defun mover-jugador (game &optional depth verbose)  
  (declare (ignore depth))
  (declare (ignore verbose))
  
  (flush-format t "~%Juegas con ~a" (if (= (situation game) black) "Negras (@)" "Rojas (O)"))
  (let (mill-formed g)
    (if (> (unplayed game (situation game)) 0) 
	(loop
	   (flush-format t "~%Coloca en la casilla -> ")
	   (let ((p (read)))
	     (when (or (eq p 'Q) (eq p 'q)) (format t "Break...") (break))
	     (when (and (numberp p) (>= p 0) (< p num-positions) (= (pos game p) empty))
	       (setf g (copy-game game))
	       (setf (pos g p) (situation game))
	       (decf (unplayed g (situation game)))
	       (setf mill-formed (mill-formed-p (situation game) p g))
	       (return)))
	   (flush-format t "No es posible colocar la pieza en esa casilla, [Salir]  q~%"))
	(loop                                 
	   (flush-format t "~%Mueve pieza en la cassilla -> ")
	   (let ((p (read)))
	     (when (or (eq p 'Q) (eq p 'q)) (format t "Break") (break))
	     (when (and (numberp p) (>= p 0) (< p num-positions) (= (pos game p) (situation game)))  ;; I can move from there
	       (flush-format t "A la casilla -> ")
	       (let ((p2 (read)))
		 (when (or (eq p 'Q) (eq p 'q)) (format t "Break") (break))
		 (when (and (numberp p2)
			    (>= p2 0) 
			    (< p2 num-positions) 
			    (= (pos game p2) empty)
			    (adjacent-p p p2))
		   (setf g (copy-game game))
		   (setf (pos g p) empty)
		   (setf (pos g p2) (situation game))
		   (setf mill-formed (mill-formed-p (situation game) p2 g))
		   (return)))))
	   (flush-format t "Los valores para mover esta pieza no son correctos, [Salir]   q~%")))
    (when mill-formed                   
      (loop
	 (flush-format t "~%Quita la pieza en la casilla -> ")
	 (let ((p (read)))
	   (when (or (eq p 'Q) (eq p 'q)) (format t "Break") (break))
	   (when (and (numberp p) (>= p 0) (< p num-positions) (= (pos g p) (if (= (situation g) black) red black)))
	     (setf g (copy-game g))
	     (setf (pos g p) empty)
	     (incf (removed g (if (= (situation g) black) red black)))
	     (return)))
	 (flush-format t "No es posible remover la pieza en la casilla seleccionada, [Salir]   q~%")))
    (setf (situation g) (if (= (situation g) black) red black))
    g))

(defun copy-array (array)  
    (let ((dims (array-dimensions array)))
    (adjust-array
    (make-array dims :element-type (array-element-type array) :displaced-to array)
    dims)))

(defun max-element (elements value-function) 
  (when elements
    (let ((probability-count 1)
	  (max-element (first elements))
	  (max-element-score (funcall value-function (first elements))))
      (dolist (element (rest elements))
	(let ((element-score (funcall value-function element)))
	  (cond ((> element-score max-element-score)
		 (setf probability-count 1)
		 (setf max-element element)
		 (setf max-element-score element-score))
		((= element-score max-element-score)
		 (incf probability-count)
		 (when (<= (random 1.0) (/ 1 probability-count))
		   (setf max-element element)
		   (setf max-element-score element-score))))))
      max-element)))

(defun play-game (black-func red-func &key (black-depth 20) (red-depth nil) (verbose t) (max-turns 100))  
  (unless red-depth (setf red-depth black-depth))
  (let ((game (make-game)) (turn 0))
    (loop
       (incf turn)
       (when (> turn max-turns)
	 (return-from play-game 0))
       (when verbose (print-game game))
       (let ((over (game-over game)))
	 (when over
	   (when verbose (format t (cond ((equalp over black) "~%~%Black Wins")
					 ((equalp over red) "~%~%Red Wins")
					 (t "~%~%Draw ~a ~a ~a" red black over))))
	   (return-from play-game over)))
       (setf game (funcall black-func game black-depth verbose))
       (when verbose (print-game game))
       (let ((over (game-over game)))
	 (when over
	   (when verbose (format t (cond ((equalp over black) "~%~%Black Wins")
					 ((equalp over red) "~%~%Red Wins")
					 (t "~%~%Draw ~a ~a ~a" red black over))))
           (return-from play-game over)))
       (setf game (funcall red-func game red-depth verbose)))))

(defun jugar-molinos-1a1 ()
  (play-game #'mover-jugador #'mover-jugador
		   :verbose t))

(defun jugar-molinos (func human-is-black &key (computer-depth 16))
  (play-game (if human-is-black #'mover-jugador func)
	     (if human-is-black func #'mover-jugador)
	     :verbose t
	     :black-depth computer-depth
	     :red-depth computer-depth))

(defun jugar-torneo (black-func red-func &key (black-depth 5) (red-depth 5) (verbose t))
  (let (round-1 round-2)

    (format t "~%~%~%~%-----------------------~a Juega con las negras-----------------------~%~%~%~%" black-func)
    (terpri)
    (setf round-1 (play-game black-func red-func :black-depth black-depth :red-depth red-depth :verbose verbose))
    (format t "~%~%~%~%------------------------~a Juega con las negras------------------------~%~%~%~%" red-func)
    (terpri)
    (setf round-2 (play-game red-func black-func :black-depth red-depth :red-depth black-depth :verbose verbose))
    
    (format t "~%~%~%~%------------------------ ~a --------------------------~%"
	    (if (= round-1 round-2) "Empate!"
		(format t "~a Gana!" (if (= round-1 black) black-func red-func))))
    nil))

(export '(copy-array max-element black red empty black-removes red-removes num-pieces num-positions
	  max-wins min-wins turn
	  board situation unplayed removed depth pos make-game copy-game 
	  adjacent-positions adjacent-p mills mill-filled-p mill-formed-p
	  moves game-over print-game mover-jugador 
	  play-game play-human-game jugar-molinos jugar-torneo))

;;PARTIDA ENTRE DOS HUMANOS QUE ESCRIBEN EN EL TECLADO
;;(jugar-molinos-1a1)
;;;;;;;;;;;;;PARA PODER JUGAR UN UNO A UNO HUMANO VS HUMANO COMPILAR HASTA ESTA LINEA DE CODIGO




(defpackage :heuristicafuerte 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :heuristicafuerte)
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

      ;;(jugar-molinos #'heuristicafuerte:make-computer-move t)

  (defpackage :heuristica2 
  (:use "COMMON-LISP" "COMMON-LISP-USER")
  (:export "MAKE-COMPUTER-MOVE"))

(in-package :heuristica2)
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
			       (alpha-beta g 12 depth 
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

      (jugar-molinos #'heuristica2:make-computer-move t)



