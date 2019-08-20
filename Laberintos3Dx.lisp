;Alberto Mendoza López.
;Laberientos en 3 dimensiones.
(load "maze_lib.lisp")
(add-algorithm 'breadth-first)
(add-algorithm 'depth-first)
(add-algorithm 'bestfs)
(add-algorithm 'a-estrella)
;Cargamos libreria y los algoritmos

(defparameter *data-maze* nil) 
(defparameter *maze-rows* nil) 
(defparameter *maze-cols* nil) 
(defparameter  *open* '()) 
(defparameter  *memory* '())   
(defparameter  *id*  -1)  
(defparameter  *current-ancestor*  nil) 
(defparameter  *solution*  nil)  
(defparameter *altura* 0)
;Variables globales
(defun reinicio ()
  (setq *data-maze* (slot-value *maze* 'data)) 
  (setq *maze-rows* (1- (get-maze-rows))) 
  (setq *maze-cols* (1- (get-maze-cols)))
  (setq *open* nil)
  (setq *memory* nil)
  (setq *id* 0)
  (setq *current-ancestor* nil)
  (setq *solution* nil)
  (setq *altura* 0)
)
;Funcion que se ejecura al inicio de una busqueda para cualquier algoritmo
;Obtenemos informacion del laberinto
;Preparamos las variables globales para comenzar una nueva busqueda

(defparameter *ops* '(
                      (:derecha (0 1))
                      (:derecha-bajo-puente (0 1))
                      (:derecha-sobre-puente (0 1))
                      (:abajo (1 0))
                      (:abajo-bajo-puente (1 0))
                      (:abajo-sobre-puente (1 0))
                      (:izquierda (0 -1))
                      (:izquierda-bajo-puente (0 -1))
                      (:izquierda-sobre-puente (0 -1))
                      (:arriba (-1 0))
                      (:arriba-bajo-puente (-1 0))
                      (:arriba-sobre-puente (-1 0))  ))

(defun misMovimientos (numero)

  (case numero
    (0 '(0 0 0 0))
    (1 '(0 0 0 1))
    (2 '(0 0 1 0))
    (3 '(0 0 1 1))
    (4 '(0 1 0 0))
    (5 '(0 1 0 1))
    (6 '(0 1 1 0))
    (7 '(0 1 1 1))
    (8 '(1 0 0 0))
    (9 '(1 0 0 1))
    (10 '(1 0 1 0))
    (11 '(1 0 1 1))
    (12 '(1 1 0 0))
    (13 '(1 1 0 1))
    (14 '(1 1 1 0))
    (15 '(1 1 1 1))
    (16 '(1 0 0 0 0)) ;puente izquierda-derecha
    (17 '(1 0 0 0 1)));puente arriba-abajo
);Casos para representar Movimiento a traves del laberinto

(defun etiquetaMov (etiqueta)

  (case etiqueta
    (:diagonal-arriba-derecha 1)
    (:diagonal-abajo-derecha 3)
    (:diagonal-abajo-izquierda 5)
    (:diagonal-arriba-izquierda 7)
    (:derecha 2)
    (:derecha-bajo-puente 2)
    (:derecha-sobre-puente 2)
    (:abajo 4)
    (:abajo-bajo-puente 4)
    (:abajo-sobre-puente 4)
    (:izquierda 6)
    (:izquierda-bajo-puente 6)
    (:izquierda-sobre-puente 6)
    (:arriba 0)
    (:arriba-bajo-puente 0)
    (:arriba-sobre-puente 0)  )
)
;ayuda visual para leer solución

(defun obtieneSolucion ()

  (let ((aux nil))
    (loop for i from 1 to (1- (length *solution*)) do
      (format t "~a~%" (fourth (nth i *solution*)))
      (setq aux (append aux (list (etiquetaMov (fourth (nth i *solution*))))))
      )
  aux
  )
)
;esta función se actualiza conforme la función RastreaSolucion va actualizando *solution*.

(defun validaPuenteArrAba (fila columna)

  (if (= 17 (aref *data-maze* fila columna))
    (return-from validaPuenteArrAba t)
    (return-from validaPuenteArrAba nil))
)

(defun validaPuenteIzqDer (fila columna)

(if (= 16 (aref *data-maze* fila columna))
    (return-from validaPuenteIzqDer t)
    (return-from validaPuenteIzqDer nil))

)

(defun validaPuente (fila columna)

  (if (or (= 16 (aref *data-maze* fila columna))
      (= 17 (aref *data-maze* fila columna)))
      (return-from validaPuente t) (return-from validaPuente nil))
)

(defun validaUsoOperador (op pos-actual)

  (let ((fila-actual 0) 
        (columna-actual 0) 
        (fila-nueva-posicioninicio 0) 
        (columna-nueva-posicioninicio 0)
        (movimiento-posicion-actual nil) 
        (movimiento-nueva-posicioninicio nil) 
        (etiqueta nil))

    
    (setq etiqueta (first op))
    (setq fila-actual (aref pos-actual 0))
    (setq columna-actual (aref pos-actual 1))
    (setq fila-nueva-posicioninicio (+ (aref pos-actual 0) (first (second op))))
    (setq columna-nueva-posicioninicio (+ (aref pos-actual 1) (second (second op))))
    (setq movimiento-posicion-actual (misMovimientos (aref *data-maze* fila-actual columna-actual)))

    (if (and (>= fila-nueva-posicioninicio 0) (<= fila-nueva-posicioninicio *maze-rows*) (>= columna-nueva-posicioninicio 0) (<= columna-nueva-posicioninicio *maze-cols*) )
      (setq movimiento-nueva-posicioninicio (misMovimientos (aref *data-maze* fila-nueva-posicioninicio columna-nueva-posicioninicio)))
      (return-from validaUsoOperador nil))
    
    (case etiqueta
      (:diagonal-arriba-derecha
        (if (and (>= fila-nueva-posicioninicio 0 ) (<= columna-nueva-posicioninicio *maze-cols*) (not (validaPuente fila-actual columna-actual)) (not (validaPuente fila-actual columna-nueva-posicioninicio)) (not (validaPuente fila-nueva-posicioninicio columna-actual)) (not (validaPuente fila-nueva-posicioninicio columna-nueva-posicioninicio))
         (or (and (= (nth 2 movimiento-posicion-actual) 0) (= (nth 1 movimiento-nueva-posicioninicio) 0)) (and (= (nth 3 movimiento-posicion-actual) 0) (= (nth 0 movimiento-nueva-posicioninicio) 0)) )  ) t))

      (:diagonal-abajo-derecha
        (if (and (<= fila-nueva-posicioninicio *maze-rows* ) (<= columna-nueva-posicioninicio *maze-cols*) (not (validaPuente fila-actual columna-actual)) (not (validaPuente fila-actual columna-nueva-posicioninicio)) (not (validaPuente fila-nueva-posicioninicio columna-actual)) (not (validaPuente fila-nueva-posicioninicio columna-nueva-posicioninicio))
        (or (and (= (nth 1 movimiento-posicion-actual) 0) (= (nth 0 movimiento-nueva-posicioninicio) 0)) (and (= (nth 2 movimiento-posicion-actual) 0) (= (nth 3 movimiento-nueva-posicioninicio) 0)) )  ) t) )

      (:diagonal-abajo-izquierda
        (if (and (<= fila-nueva-posicioninicio *maze-rows* ) (>= columna-nueva-posicioninicio 0) (not (validaPuente fila-actual columna-actual)) (not (validaPuente fila-actual columna-nueva-posicioninicio)) (not (validaPuente fila-nueva-posicioninicio columna-actual)) (not (validaPuente fila-nueva-posicioninicio columna-nueva-posicioninicio))
        (or (and (= (nth 1 movimiento-posicion-actual) 0) (= (nth 2 movimiento-nueva-posicioninicio) 0)) (and (= (nth 0 movimiento-posicion-actual) 0) (= (nth 3 movimiento-nueva-posicioninicio) 0)) )  ) t) )

      (:diagonal-arriba-izquierda
        (if (and (>= fila-nueva-posicioninicio 0 ) (>= columna-nueva-posicioninicio 0) (not (validaPuente fila-actual columna-actual)) (not (validaPuente fila-actual columna-nueva-posicioninicio)) (not (validaPuente fila-nueva-posicioninicio columna-actual)) (not (validaPuente fila-nueva-posicioninicio columna-nueva-posicioninicio))
        (or (and (= (nth 3 movimiento-posicion-actual) 0) (= (nth 2 movimiento-nueva-posicioninicio) 0)) (and (= (nth 0 movimiento-posicion-actual) 0) (= (nth 1 movimiento-nueva-posicioninicio) 0)) )  ) t) )

      (:arriba
        (if (and (>= fila-nueva-posicioninicio 0) (= (nth 3 movimiento-posicion-actual) 0) (not (validaPuente fila-actual columna-actual)) )t))
      (:arriba-bajo-puente
        (if (and (>= fila-nueva-posicioninicio 0) (= (nth 3 movimiento-posicion-actual) 0) (validaPuenteIzqDer fila-actual columna-actual) (= (aref pos-actual 3) 0) )t))
      (:arriba-sobre-puente
        (if (and (>= fila-nueva-posicioninicio 0) (= (nth 3 movimiento-posicion-actual) 0) (validaPuenteArrAba fila-actual columna-actual) (= (aref pos-actual 3) 1) )t))

      (:derecha
        (if (and (<= columna-nueva-posicioninicio *maze-cols*) (= (nth 2 movimiento-posicion-actual) 0) (not (validaPuente fila-actual columna-actual)) )t))
      (:derecha-bajo-puente
        (if (and (<= columna-nueva-posicioninicio *maze-cols*) (= (nth 2 movimiento-posicion-actual) 0) (validaPuenteArrAba fila-actual columna-actual) (= (aref pos-actual 3) 0) )t))
      (:derecha-sobre-puente
        (if (and (<= columna-nueva-posicioninicio *maze-cols*) (= (nth 2 movimiento-posicion-actual) 0) (validaPuenteIzqDer fila-actual columna-actual) (= (aref pos-actual 3) 1) )t))

      (:abajo
        (if (and (<= fila-nueva-posicioninicio *maze-rows*) (= (nth 1 movimiento-posicion-actual) 0) (not (validaPuente fila-actual columna-actual)) )t))
      (:abajo-bajo-puente
        (if (and (<= fila-nueva-posicioninicio *maze-rows*) (= (nth 1 movimiento-posicion-actual) 0) (validaPuenteIzqDer fila-actual columna-actual) (= (aref pos-actual 3) 0) )t))
      (:abajo-sobre-puente
        (if (and (<= fila-nueva-posicioninicio *maze-rows*) (= (nth 1 movimiento-posicion-actual) 0) (validaPuenteArrAba fila-actual columna-actual) (= (aref pos-actual 3) 1) )t))

      (:izquierda
        (if (and (>= columna-nueva-posicioninicio 0) (= (nth 0 movimiento-posicion-actual) 0) (not (validaPuente fila-actual columna-actual)))t))
      (:izquierda-bajo-puente
        (if (and (>= columna-nueva-posicioninicio 0) (= (nth 0 movimiento-posicion-actual) 0) (validaPuenteArrAba fila-actual columna-actual) (= (aref pos-actual 3) 0) )t))
      (:izquierda-sobre-puente
        (if (and (>= columna-nueva-posicioninicio 0) (= (nth 0 movimiento-posicion-actual) 0) (validaPuenteIzqDer fila-actual columna-actual) (= (aref pos-actual 3) 1) )t))
    (otherwise nil)
    )
  )
)
;De acuerdo con la dupla [fila actual columna actual] se pregunta si el siguiente operador a analizar es valido.
;*pos-actual* lleva esta dupla (fila columna).
;consideramos la evaluación de las casillas especiales del caso Puente.

(defun costo (new-pos old-pos)
  (let ((costo 0))
    (setq costo (+ (* (- (aref new-pos 0) (aref old-pos 0) ) (- (aref new-pos 0) (aref old-pos 0) ) )
    (* (- (aref new-pos 1) (aref old-pos 1)) (- (aref new-pos 1) (aref old-pos 1)) ) ) )
    costo
  )
)
;Con ayuda de la antigua posición y la nueva determinamos un valor al movimiento siguiente.

(defun aptitud (posicion &optional (star nil) (old-pos nil))
  
  (let ((aptitud 0))
    (setq aptitud (+ (* (- (aref posicion 0) (aref *goal* 0) ) (- (aref posicion 0) (aref *goal* 0) ) )
     (* (- (aref posicion 1) (aref *goal* 1)) (- (aref posicion 1) (aref *goal* 1)) ) ) )
     (if star (setq aptitud (+ aptitud (costo posicion old-pos)))) 
     aptitud)
)
;Funcion de actitud con posición, un indicador para el caso bestfs y estrella asi como la antigua posicion

(defun pruebaOperador (op pos-actual &optional (star nil))

  (let ((nueva-pos nil) (mov-reng 0) (mov-col 0) (etiqueta nil) )

    (setq etiqueta (first op)) 
    (setq nueva-pos (make-array 4)) 

    (setq mov-reng (first (second op))) 
    (setq mov-col (second (second op))) 
    (setf (aref nueva-pos 0) (+ (aref pos-actual 0) mov-reng)) 
    (setf (aref nueva-pos 1) (+ (aref pos-actual 1) mov-col)) 

    
    (setf (aref nueva-pos 2) (aptitud nueva-pos star pos-actual))
    
    (cond
      ((and (>= (aref nueva-pos 0) 0) (<= (aref nueva-pos 0) *maze-rows*) (>= (aref nueva-pos 1) 0) (<= (aref nueva-pos 1) *maze-cols*) (validaPuenteIzqDer (aref nueva-pos 0) (aref nueva-pos 1))
        (or (eql etiqueta :derecha) (eql etiqueta :izquierda) (eql etiqueta :derecha-sobre-puente) (eql etiqueta :derecha-bajo-puente) (eql etiqueta :izquierda-sobre-puente) (eql etiqueta :izquierda-bajo-puente) ) ) (setf (aref nueva-pos 3) 1) )
      ((and (>= (aref nueva-pos 0) 0) (<= (aref nueva-pos 0) *maze-rows*) (>= (aref nueva-pos 1) 0) (<= (aref nueva-pos 1) *maze-cols*) (validaPuenteArrAba (aref nueva-pos 0) (aref nueva-pos 1))
        (or (eql etiqueta :arriba) (eql etiqueta :abajo) (eql etiqueta :arriba-sobre-puente) (eql etiqueta :arriba-bajo-puente) (eql etiqueta :abajo-sobre-puente) (eql etiqueta :abajo-bajo-puente) ) ) (setf (aref nueva-pos 3) 1) )
    )

  nueva-pos
  )
)
;Nueva posición al aplicar el ordenador selecionado.

(defun  creaNodo (estado  op)
  (incf  *id*)  
  (list  *id*  estado  *current-ancestor*  (first op)) )
;Función para devolver los siguientes parametros de busqueda al aplicar un operador asi como el estado donde se encuentra.

(defun reordenaOpen ()
  (let ((open-aux nil) (aptitudes nil) (aux nil))

    
    (loop for nodo in *open* do
      (setq aptitudes (append aptitudes (list (aref (second nodo) 2) ) ) ))

    
    (setq aptitudes (sort aptitudes #'<))

    
    (loop for aptitud in aptitudes do
      (loop for nodo in *open* do
        (when (and (equal aptitud (aref (second nodo) 2)) (not (member aptitud aux)))
            (setq open-aux (append open-aux (list nodo)) ) )
      )
      (setq aux (append aux (list aptitud)))
    )
    (setq *open* open-aux)
  )
)
;Considerando las aptitudes se ordenan los estados

(defun actualizaOpen (nodo)
  
  (let ((edo-nodo nil) (costo-nodo nil) (edo-open nil) (costo-open nil) (flag nil))

    (when (null *open*) (push nodo *open*) (return-from actualizaOpen nil) )
    (setq edo-nodo (second nodo)) 
    (setq costo-nodo (aref (second nodo) 2)) 
    
    (loop for i from 0 to (1- (length *open*)) do
      (setq edo-open (second (nth i *open*)))
      (setq costo-open (aref (second (nth i *open*)) 2))

      (cond

        ((and (equalp edo-nodo edo-open) (< costo-nodo costo-open) )
          (setf (nth i *open*) nodo) (setq flag t)) 

        ( (and (equalp edo-nodo edo-open) (>= costo-nodo costo-open)) (setq flag t)) 
      )
    )
    (if (not flag) (push nodo *open*)) 
  )
)
;Función que recibe nodo que es una lista de sucesores y evaluando si el estado de los sucesores ya se encontraba en *open*.
;Obtenermos la actualización de *open* pues nuestra función se comporta de forma destructiva.

(defun  buscaEstadoLista  (estado  lista-memoria)

     (cond ((null  lista-memoria)  Nil)
          ((equalp  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el primer nodo de la memoria?
    (T  (buscaEstadoLista  estado  (rest  lista-memoria))))  ) ;;Busca en el resto de la memoria
;Función para recordar nuestros intentos previos.

(defun  filtraListaMemoria (lista-estados-y-ops)

     (cond ((null  lista-estados-y-ops)  Nil)
         ((buscaEstadoLista (first (first  lista-estados-y-ops)) *memory*);; si se recuerda el primer elemento de la lista, filtrarlo...
         (filtraListaMemoria  (rest  lista-estados-y-ops)))
    (T  (cons  (first lista-estados-y-ops) (filtraListaMemoria  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta
;Cuando expandamos la solución necesitaremos una lista de estados y operadores, mejor nos aseguramos de que *memory* no lleve nada extra.

(defun insertaNodoOpen (estado  op  metodo)

(let ((nodo  (creaNodo  estado  op)))
     (cond ((eql  metodo :depth-first)
     (push  nodo  *open*))
     ((eql  metodo :breadth-first)
     (setq *open*  (append  *open*  (list nodo))))
     ((eql metodo :bestfs)      
      (when (and  (not (buscaEstadoLista estado *open*))  (not (buscaEstadoLista estado *memory*)) )
        (push nodo *open*) (reordenaOpen)) )
      ((eql metodo :star)      
        (when (not (buscaEstadoLista estado *memory*))
          (actualizaOpen nodo) (reordenaOpen)))
     (T  Nil)))  )
;Función para insertar nodos en la frontera de busqueda

(defun recuperaOpen ()
  (pop  *Open*))
;Función que llamamos para solicitar el siguiente elemento de *open*

(defun expand (estado &optional (star nil))

  (let ((descendientes  nil)
     (nuevo-estado  nil))
     (dolist  (op  *ops*  descendientes)
        (setq  nuevo-estado  (pruebaOperador  op estado star))
      (when (validaUsoOperador  op  estado)
     (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))))
;Función para expander la solución y encontrar decencientes ordenados.

(defun rastreaSolucion (nodo)

     (labels ((locate-node  (id  lista)
      (cond ((null  lista)  Nil)
            ((eql  id  (first (first  lista))) (first  lista)) 
            (T  (locate-node  id (rest  lista)))))) 

    (let ((current  (locate-node  (first  nodo)  *memory*))) 
       (loop  while  (not (null  current))  do  
     (push  current  *solution*)     
     (setq  current  (locate-node  (third  current) *memory*))))  
       *solution*))
;Función que con los datos almacenados en *memory* rastrea los decencientes desde la solución hasta el estado inicial

(defun breadth-first ()

  (reinicio)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (meta-aux (make-array 2))
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :breadth-first))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))

   (insertaNodoOpen   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (recuperaOpen)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (cond
       
        ((equalp  *goal*  meta-aux)
           (rastreaSolucion  nodo)
           (setq aux-sol (obtieneSolucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))
     
          (t (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado))
            (setq  sucesores  (filtraListaMemoria  sucesores))
            (loop for  element  in  sucesores  do
            (insertaNodoOpen  (first element)  (second element)  metodo))))))

)

(defun depth-first ()

  (reinicio)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (meta-aux (make-array 2))
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :depth-first))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))

   (insertaNodoOpen   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (recuperaOpen)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (cond
       
        ((equalp  *goal*  meta-aux)
           (rastreaSolucion  nodo)
           (setq aux-sol (obtieneSolucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))

      
          (t (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado))
            (setq  sucesores  (filtraListaMemoria  sucesores))
            (loop for  element  in  sucesores  do
            (insertaNodoOpen  (first element)  (second element)  metodo))))))

)

(defun bestfs ()

  (reinicio)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (meta-aux (make-array 2)) 
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :bestfs))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))
  (setf (aref pos-actual 2) (aptitud pos-actual))

   (insertaNodoOpen   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (recuperaOpen)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (cond
       
        ((equalp  *goal*  meta-aux)
           (rastreaSolucion  nodo)
           (setq aux-sol (obtieneSolucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))
      
          (t (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado))
            (setq  sucesores  (filtraListaMemoria  sucesores))
            (loop for  element  in  sucesores  do
              (insertaNodoOpen  (first element)  (second element)  metodo))))))
)

(defun a-estrella ()

  (reinicio)
  (let ((nodo nil)
  (aux-sol nil)
  (estado nil)
  (meta-aux (make-array 2)) 
  (sucesores  '())
  (operador  nil)
  (meta-encontrada  nil)
  (pos-actual (make-array 4))
  (metodo :star))

  (setf (aref pos-actual 0) (aref *start* 0))
  (setf (aref pos-actual 1) (aref *start* 1))
  (setf (aref pos-actual 2) (aptitud pos-actual))

   (insertaNodoOpen   pos-actual  nil  metodo)
   (loop until  (or  meta-encontrada
   (null *open*))  do

     (setq  nodo    (recuperaOpen)
     estado  (second  nodo)
     operador  (third  nodo))
     (setf (aref meta-aux 0) (aref estado 0))
     (setf (aref meta-aux 1) (aref estado 1))
     (push  nodo  *memory*)
     (incf *altura*)
     (cond
       
        ((equalp  *goal*  meta-aux)
           (rastreaSolucion  nodo)
           (setq aux-sol (obtieneSolucion))
           (setq *solution* aux-sol)
           (format t "Solución encontrada ~a~% " aux-sol)
          (setq  meta-encontrada  T))

          (t (setq  *current-ancestor*  (first  nodo))
          (setq  sucesores  (expand estado t))
            (setq  sucesores  (filtraListaMemoria  sucesores))
            (loop for  element  in  sucesores  do
              (insertaNodoOpen  (first element)  (second element)  metodo))))))
)

(start-maze)