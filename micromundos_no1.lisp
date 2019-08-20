;Alberto Mendoza López
;problema # 2:
;;Comenzar con 10 herbívoros en posiciones al azar sobre el
;;mapa de ríos. Sin reproducirse, conservar un mínimo de 7
;;vivos durante 50 días de simulación. No debe ahogarse
;;ningún animal…


(defrule inicio
	:group
			:initialization
	:when	
	:do
		(set-entities :herbivore 10 :desert)
)

;======================================================================
;    Herbivoros
;======================================================================
(defrule buscar-aguaMapa
	:group
			:herbivores
	:when	
			(< (get-entity-water @id) 50)
			(not(search-cell @cell1
				(equal (get-cell-type @cell1) :water)))
			(search-distant-cell @cell1
				(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
	:do
			(move-entity-to @id @cell1 :orthogonal))

(defrule herbivoros-comen
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(< (get-entity-food @id) 100)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-entity-consume @id))
					(> (get-entity-food @id1) 1)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
			(move-entity-to @id (get-entity-coordinates @id1) :orthogonal)
			(feed-entity @id @id1))


(defrule carnovoros-evitarCadaver
	:group
			:carnivores
	:when	
			(equal (get-entity-cell-type @id) :contamination)
			(search-cell-lim @cell1 2
					(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
	:do
			(move-entity-to @id @cell1 :diagonal))

(defrule beber
	:group
			:herbivores
	:when	
			(< (get-entity-water @id) 20)
			(search-cell @cell1
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					(area-around @cell1 :water))
	:do
			(move-entity-to @id @cell1 :orthogonal)
			(drink-water @id))



(defrule moverse
	:group
			:herbivores
	:when	
			(search-cell @cell1
					(or (< (get-entity-water @id) 70) (< (get-entity-food @id) 70))
					(not (equal @cell1 @cell))
					(> (manhattan-distance @cell1 @cell) 1)
					(not (equal (get-cell-type @cell1) :contamination))
					(simulate-move @cell2 (get-entity-coordinates @id) @cell1  :orthogonal)	)
			
	:do
			(move-entity-to @id @cell2 :orthogonal))

