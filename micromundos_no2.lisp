;Alberto Mendoza López
;problema # 2:
;Comenzar con 3 animales de cada especie, en posiciones
;;al azar de cualquier mapa. Ninguna especie se debe
;;extinguir durante 50 días. Sin restricciones…

(defrule inicio
	:group
			:initialization
	:when	
	:do
			(set-entities :herbivore 3 :desert)
			(set-entities :carnivore 3 :desert)
			(set-entities :scavenger 3 :desert)
)
;======================================================================
;    Herbivoros
;======================================================================
(defrule herbivoros-dividen
	:group
		:herbivores
	:when T
		;(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		;(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))		
	:do	
		(reproduce-entity @id))


(defrule herbivoros-comen
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(< (get-entity-food @id) 100)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-entity-consume @id))
					(> (get-entity-food @id1) 20)
			)
			(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)
			(feed-entity @id @id1))

(defrule herbivoros-evitaCadaver
	:group
			:herbivores
	:when	
			(equal (get-entity-cell-type @id) :contamination)
			(search-cell-lim @cell1 2
					(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
	:do
			(move-entity-to @id @cell1 :diagonal))

(defrule moverse
	:group
			:herbivores
	:when	
			(search-cell @cell1
					(not (equal @cell1 @cell))
					(> (manhattan-distance @cell1 @cell) 1)
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					)
			(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
			
	:do
			(move-entity-to @id @cell2 :diagonal)) 

;======================================================================
;   Carnivoro
;======================================================================

(defrule carnivoros-divide
	:group
		:carnivores
	:when	
		(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))		
	:do
		(reproduce-entity @id))


(defrule carnivoros-cazanCarroñeros
	:group
			:carnivores
	:when				
			(< (get-entity-food @id) 50)
			(view-field-vision @id1
				(equal  (get-entity-type @id) :scavengers))
	:do
			(move-entity-to @id (get-entity-coordinates @id1) :diagonal)
			(feed-entity @id @id1))

(defrule carnivoros-cazanHerbivoros
	:group
			:carnivores
	:when			
			(< (get-entity-food @id) 50)
			(view-field-vision @id1
				(equal  (get-entity-type @id) :herbivores))
	:do
			(move-entity-to @id (get-entity-coordinates @id1) :diagonal)
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

;======================================================================
;   Carroñeros
;======================================================================
(defrule carroñero-divide
	:group
		:scavengers
	:when	
		(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
		(< (get-entity-vision-type-size @id :scavenger) 25)
	:do
		(reproduce-entity @id))

(defrule carroñero-cazaCadaver
	:group
			:scavengers
	:when	
			(> (get-entity-movements @id) 1)
			(view-field-vision @id1
					(in (get-entity-type @id1) (get-entity-consume @id))
					(< (manhattan-distance @cell (get-entity-coordinates @id1)) 6))
	:do
			(move-entity-to @id (get-entity-coordinates @id1) :diagonal)
			(feed-entity @id @id1))

(defrule carroñero-evitaContaminacion
	:group
			:scavengers
	:when	
			(equal (get-entity-cell-type @id) :contamination)
			(search-cell-lim @cell1 2
					(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
	:do
			(move-entity-to @id @cell1 :diagonal))

(defrule buscar-aguaMapa
	:group
			:all
	:when	
			(not(search-cell @cell1
				(equal (get-cell-type @cell1) :water)))
			(search-distant-cell @cell1
				(equal (get-cell-type @cell1) :grass))
	:do
			(move-entity-to @id @cell1 :diagonal))

(defrule beber
	:group
			:all
	:when	
			(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
			(search-cell @cell1
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					(area-around @cell1 :water))
	:do
			(move-entity-to @id @cell1 :diagonal)
			(drink-water @id))

