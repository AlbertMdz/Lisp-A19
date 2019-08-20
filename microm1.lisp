;Alberto Mendoza López
;problema # 2:
;;Comenzar con 10 herbívoros en posiciones al azar sobre el
;;mapa de ríos. Sin reproducirse, conservar un mínimo de 7
;;vivos durante 50 días de simulación. No debe ahogarse
;;ningún animal…
(defrule start
	:group
		:initialization
	:when
	:do
		(set-entities :herbivore 10 :desert))

(defrule comer-pasto
	:group
			:all
	:when
			(not(search-cell @cell1
				(equal (get-cell-type @cell1) :water)))
			(view-field-vision @id1
				(or (equal (get-entity-type @id1) :grass) (equal (get-entity-type @id1) :bush)))
	:do
			(move-entity-to @id (get-entity-coordinates @id1))
			(feed-entity @id @id1))

(defrule comer-pasto-al-rededor
	:group
			:all
	:when
			(view-field-vision @id1 
				(or (equal (get-entity-type @id1) :grass) (equal (get-entity-type @id1) :bush))
				(<= (manhattan-distance @cell (get-entity-coordinates @id1)) 1))
	:do
			(move-entity-to @id (get-entity-coordinates @id1))
			(feed-entity @id @id1))

(defrule tomar-agua
	:group
			:all
	:when
			(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
			(search-cell @cell1
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					(area-around @cell1 :water))
	:do
			(move-entity-to @id @cell1)
			(drink-water @id))

(defrule moverse-este
	:group
			:herbivores
	:when
			(< (- (get-type-movements :herbivore) (get-entity-movements @id)) 2)
			(equal (find-cell-type @cell :water :east 1) nil)
	:do
			(move-entity @id :east 1))

(defrule moverse-norte
	:group
			:herbivores
	:when
			(< (- (get-type-movements :herbivore) (get-entity-movements @id)) 2)
			(equal (find-cell-type @cell :water :north 1) nil)
	:do
			(move-entity @id :north 1))

(defrule moverse-oeste
	:group
			:herbivores
	:when
			(< (- (get-type-movements :herbivore) (get-entity-movements @id)) 2)
			(equal (find-cell-type @cell :water :west 1) nil)
	:do
			(move-entity @id :west 1))

(defrule moverse-sur
	:group
			:herbivores
	:when
			(< (- (get-type-movements :herbivore) (get-entity-movements @id)) 2)
			(equal (find-cell-type @cell :water :south 1) nil)
	:do
			(move-entity @id :south 1))

(defrule comer-pasto-otra-vez
	:group
			:herbivores
	:when
			(not(search-cell @cell1
				(equal (get-cell-type @cell1) :water)))
			(view-field-vision @id1
				(or (equal (get-entity-type @id1) :grass) (equal (get-entity-type @id1) :bush)))
	:do
			(move-entity-to @id (get-entity-coordinates @id1))
			(feed-entity @id @id1))

(defrule comer-pasto-al-rededor-otra-vez
	:group
			:herbivores
	:when
			(view-field-vision @id1 
				(or (equal (get-entity-type @id1) :grass) (equal (get-entity-type @id1) :bush))
				(<= (manhattan-distance @cell (get-entity-coordinates @id1)) 1))
	:do
			(move-entity-to @id (get-entity-coordinates @id1))
			(feed-entity @id @id1))

(defrule tomar-agua-otra-vez
	:group
			:herbivores
	:when
			(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
			(search-cell @cell1
					(or (equal (get-cell-type @cell1) :desert) 
					    (equal (get-cell-type @cell1) :grass))
					(area-around @cell1 :water))
	:do
			(move-entity-to @id @cell1)
			(drink-water @id))























