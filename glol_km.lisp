s ;; Alberto Mendoza López
;; Reglas KNOWLEDGE MACHINE para el problema GLOL
(in-package :km)
(km)
(every thing has
  (posicion ()))

(*Lobo has
  (instance-of (thing))
  (posicion ("origen")))

(*Oveja has
  (instance-of (thing))
  (posicion ("origen")))

(*Granjero has
  (instance-of (thing))
  (posicion ("origen")))

(*Comida has
  (instance-of (thing))
  (posicion ("origen")))

(*Orilla-origen has
  (items (*Lobo *Oveja *Granjero *Comida )))

(*Orilla-destino has
  (items ((a thing) (a thing) (a thing) (a thing))))

(Last has (ultimo ( (a thing ))))


(reglas has (

  (regla1 ( (if ( (the posicion of *Granjero) = (the posicion of *Oveja)  ) and ( (the ultimo of Last) /= *Oveja )
  then ( (*Oveja now-has (posicion ("destino"))) and ( (Last now-has (ultimo (*Oveja))) ) )  ) ) )

  (regla2 ( (if ( (the posicion of *Granjero) = "destino") and ( (oneof (the items of *Orilla-origen) where (t)  ) )
  then ( (*Granjero now-has (posicion ("origen")) ) and (Last now-has (ultimo (nil))) )  ) ) )

  (regla3 ( (if ( (the posicion of *Granjero) = "origen" ) and ((oneof (the items of *Orilla-origen) where (t) ))
  then ( (?x == (oneof (the items of *Orilla-origen) where (t) )) and (?x now-has (posicion ("destino")) ) and (*Granjero now-has (posicion ("destino")) )
    and (Last now-has ( ultimo (?x)))))))
  )
)