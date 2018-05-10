;;; ################################ MODULOS ################################

(defmodule MAIN (export ?ALL))
(defmodule RECOPILAR_INFO (import MAIN ?ALL)(export ?ALL))
(defmodule PROCESO_DATOS (import MAIN ?ALL)(export ?ALL))
(defmodule IMPRIMIR_SOL (import MAIN ?ALL)(export ?ALL))

;;; ################################ DEFTEMPLATES Y CLASES PROPIAS ################################

(defclass MAIN::Puntuaciones
	(is-a USER) (role concrete)
    (slot vivienda (type INSTANCE)(create-accessor read-write))
    (slot puntuacion (type INTEGER)(default 0)(create-accessor read-write))
    (multislot justificaciones (type STRING)(create-accessor read-write))
)

(deftemplate MAIN::solucion
	(multislot parcialmente (type INSTANCE))
	(multislot adecuado (type INSTANCE))
	(multislot recomendable (type INSTANCE))
)

(deftemplate MAIN::datos_cliente
    ; Caracteristicas cliente
	(slot edad (type STRING) (default ?NONE))
	(slot estudiante (type INTEGER) (default -1))
    
    (slot posX_Est_Trb (type INTEGER) (default -1))
    (slot posY_Est_Trb (type INTEGER) (default -1))
    
	(slot pareja (type INTEGER) (default -1))
    (slot hijos (type INTEGER) (default -1))
    
    ; Caracteristicas vivienda
	(slot precio_min (default -1))
	(slot precio_max (default -1))
	
	(slot mascotas (type INTEGER) (default -1))
	(slot vehiculo (type INTEGER) (default -1))
    
    (slot banyos (type INTEGER) (default -1))
    (slot ascensor (type INTEGER) (default -1))
	(slot dorm_sim (type INTEGER) (default -1))
	(slot dorm_dob (type INTEGER) (default -1))
    
    (slot superficie (type INTEGER) (default -1))
    (slot terraza (type INTEGER) (default -1))
    (slot balcon (type INTEGER) (default -1))
    
    (slot amueblada (type INTEGER) (default -1))
    (slot electrodomesticos (type INTEGER) (default -1))
	(slot piscina (type INTEGER) (default -1))
    
    (slot transporte (type INTEGER) (default -1))
    
    (multislot barrios (type INSTANCE))
    (multislot tipo_vivienda_pref (type INTEGER))
)

(deftemplate MAIN::restriccion
	(slot dato)
	(slot is_rest)
)


(defrule MAIN::Inicio
    (declare (salience 10))
    =>
    (printout t crlf)
    (printout t "##########################################################################")
    (printout t crlf)
    (printout t crlf)
    (format t "    ) )        /\\                                                        %n")
    (format t "   =====      /  \\                                                       %n")
    (format t "  _|___|_____/ __ \\____________                                          %n")
    (format t " |::::::::::/ |  | \\:::::::::::|                                         %n")
    (format t " |:::::::::/  ====  \\::::::::::|                                         %n")
    (format t " |::::::::/__________\\:::::::::|                                         %n")
    (format t " |_________|  ____  |__________|                                          %n")
    (format t "  | ______ | / || \ | _______ |                 BIENVENIDOS               %n")
    (format t "  ||  |   || ====== ||   |   ||                    A                      %n")
    (format t "  ||--+---|| |    | ||---+---||                  NUESTRA                  %n")
    (format t "  ||__|___|| |   o| ||___|___||               INMOBILIARIA!               %n")
    (format t "  |========| |____| |=========|                                           %n")
    (format t " (^^-^^^^^-|________|-^^^--^^^)                                           %n")
    (format t " (,, , ,, ,/________\\,,,, ,, ,)                                          %n")
    (format t "','',,,,' /__________\\,,,',',;;                                          %n")
    (printout t crlf)
    (printout t crlf)
    (printout t "##########################################################################")
    (printout t crlf)
    (printout t crlf)
    (printout t crlf)

    (focus RECOPILAR_INFO)
)
;;; ################################ FUNCIONES PARA HACER PREGUNTAS ################################

(deffunction RECOPILAR_INFO::ask-question (?question $?allowed-values)
"Escribe una pregunta y lee uno de los valores posibles (allowed-values)"
	(printout t ?question)
    (printout t "(si/s/no/n): ")
	(bind ?answer (read))
	(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	)
    (printout t crlf)
	(return ?answer)
)

(deffunction RECOPILAR_INFO::preguntar-valorconcreto (?question)
"Escribe una pregunta y lee un entero"
	(printout t ?question)
    (printout t "(NUMERO): ")
    (bind ?answer (read))
    (while (not (integerp ?answer)) do
        (printout t ?question)
        (printout t "(NUMERO): ")
        (bind ?answer (read))
    )
    (printout t crlf)
	(return ?answer)
)

(deffunction RECOPILAR_INFO::preguntar-valor (?question ?min ?incr)
"Dado un minimo y un incremento, muestra por pantalla los diferentes valores entre min y min+10*incr"
	(printout t ?question)
    (printout t crlf)
    (loop-for-count (?cnt1 1 10) do
        (printout t "- ")
		(printout t (+ (* ?cnt1 ?incr) ?min))
		(printout t crlf)
	)
    (printout t "Valor: ")
    (bind ?answer (read))
    (printout t crlf)
	return ?answer
)

(deffunction RECOPILAR_INFO::yes-or-no (?question)
"Hace una pregunta y espera como entrada yes/y/si/s/no/n"
    (bind ?response (ask-question ?question yes y si s no n))
    (if (or (eq ?response yes) (eq ?response y)(eq ?response si)(eq ?response s))
        then (return TRUE)
        else (return FALSE)
    )
)

(deffunction RECOPILAR_INFO::pregunta-multirespuesta (?question $?valores-posibles)
"Escribe una pregunta y espera uno o mas valores de entre los valores posibles (separados con espacio)"
    (printout t ?question)
    (printout t crlf)
    (progn$ (?var ?valores-posibles) 
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (printout t "Indica los numeros referentes a las preferencias separados por un espacio: ")
    (bind ?resp (readline))
    (bind ?numeros (str-explode ?resp))
    (bind $?lista (create$))
    (progn$ (?var ?numeros) 
        (if (and (integerp ?var) (and (>= ?var 0) (<= ?var (length$ ?valores-posibles))))
            then 
                (if (not (member$ ?var ?lista))
                    then (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?var))
                )
        ) 
    )
    (if (or(member$ 0 ?lista)(= (length$ ?lista) 0)) then (bind ?lista (create$ )))
    (printout t crlf)
    return ?lista
)

(deffunction PROCESO_DATOS::distancia (?a ?b)
"Calcula la distancia entre a (Vivienda) y b (Servicio)"
    (bind ?Ax (send ?a get-posicionX))
    (bind ?Ay (send ?a get-posicionY))
    (bind ?Bx (send ?b get-posX))
    (bind ?By (send ?b get-posY))
    (bind ?dist (+ (abs (- ?Ax ?Bx))(abs (- ?Ay ?By))))
    (if (< ?dist 40) then (return 0))
    (if (< ?dist 60) then (return 1))
    (return 2)
)

(deffunction PROCESO_DATOS::max-puntuacion ($?puntuaciones)
"Devuelve la instancia de Puntuacion que tiene mayor puntuacion"
    (bind ?max -1)
	(bind ?resultado nil)
	(progn$ (?curr-obj $?puntuaciones)
		(bind ?p (send ?curr-obj get-puntuacion))
		(if (> ?p ?max) then 
			(bind ?max ?p)
			(bind ?resultado ?curr-obj)
		)
	)
	return ?resultado
)

;;; ################################ RECOPILACION DE DATOS ################################

(defrule RECOPILAR_INFO::determinar-edad
	(declare (salience 10))
    (not (datos_cliente))
	=>
	(bind ?d (preguntar-valorconcreto "Que edad tienes? "))
    (bind ?edad nil)
	(if (< ?d 30) then (bind ?edad "Joven"))
    (if (and (>= ?d 30)(< ?d 60)) then (bind ?edad "Adulto"))
    (if (>= ?d 60) then (bind ?edad "Anciano"))
	(assert (datos_cliente (edad ?edad)))
)

(defrule RECOPILAR_INFO::determinar-estudiante_trabajador
    (declare (salience 8))
    ?ref <- (datos_cliente)
    (not (test_estudiante_trabajador))
	=>
    (bind ?ess 0)
    (bind ?trabajab FALSE)
    (if (yes-or-no "Estudias? ")
        then (bind ?ess 1)
        else (bind ?trabajab (yes-or-no "Trabajas? "))
    )
    (bind ?pox -1)
    (bind ?poy -1)
    (if (or (eq ?ess 1)(eq ?trabajab TRUE)) then 
        (bind ?pox (preguntar-valorconcreto "Posicion X de tu centro educativo/trabajo (rango 0..100): "))
        (bind ?poy (preguntar-valorconcreto "Posicion Y de tu centro educativo/trabajo (rango 0..100): "))
    )
    (modify ?ref (estudiante ?ess)(posX_Est_Trb ?pox)(posY_Est_Trb ?poy))
    (assert (test_estudiante_trabajador))
)

(defrule RECOPILAR_INFO::determinar-pareja
    (declare (salience 8))
    ?ref <- (datos_cliente (pareja ?pareja))
    (test (= ?pareja -1))
	=>
    (if (yes-or-no "Vas a vivir en pareja? ")
        then (modify ?ref (pareja 1))
        else (modify ?ref (pareja 0))
    )
)

(defrule RECOPILAR_INFO::determinar-precio_min
    (declare (salience 8))
    ?ref <- (datos_cliente (precio_min ?precio_min))
    (test (= ?precio_min -1))
	=>
    (bind ?precio_min (preguntar-valorconcreto "Cual es el precio MINIMO que desea pagar? "))
    (assert (restriccion (dato "precio_min") (is_rest (yes-or-no "---- Es restriccion? "))))
	(modify ?ref (precio_min ?precio_min))
)

(defrule RECOPILAR_INFO::determinar-precio_max
    (declare (salience 8))
    ?ref <- (datos_cliente (precio_max ?precio_max))
    (test (= ?precio_max -1))
	=>
    (bind ?precio_max (preguntar-valorconcreto "Cual es el precio MAXIMO dispuesto a pagar? "))
    (assert (restriccion (dato "precio_max") (is_rest (yes-or-no "---- Es restriccion? "))))
	(modify ?ref (precio_max ?precio_max))
)


(defrule RECOPILAR_INFO::determinar-hijos
    (declare (salience 8))
    ?ref <- (datos_cliente (hijos ?h))
    (test (= ?h -1))
	=>
    (if (yes-or-no "Tienes hijos que requieran de un centro educativo? ") then
            (printout t "La edad de referencia indicara el nivel del centro educativo a buscar")
            (printout t crlf)
            (bind ?edad-hijo (preguntar-valorconcreto "Edad de referencia? "))
            (modify ?ref (hijos ?edad-hijo))
        else
            (modify ?ref (hijos 0))
    )

)

(defrule RECOPILAR_INFO::determinar-mascotas
    (declare (salience 8))
    ?ref <- (datos_cliente (mascotas ?mascotas))
    (test (= ?mascotas -1))
	=>
    (if (yes-or-no "Tiene que admitir mascotas? ") then
            (modify ?ref (mascotas 1))
            (assert (restriccion (dato "mascota") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (mascotas 0))
    )
)

(defrule RECOPILAR_INFO::determinar-vehiculo
    (declare (salience 8))
    ?ref <- (datos_cliente (vehiculo ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener garaje? ") then 
            (modify ?ref (vehiculo 1))
            (assert (restriccion (dato "vehiculo") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (vehiculo 0))
    )
)

(defrule RECOPILAR_INFO::determinar-banyos
    (declare (salience 8))
    ?ref <- (datos_cliente (banyos ?v))
    (test (= ?v -1))
	=>
    (modify ?ref (banyos (preguntar-valorconcreto "Cuantos banyos quieres que tenga la vivienda? ")))
    (assert (restriccion (dato "banyos") (is_rest (yes-or-no "---- Es restriccion? "))))
)

(defrule RECOPILAR_INFO::determinar-ascensor
    (declare (salience 8))
    ?ref <- (datos_cliente (ascensor ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener ascensor? ") then 
            (modify ?ref (ascensor 1))
            (assert (restriccion (dato "ascensor") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (ascensor 0))
    )
)

(defrule RECOPILAR_INFO::determinar-dorm_sim
    (declare (salience 8))
    ?ref <- (datos_cliente (dorm_sim ?v))
    (test (= ?v -1))
	=>
    (modify ?ref (dorm_sim (preguntar-valorconcreto "Cuantos dormitorios simples quieres que tenga la vivienda? ")))
    (assert (restriccion (dato "dorm_sim") (is_rest (yes-or-no "---- Es restriccion? "))))
)

(defrule RECOPILAR_INFO::determinar-dorm_dob
    (declare (salience 8))
    ?ref <- (datos_cliente (dorm_dob ?v))
    (test (= ?v -1))
	=>
    (modify ?ref (dorm_dob (preguntar-valorconcreto "Cuantos dormitorios dobles quieres que tenga la vivienda? ")))
    (assert (restriccion (dato "dorm_dob") (is_rest (yes-or-no "---- Es restriccion? "))))
)

(defrule RECOPILAR_INFO::determinar-superficie
    (declare (salience 8))
    ?ref <- (datos_cliente (superficie ?v))
    (test (= ?v -1))
	=>
    (modify ?ref (superficie (preguntar-valorconcreto "Cual es la superfice minima que deseas? ")))
    (assert (restriccion (dato "superficie") (is_rest (yes-or-no "---- Es restriccion? "))))
)

(defrule RECOPILAR_INFO::determinar-terraza
    (declare (salience 8))
    ?ref <- (datos_cliente (terraza ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener terraza? ") then 
            (modify ?ref (terraza 1))
            (assert (restriccion (dato "terraza") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (terraza 0))
    )
)

(defrule RECOPILAR_INFO::determinar-balcon
    (declare (salience 8))
    ?ref <- (datos_cliente (balcon ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener balcon? ") then 
            (modify ?ref (balcon 1))
            (assert (restriccion (dato "balcon") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (balcon 0))
    )
)


(defrule RECOPILAR_INFO::determinar-amueblada
    (declare (salience 8))
    ?ref <- (datos_cliente (amueblada ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que estar amueblada? ") then 
            (modify ?ref (amueblada 1))
            (assert (restriccion (dato "amueblada") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (amueblada 0))
    )
)

(defrule RECOPILAR_INFO::determinar-electrodomesticos
    (declare (salience 8))
    ?ref <- (datos_cliente (electrodomesticos ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener electrodomesticos? ") then 
            (modify ?ref (electrodomesticos 1))
            (assert (restriccion (dato "electrodomesticos") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (electrodomesticos 0))
    )
)

(defrule RECOPILAR_INFO::determinar-piscina
    (declare (salience 8))
    ?ref <- (datos_cliente (piscina ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener piscina? ") then 
            (modify ?ref (piscina 1))
            (assert (restriccion (dato "piscina") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (piscina 0))
    )
)

(defrule RECOPILAR_INFO::determinar-transporte
    (declare (salience 8))
    ?ref <- (datos_cliente (transporte ?v))
    (test (= ?v -1))
	=>
    (if (yes-or-no "Tiene que tener transporte public cerca? ") then 
            (modify ?ref (transporte 1))
            (assert (restriccion (dato "transporte") (is_rest (yes-or-no "---- Es restriccion? "))))
        else 
            (modify ?ref (transporte 0))
    )
)

(defrule RECOPILAR_INFO::determinar-barrios
    (declare (salience 8))
    ?ref <- (datos_cliente)
    (not (determinado-barrios))
	=>
    (if (yes-or-no "Quieres indicar en que barrios prefieres la vivienda? ") then 
        (bind $?obj-barrios (find-all-instances ((?inst Barrio)) TRUE))
        (bind $?nombre_barrios (create$ ))
        (loop-for-count (?i 1 (length$ $?obj-barrios)) do
            (bind ?curr-obj (nth$ ?i $?obj-barrios))
            (bind ?curr-nom (send ?curr-obj get-Nombre))
            (bind $?nombre_barrios(insert$ $?nombre_barrios (+ (length$ $?nombre_barrios) 1) ?curr-nom))
        )
        (bind ?escogidos (pregunta-multirespuesta "Que barrios quieres? " $?nombre_barrios))
        (bind $?lista_barrios (create$ ))
        (loop-for-count (?i 1 (length$ $?escogidos)) do
            (bind ?curr-obj (nth$ ?i $?escogidos))
            (bind $?lista_barrios(insert$ $?lista_barrios (+ (length$ $?lista_barrios) 1) (nth$ ?curr-obj ?obj-barrios)))
        )
        (modify ?ref (barrios $?lista_barrios))
        (assert (restriccion (dato "barrios") (is_rest (yes-or-no "---- Es restriccion? "))))
    )
    (assert (determinado-barrios))
)

(defrule RECOPILAR_INFO::determinar-tipo_vivienda_pref
    (declare (salience 8))
    ?ref <- (datos_cliente)
    (not (determinado-tipo_vivienda_pref))
	=>
    (if (yes-or-no "Quieres indicar que tipo de vivienda prefieres? ") then 
        (bind $?nombre_vivienda (create$ ))
        (bind $?nombre_vivienda(insert$ $?nombre_vivienda (+ (length$ $?nombre_vivienda) 1) "Duplex"))
        (bind $?nombre_vivienda(insert$ $?nombre_vivienda (+ (length$ $?nombre_vivienda) 1) "Piso"))
        (bind $?nombre_vivienda(insert$ $?nombre_vivienda (+ (length$ $?nombre_vivienda) 1) "Unifamiliar"))
        (bind $?escogidos (pregunta-multirespuesta "Que tipo de vivienda prefieres? " $?nombre_vivienda))
        (modify ?ref (tipo_vivienda_pref $?escogidos))
    )
    (assert (determinado-tipo_vivienda_pref))
)

(defrule RECOPILAR_INFO::determinar-acabado
    (declare (salience 1))
    =>
    (focus PROCESO_DATOS)
    (printout t crlf)
    (printout t crlf)
    (printout t crlf)
    (format t "##########################################################################%n%n")
    (format t "                              PROCESANDO DATOS ...                        %n%n")
    (format t "##########################################################################")
    (printout t crlf)
    (printout t crlf)
)


;;; ################################ PROCESO DE DATOS ################################

(defrule PROCESO_DATOS::inicializar_puntuacion
    (declare (salience 10))
	=>
	(bind $?a (find-all-instances ((?inst Vivienda)) TRUE))
	(progn$ (?curr-con ?a)
		(make-instance (gensym) of Puntuaciones (vivienda ?curr-con)(puntuacion 0))
	)
)

(defrule PROCESO_DATOS::puntuarSegun-edad
    (declare (salience 8))
    (datos_cliente (edad ?t))
    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-edad ?viv))
	=>
    (if (and (eq ?t "Joven") (eq (type ?viv) Piso)) then
        (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
        (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "A los jovenes los pisos: +3")
    )
    (if (eq ?t "Adulto") then
        (if (eq (type ?viv) Piso) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "A los adultos los piso: +2")
        )
        (if (eq (type ?viv) Duplex) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "A los adultos los duplex: +1")
        )
    )
    (if (eq ?t "Anciano") then
        (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
        (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "A los ancianos cualquier vivienda: +2")

    )
    (assert (valorado-edad ?viv))
)


(defrule PROCESO_DATOS::puntuarSegun-estudio_trabajo
    (declare (salience 8))
    (datos_cliente (posX_Est_Trb ?px)(posY_Est_Trb ?py))
    (test (not (eq ?px -1)))
    (test (not (eq ?py -1)))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-estudio_trabajo ?viv))
    =>
    (bind ?dist (+ (abs (- (send ?viv get-posicionX) ?px))(abs (- (send ?viv get-posicionY) ?py))))
    (if (< ?dist 40) then
        (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 8))
        (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca del centro educativo/trabajo +8")
    )
    (assert (valorado-estudio_trabajo ?viv))
)


(defrule PROCESO_DATOS::puntuarSegun-precio
	(declare (salience 8))
    (datos_cliente (precio_min ?pmin) (precio_max ?pmax))
    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv)(puntuacion ?p)(justificaciones $?j))
	(not (valorado-precio ?viv))
    (restriccion (dato "precio_max")(is_rest ?isrestmax))
    (restriccion (dato "precio_min")(is_rest ?isrestmin))
    =>
    (bind ?precio (send ?viv get-preciomensual))
    (if  (and (>= ?precio ?pmin)(>= ?pmax ?precio))
        then
            ; Las puntuaciones con precio dentro del rango += 6
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 6))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "El precio esta dentro de los limites: +6")
        else
            (if (> ?precio ?pmax) then
                (bind ?incremento (max 0 (- 6 (* 6 (/ ?precio ?pmax)))))
            )
            (if (< ?precio ?pmin) then
                (bind ?incremento (max 0 (- 6 (* 3 (/ ?precio ?pmax)))))
            )
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) ?incremento))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j))
                (format nil "El precio esta proximo al intervalo de precios +%d" ?incremento))
    )
    (if (and (>= ?precio ?pmin)(eq TRUE ?isrestmin)) then  
        (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 100))
        (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de precio minimo +100"))
    (if (and (<= ?precio ?pmax)(eq TRUE ?isrestmax)) then  
        (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 100))
        (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de precio maximo +100"))
    (assert (valorado-precio ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-mascota
	(declare (salience 8))
	(datos_cliente (mascotas ?mascotas))
    (test (not (eq ?mascotas 0))) ; Comprobamos si esta interesado en las mascotas
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-mascota ?viv))
    (restriccion (dato "mascota")(is_rest ?isrest))
	=>
	(if (send ?viv get-mascotas)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda admite mascotas: +2")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 20))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de mascota +20"))
	)
	(assert (valorado-mascota ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-vehiculo
	(declare (salience 8))
	(datos_cliente (vehiculo ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los vehiculos
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-vehiculo ?viv))
    (restriccion (dato "vehiculo")(is_rest ?isrest))
	=>
	(if (send ?viv get-garaje)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene plaza de parking: +2")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 20))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de vehiculo +20"))
	)
	(assert (valorado-vehiculo ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-banyos
	(declare (salience 8))
	(datos_cliente (banyos ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los banyos
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-banyos ?viv))
    (restriccion (dato "banyos")(is_rest ?isrest))
	=>
    (bind ?b (send ?viv get-numbanyos))
	(if (>= ?b ?v)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene como minimo los banyos pedidos: +2")
            (if (and (eq TRUE ?isrest)(eq ?b ?v)) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 20))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de banyos +20"))
	)
	(assert (valorado-banyos ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-ascensor
	(declare (salience 8))
	(datos_cliente (ascensor ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los ascensor
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-ascensor ?viv))
    (restriccion (dato "ascensor")(is_rest ?isrest))
	=>
	(if (send ?viv get-ascensor)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene ascensor: +2")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 20))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de ascensor +20"))
	)
	(assert (valorado-ascensor ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-dorm_sim
	(declare (salience 8))
	(datos_cliente (dorm_sim ?v))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-dorm_sim ?viv))
    (restriccion (dato "dorm_sim")(is_rest ?isrest))
	=>
    (bind ?b (send ?viv get-numDormSimple))
	(if (>= ?b ?v)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene como minimo los dorm_sim pedidos: +2")
            (if (and (eq TRUE ?isrest)(eq ?b ?v)) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 20))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de dorm_sim +20"))
	)
	(assert (valorado-dorm_sim ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-dorm_dob
	(declare (salience 8))
	(datos_cliente (dorm_dob ?v))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-dorm_dob ?viv))
    (restriccion (dato "dorm_dob")(is_rest ?isrest))
	=>
    (bind ?b (send ?viv get-numDormDoble))
	(if (>= ?b ?v)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene como minimo los dorm_dob pedidos: +2")
            (if (and (eq TRUE ?isrest)(eq ?b ?v)) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 20))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de dorm_dob +20"))
	)
	(assert (valorado-dorm_dob ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-superficie
	(declare (salience 8))
	(datos_cliente (superficie ?v))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-superficie ?viv))
    (restriccion (dato "superficie")(is_rest ?isrest))
	=>
    (bind ?b (send ?viv get-superficie))
	(if (>= ?b ?v)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 6))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene como minimo los superficie pedidos: +6")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 60))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de superficie +60"))
	)
	(assert (valorado-superficie ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-terraza
	(declare (salience 8))
	(datos_cliente (terraza ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los terraza
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-terraza ?viv))
    (restriccion (dato "terraza")(is_rest ?isrest))
	=>
	(if (send ?viv get-terraza)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene terraza: +3")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 30))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de terraza +30"))
	)
	(assert (valorado-terraza ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-balcon
	(declare (salience 8))
	(datos_cliente (balcon ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los balcon
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-balcon ?viv))
    (restriccion (dato "balcon")(is_rest ?isrest))
	=>
	(if (send ?viv get-balcon)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene balcon: +3")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 30))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de balcon +30"))
	)
	(assert (valorado-balcon ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-amueblada
	(declare (salience 8))
	(datos_cliente (amueblada ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los amueblada
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-amueblada ?viv))
    (restriccion (dato "amueblada")(is_rest ?isrest))
	=>
	(if (send ?viv get-amueblada)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 5))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene amueblada: +5")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 50))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de amueblada +50"))
	)
	(assert (valorado-amueblada ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-electrodomesticos
	(declare (salience 8))
	(datos_cliente (electrodomesticos ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los electrodomesticos
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-electrodomesticos ?viv))
    (restriccion (dato "electrodomesticos")(is_rest ?isrest))
	=>
	(if (send ?viv get-electrodomesticos)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 4))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene electrodomesticos: +4")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 40))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de electrodomesticos +40"))
	)
	(assert (valorado-electrodomesticos ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-piscina
	(declare (salience 8))
	(datos_cliente (piscina ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los piscina
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-piscina ?viv))
    (restriccion (dato "piscina")(is_rest ?isrest))
	=>
	(if (send ?viv get-piscina)
		then
			(send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 5))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda tiene piscina: +5")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 50))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de piscina +50"))
	)
	(assert (valorado-piscina ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-transporte
    (declare (salience 8))
    (datos_cliente (transporte ?v))
    (test (not (eq ?v 0))) ; Comprobamos si esta interesado en los transporte
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-transporte ?viv))
    (restriccion (dato "transporte")(is_rest ?isrest))
    =>
    (bind $?objs (find-all-instances ((?inst Transporte)) TRUE))
    (loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (= ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 6))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de un transporte publico: +6")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 60))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de transporte +60"))
        )
        (if (= ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de un transporte publico: +3")
        )
    )
    (assert (valorado-transporte ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-barrios
    (declare (salience 8))
    (datos_cliente (barrios $?v))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-barrios ?viv))
    (restriccion (dato "barrios")(is_rest ?isrest))
    =>
    (loop-for-count (?i 1 (length$ $?v)) do
        (bind ?curr-obj (nth$ ?i $?v))
        (bind ?barrio_vivienda (send ?viv get-barrioVivienda))
        (if (eq ?curr-obj ?barrio_vivienda) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 8))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta en un barrio deseado: +8")
            (if (eq TRUE ?isrest) then  
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 80))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de barrio +80"))
            (break) ; Ya hemos encontrado el barrio de la vivienda
        )
    )
    (assert (valorado-barrios ?viv))
)


(defrule PROCESO_DATOS::puntuarSegun-tipo_vivienda
    (declare (salience 8))
    (datos_cliente (tipo_vivienda_pref $?v))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-tipo_vivienda ?viv))
    =>
    (loop-for-count (?i 1 (length$ $?v)) do
        (bind ?curr-obj (nth$ ?i $?v))
        (if (and (eq ?curr-obj 1)(eq (str-cat (class ?viv)) "Duplex")) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 90))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de vivienda Duplex +90")
            (break)
        )
        (if (and (eq ?curr-obj 2)(eq (str-cat (class ?viv)) "Piso")) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 90))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de vivienda Piso +90")
            (break)
        )
        (if (and (eq ?curr-obj 3)(eq (str-cat (class ?viv)) "Unifamiliar")) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 90))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "RESTRICCION de vivienda Unifamiliar +90")
            (break)
        )
    )
    (assert (valorado-tipo_vivienda ?viv))
)


;;; -------------------------------- INICIO PUNTUAR SEGUN SERVICIOS --------------------------------

;;COMERCIO

(defrule PROCESO_DATOS::puntuarSegun-24h
    (declare (salience 8))
    (datos_cliente (edad ?d))
		(datos_cliente (pareja ?p))
    (test (or (eq ?d "Joven") (eq ?p 0)))

    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
    (not (valorado-24H ?viv))
    =>
    (bind $?objs (find-all-instances ((?inst Comercio)) (eq ?inst:tipo_comercio 24h))) ;; 1 pero no se :(
    (loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (= ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de un 24H: +2")
        )
        (if (= ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de un 24H: +1")
        )
    )
    (assert (valorado-24H ?viv))
)
(defrule PROCESO_DATOS::puntuarSegun-CentroComercial
    (declare (salience 8))
    (datos_cliente (edad ?d))
		(datos_cliente (pareja ?p))
    (test (or (eq ?d "Anciano") (eq ?d "Joven") (eq ?p 1)))

    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
    (not (valorado-CentroComercial ?viv))
    =>
    (bind $?objs (find-all-instances ((?inst Comercio)) (eq ?inst:tipo_comercio CentroComercial)))
    (loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (= ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de un Centro Comercial: +2")
        )
        (if (= ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de un Centro Comercial: +1")
        )
    )
    (assert (valorado-CentroComercial ?viv))
)


(defrule PROCESO_DATOS::puntuarSegun-Mercado
    (declare (salience 8))
    (datos_cliente (edad ?d))
    (test (or (eq ?d "Anciano") (eq ?d "Adulto")))

    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
    (not (valorado-mercado ?viv))
    =>
    (bind $?objs (find-all-instances ((?inst Comercio)) (eq ?inst:tipo_comercio Mercado)))
    (loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (= ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de un mercado: +2")
        )
        (if (= ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de un mercado: +1")
        )
    )
    (assert (valorado-mercado ?viv))
)

;;EDUCACION


(defrule PROCESO_DATOS::puntuarSegun-educacion
	(declare (salience 8))
    (datos_cliente (hijos ?h))
    (test (not (eq ?h 0))) ; Comprobamos si tiene hijos

    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-hijos ?viv))
    =>
    (bind ?posX (send ?viv get-posicionX))
    (bind ?posY (send ?viv get-posicionY))
    (bind $?objs (find-all-instances ((?inst Educacion)) TRUE))
    (loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?nivel (send ?curr-obj get-tipo_nivel_maximo))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (or
                (and (< ?h 5)           (eq ?nivel Guarderia))
                (and (>= ?h 5) (< ?h 12)(eq ?nivel Primaria))
                (and (>= ?h 12)(< ?h 15)(eq ?nivel Secundaria))
                (and (>= ?h 15)(< ?h 17)(eq ?nivel Bachillerato))
                (and (>= ?h 17)         (eq ?nivel Universidad))
            )
            then
            (if (= ?d 0) then
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 10))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j))
                    (format nil "La vivienda tiene un centro educativo adaptado a los hijos +10"))
            )
            (if (= ?d 1) then
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 7))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j))
                    (format nil "La vivienda tiene un centro educativo adaptado a los hijos +7"))
            )
        )
    )
    (assert (valorado-hijos ?viv))
)

;;NATURAL

(defrule PROCESO_DATOS::puntuarSegun-Playa
    (declare (salience 8))
    (datos_cliente (edad ?d))
    (test (eq ?d "Anciano"))
    ?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
		(not (valorado-playa ?viv))
    =>
    (bind $?objs (find-all-instances ((?inst Natural)) (eq ?inst:tipo_natural Playa)))
    (loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (= ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de una playa: +3")
        )
        (if (= ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de una playa: +1")
        )
    )
    (assert (valorado-playa ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-Montanya
	(declare (salience 8))
	(datos_cliente (edad ?edad) (pareja ?pareja))
	(test (or (eq ?edad "Anciano") (eq ?edad "Joven") (eq ?pareja 1)))

	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-montanya ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Natural)) (eq ?inst:tipo_natural Montanya)))
	(loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (= ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de una montanya: +3")
        )
        (if (= ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de una montanya: +1")
        )
	)
	(assert (valorado-montanya ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-Parque
	(declare (salience 8))
	(datos_cliente (edad ?edad) (pareja ?pareja) (hijos ?hijos))
	(test (or(eq ?edad "Anciano") (eq ?hijos 1) (eq ?pareja 1)))

	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-parque ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Natural)) (eq ?inst:tipo_natural Parque)))
	(loop-for-count (?i 1 (length$ $?objs)) do
			(bind ?curr-obj (nth$ ?i ?objs))
			(bind ?d (distancia ?viv ?curr-obj))
			(if (= ?d 0) then
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de un parque: +2")
			)
			(if (= ?d 1) then
                (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
                (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de un parque: +1")
			)
	)
	(assert (valorado-parque ?viv))
)


;;OCIO

(defrule PROCESO_DATOS::puntuarSegun-Bar
	(declare (salience 8))
	(datos_cliente (edad ?edad) (pareja ?pareja))
	(test (or (eq ?edad "Anciano") (eq ?edad "Joven") (eq ?pareja 0)))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p) (justificaciones $?j))
	(not (valorado-bar ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Ocio)) (eq ?inst:tipo_ocio Bar)))
	(loop-for-count (?i 1 (length$ $?objs)) do
		(bind ?curr-obj (nth$ ?i ?objs))
		(bind ?d (distancia ?viv ?curr-obj))
		(if (eq ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 2))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a cerca distancia de un bar: +2")
		)
		(if (eq ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a media distancia de un bar: +1")
		)
	)
	(assert (valorado-bar ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-Cine
	(declare (salience 8))
	(datos_cliente (edad ?edad) (hijos ?hijos) (pareja ?pareja))
	(test (or (eq ?edad "Joven") (and (eq ?pareja 1) (eq ?hijos 1))))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p) (justificaciones $?j))
	(not (valorado-cine ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Ocio)) (eq ?inst:tipo_ocio Cine)))
	(loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (eq ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a cerca distancia de un cine: +3")
        )
        (if (eq ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a media distancia de un cine: +1")
        )
	)
	(assert (valorado-cine ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-Teatro
	(declare (salience 8))
	(datos_cliente (edad ?edad) (pareja ?pareja))
	(test (or (eq ?edad "Adulto") (eq ?pareja 1)))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p) (justificaciones $?j))
	(not (valorado-teatro ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Ocio)) (eq ?inst:tipo_ocio Teatro)))
	(loop-for-count (?i 1 (length$ $?objs)) do
		(bind ?curr-obj (nth$ ?i ?objs))
		(bind ?d (distancia ?viv ?curr-obj))
		(if (eq ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a cerca distancia de un teatro: +3")
		)
		(if (eq ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a media distancia de un teatro: +1")
		)
	)
	(assert (valorado-teatro ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-Biblioteca
	(declare (salience 8))
	(datos_cliente (edad ?edad) (estudiante ?estudiante))
	(test (or (eq ?edad "Anciano") (eq ?estudiante 1)))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p) (justificaciones $?j))
	(not (valorado-bibilioteca ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Ocio)) (eq ?inst:tipo_ocio Biblioteca)))
	(loop-for-count (?i 1 (length$ $?objs)) do
		(bind ?curr-obj (nth$ ?i ?objs))
		(bind ?d (distancia ?viv ?curr-obj))
		(if (eq ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 3))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a cerca distancia de una bibilioteca: +3")
		)
		(if (eq ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 1))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a media distancia de una bibilioteca: +1")
		)
	)
	(assert (valorado-bibilioteca ?viv))
)


;;SANITARIO

(defrule PROCESO_DATOS::puntuarSegun-CAP
	(declare (salience 8))
	(datos_cliente (edad ?edad)(hijos ?hijos))
	(test (or (eq ?edad "Anciano") (not(eq ?hijos 0))))		; Comprobamos si el cliente es Anciano o si tiene hijos
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p)(justificaciones $?j))
	(not (valorado-cap ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Sanitario)) (eq ?inst:tipo_sanitario CAP)))
	(loop-for-count (?i 1 (length$ $?objs)) do
		(bind ?curr-obj (nth$ ?i ?objs))
		(bind ?d (distancia ?viv ?curr-obj))
		(if (eq ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 10))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta cerca de un centro de atencion primaria: +10")
		)
		(if (eq ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 7))
            (slot-insert$ ?rec justificaciones (+ 1(length$ $?j)) "La vivienda esta a media distancia de un centro de atencion primaria: +7")
        )
	)
	(assert (valorado-cap ?viv))
)

(defrule PROCESO_DATOS::puntuarSegun-Hospital
	(declare (salience 8))
	(datos_cliente (edad ?edad))
	(test (eq ?edad "Anciano"))
	?rec <- (object (is-a Puntuaciones) (vivienda ?viv) (puntuacion ?p) (justificaciones $?j))
	(not (valorado-hospital ?viv))
	=>
	(bind $?objs (find-all-instances ((?inst Sanitario)) (eq ?inst:tipo_sanitario Hospital)))
	(loop-for-count (?i 1 (length$ $?objs)) do
        (bind ?curr-obj (nth$ ?i ?objs))
        (bind ?d (distancia ?viv ?curr-obj))
        (if (eq ?d 0) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 10))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a cerca distancia de un Hospital: +10")
        )
        (if (eq ?d 1) then
            (send ?rec put-puntuacion (+ (send ?rec get-puntuacion) 7))
            (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "La vivienda esta a media distancia de un Hospital: +7")
        )
	)
	(assert (valorado-hospital ?viv))
)

;;; -------------------------------- FIN PUNTUAR SEGUN SERVICIOS --------------------------------


(defrule PROCESO_DATOS::FinalizarProcesoDatos
    (declare (salience 2))
    (not (final-proceso))
    =>
    (bind $?objs (find-all-instances ((?inst Puntuaciones)) TRUE))
    (bind $?rec (create$ ))
    (bind $?ade (create$ ))
    (bind $?par (create$ ))
    (bind ?i 9)
    (if (<= ?i (length$ $?objs)) ;; Comprobamos que haya como minimo "i" viviendas
        then
            (while (< 0 ?i) do
                (bind ?curr-max-punt (max-puntuacion $?objs))
                (bind $?objs (delete-member$ $?objs ?curr-max-punt))
                (if (> ?i 6) then
                    (bind ?rec (insert$ ?rec (+ (length$ ?rec) 1) ?curr-max-punt))
                    else
                        (if (> ?i 3) then
                            (bind ?ade (insert$ ?ade (+ (length$ ?ade) 1) ?curr-max-punt))
                        else
                            (bind ?par (insert$ ?par (+ (length$ ?par) 1) ?curr-max-punt)))
                )
                (bind ?i (- ?i 1))
            )
            (assert (solucion
                (parcialmente ?par)
                (adecuado ?ade)
                (recomendable ?rec)
            ))
            (focus IMPRIMIR_SOL)
            (format t "                        PASANDO A IMPRIMIR SOLUCION ...                   %n%n")
            (format t "##########################################################################%n%n")
        else
            (format t "Se necesitan como minimo %d instancias de vivienda!" ?i)
            (printout t crlf)
    )
    (assert (final-proceso))
)

;;; ################################ IMPRIMIR SOLUCION ################################

(defrule IMPRIMIR_SOL::imprimirSolucion
    (solucion (parcialmente $?a)(adecuado $?b)(recomendable $?c))
	=>
    (printout t "RECOMENDABLE: ")
    (loop-for-count (?i 1 (length$ $?c)) do
        (if (not (eq ?i 1)) then (format t "%n%n@@@@@@@@@@"))
		(bind ?act(nth$ ?i ?c))
		(send ?act imprimir)
	)
    (printout t crlf)
    (printout t crlf)
    (printout t "--------------------------------------------------------------------------")
    (printout t crlf)
    (printout t crlf)
    (printout t "ADECUADO: ")
    (loop-for-count (?i 1 (length$ $?b)) do
        (if (not (eq ?i 1)) then (format t "%n%n@@@@@@@@@@"))
		(bind ?act(nth$ ?i ?b))
		(send ?act imprimir)
	)
    (printout t crlf)
    (printout t crlf)
    (printout t "--------------------------------------------------------------------------")
    (printout t crlf)
    (printout t crlf)
    (printout t "PARCIALMENTE: ")
    (loop-for-count (?i 1 (length$ $?a)) do
        (if (not (eq ?i 1)) then (format t "%n%n@@@@@@@@@@"))
		(bind ?act(nth$ ?i ?a))
		(send ?act imprimir)
	)
    (printout t crlf)
    (printout t crlf)
    (format t "##########################################################################%n%n")
    (printout t crlf)
)

(defmessage-handler IMPRIMIR_SOL::Puntuaciones imprimir()
    (printout t crlf)
    (send ?self:vivienda imprimir)
    (format t "%nJustificaciones:")
    (loop-for-count (?i (length$ $?self:justificaciones)) do
        (bind ?act(nth$ ?i ?self:justificaciones))
        (printout t crlf)
        (format t " - ")
		(format t ?act)
    )
)

(defmessage-handler IMPRIMIR_SOL::Vivienda imprimir()
    (printout t crlf)
    (if (eq (type ?self) Unifamiliar) then (format t "La casa unifamilar "))
    (if (eq (type ?self) Piso) then (format t "El piso "))
    (if (eq (type ?self) Duplex) then (format t "El duplex "))
    (format t "se encuentra  en la posicion: (%d,%d)%n" ?self:posicionX ?self:posicionY)
    (format t "Barrio %s%n" (send ?self:barrioVivienda get-Nombre))
    (format t "Precio mensual: %d%n" ?self:preciomensual)
    (format t "Numero de dormitorios simples: %d%n" ?self:numDormSimple)
    (format t "Numero de dormitorios dobles: %d%n" ?self:numDormDoble)
    (format t "Numero de banos: %d%n" ?self:numbanyos)
    (format t "Superficie: %d%n" ?self:superficie)
    (if (eq ?self:mascotas TRUE) then (format t "Permite mascotas: Si%n")
        else (format t "Permite mascotas: No%n")
    )
    (if (eq ?self:garaje TRUE) then (format t "Garaje: Si%n")
        else (format t "Garaje: No%n")
    )
    (if (eq ?self:ascensor TRUE) then (format t "Ascensor: Si%n")
        else (format t "Ascensor: No%n")
    )
    (if (eq ?self:terraza TRUE) then (format t "Terraza: Si%n")
        else (format t "Terraza: No%n")
    )
    (if (eq ?self:balcon TRUE) then (format t "Balcon: Si%n")
        else (format t "Balcon: No%n")
    )
    (if (eq ?self:amueblada TRUE) then (format t "Amueblada: Si%n")
        else (format t "Amueblada: No%n")
    )
    (if (eq ?self:amueblada TRUE) then (format t "Electrodomesticos: Si%n")
        else (format t "Electrodomesticos: No%n")
    )
    (if (eq ?self:amueblada TRUE) then (format t "Piscina: Si%n")
        else (format t "Piscina: No%n")
    )
)

