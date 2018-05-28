(defmodule MAIN (export ?ALL))
(defmodule RECOPILAR_INFO (import MAIN ?ALL)(export ?ALL))
(defmodule PROCESAR_DATOS (import MAIN ?ALL)(export ?ALL))
(defmodule IMPRIMIR_SOL (import MAIN ?ALL)(export ?ALL))

(defclass MAIN::ciudades_validas
  (is-a USER) (role concrete)
    (slot ciudad (type INSTANCE)(create-accessor read-write))
    (slot puntos (type INTEGER)(default 0)(create-accessor read-write))
    (multislot justificaciones (type STRING)(create-accessor read-write))
)

(deftemplate MAIN::viaje
  (multislot ciudades (type INSTANCE))
  (multislot servicios (type INSTANCE))
  (slot medioViaje (type INSTANCE))
  (slot precio (type INTEGER))
  (slot dias (type INTEGER))
  (slot puntuacion (type INTEGER))
)

(deftemplate MAIN::datos_grupo
  ; Caracteristicas del Grupo
  (slot num_adultos (type INTEGER)(default 1))
  (slot tipo_grupo (type STRING)(default ?NONE))
  (slot ninos (type INTEGER)(default 0))
  (slot zona (type STRING)(default ""))
  (slot edad (type STRING)(default ""))
  (multislot tipo_viaje (type STRING))

  ; Restricciones del Viaje
  (slot min_num_dias (type INTEGER)(default 1))
  (slot max_num_dias (type INTEGER)(default 1))
  (slot num_ciudades (type INTEGER)(default 1))
  (slot min_dias_ciudad (type INTEGER)(default 1))
  (slot max_dias_ciudad (type INTEGER)(default 1))
  (slot presupuesto (type INTEGER)(default 0))

  (slot calidad (type STRING)(default ""))
)

(defrule MAIN::init
  (declare (salience 99))
  =>
  (printout t crlf)
  (printout t crlf)
  (printout t "###############################################################")
  (printout t crlf)
  (printout t crlf)
  (format t "                       AGENCIA DE VIAJES%n")
  (format t "                           HOLA Q TAL%n")
  (printout t crlf)
  (printout t "###############################################################")
  (printout t crlf)
  (printout t crlf)

  (focus RECOPILAR_INFO)
)

(deffunction RECOPILAR_INFO::pregunta_valor_posible (?question $?allowed-values)
"Escribe una pregunta y lee uno de los valores posibles (allowed-values)"
	(printout t ?question)
    (printout t $?allowed-values " : ")
	(bind ?answer (read))
	(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	(while (not (member$ ?answer ?allowed-values)) do
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
	)
    (printout t crlf)
	(return ?answer)
)

(deffunction RECOPILAR_INFO::pregunta_entero (?question)
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

(deffunction PROCESAR_DATOS::max-puntos ($?puntos)
  (bind ?max 0)
  (bind ?resultado nil)
  (loop-for-count (?i 1 (length$ $?puntos)) do
  (bind ?aux (nth$ ?i $?puntos))
  (if (= ?i 1) then (bind ?max (send ?aux get-puntos))
                    (bind ?resultado ?aux)
  else
  (if (> (send ?aux get-puntos) ?max) then (bind ?max (send ?aux get-puntos))
                                           (bind ?resultado ?aux))))
  (return ?resultado))

(deffunction PROCESAR_DATOS::min-puntos ($?puntos)
  (bind ?min 0)
  (bind ?resultado nil)
  (loop-for-count (?i 1 (length$ $?puntos)) do
    (bind ?aux (nth$ ?i $?puntos))
    (if (= ?i 1)
      then
      (bind ?min (send ?aux get-puntos))
      (bind ?resultado ?aux)
      else
      (if (< (send ?aux get-puntos) ?min) then
        (bind ?min (send ?aux get-puntos))
        (bind ?resultado ?aux)
      )
    )
  )
  (return ?resultado)
)

(deffunction PROCESAR_DATOS::distancia (?a ?b)
"Calcula la distancia entre a (Ciudad) y b (Ciudad)"
  (bind ?Ax (send ?a get-PosX))
  (bind ?Ay (send ?a get-PosY))
  (bind ?Bx (send ?b get-PosX))
  (bind ?By (send ?b get-PosY))
  (bind ?dist (+ (abs (- ?Ax ?Bx))(abs (- ?Ay ?By))))
  (return ?dist)
)

(deffunction PROCESAR_DATOS::min-precio-ocio ($?objs)
(bind ?min 0)
(bind ?resultado nil)
(loop-for-count (?i 1 (length$ $?objs)) do
  (bind ?aux (nth$ ?i $?objs))
  (if (eq "Ocio" (send ?aux get-class)) then
    (if (eq ?resultado nil)
      then
      (bind ?min (send ?aux get-Precio))
      (bind ?resultado ?aux)
      else
      (if (< (send ?aux get-Precio) ?min) then
        (bind ?min (send ?aux get-Precio))
        (bind ?resultado ?aux)
      )
    )
  )
)
(return ?resultado)
)

(deffunction PROCESAR_DATOS::min-precio-hospedaje ($?objs)
(bind ?min 0)
(bind ?resultado nil)
(loop-for-count (?i 1 (length$ $?objs)) do
  (bind ?aux (nth$ ?i $?objs))
  (if (eq "Hospedaje" (send ?aux get-class)) then
    (if (eq ?resultado nil)
      then
      (bind ?min (send ?aux get-Precio))
      (bind ?resultado ?aux)
      else
      (if (< (send ?aux get-Precio) ?min) then
        (bind ?min (send ?aux get-Precio))
        (bind ?resultado ?aux)
      )
    )
  )
)
(return ?resultado)
)

;PREGUNTAS PARA RECOGER DATA DEL USUARIO

(defrule RECOPILAR_INFO::determinar_tipo_grupo
  (declare (salience 10))
  (not (datos_grupo))
  =>
  (bind ?d (pregunta_entero "Cuantos adultos sois en el grupo? "))
  (if (= ?d 1) then (bind ?tipo_grupo "Individual"))
  (if (= ?d 2) then
    (bind ?pareja (pregunta_valor_posible "Sois pareja? " si s no n))
    (if (or (eq ?pareja si)(eq ?pareja s)) then (bind ?tipo_grupo "Pareja") else (bind ?tipo_grupo "Grupo")))
  (if (> ?d 2) then (bind ?tipo_grupo "Grupo"))
  (assert (datos_grupo (tipo_grupo ?tipo_grupo)(num_adultos ?d)))
)

(defrule RECOPILAR_INFO::determinar_edad
  (declare (salience 9))
  ?ref <- (datos_grupo)
  (not (test_edad))
  =>
  (bind ?d (pregunta_entero "Cuantos anos teneis? "))
  (if (and (>= ?d 18)(< ?d 30)) then (modify ?ref (edad "Joven")))
  (if (and (>= ?d 30)(< ?d 60)) then (modify ?ref (edad "Adulto")))
  (if (>= ?d 60) then (modify ?ref (edad "Anciano")))
  (assert (test_edad))
)

(defrule RECOPILAR_INFO::determinar_ninos
  (declare (salience 7))
  ?ref <- (datos_grupo (tipo_grupo ?t))
  (not (test_ninos))
  =>
  (bind ?s "")
  (bind ?d (pregunta_valor_posible "Hay ninos en el grupo? " si s no n))
  (if (or (eq ?d si)(eq ?d s))
    then
      (modify ?ref (tipo_grupo "Familia"))
      (modify ?ref (ninos (pregunta_entero "Cuantos? ")))
    else
      (modify ?ref (ninos 0))
  )
  (assert (test_ninos))
  )

  (defrule RECOPILAR_INFO::determinar_numero_ciudades
    (declare (salience 7))
    ?ref <- (datos_grupo)
    (not (test_ciudades))
    =>
    (modify ?ref (num_ciudades (pregunta_entero "Cuantas ciudades quereis ver? ")))
    (assert (test_ciudades))
  )

  (defrule RECOPILAR_INFO::determinar_dias
    (declare (salience 7))
    ?ref <- (datos_grupo)
    (not (test_dias))
    =>
    (modify ?ref (min_num_dias (pregunta_entero "Cuantos dias va a durar el viaje como minimo? ")))
    (modify ?ref (max_num_dias (pregunta_entero "Cuantos dias va a durar el viaje como maximo? ")))
    (assert (test_dias))
  )

(defrule RECOPILAR_INFO::determinar_dias_ciudad
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_dias_ciudad))
  =>
  (modify ?ref (min_dias_ciudad (pregunta_entero "Minimo de dias por ciudad? ")))
  (modify ?ref (max_dias_ciudad (pregunta_entero "Maximo de dias por ciudad? ")))
  (assert (test_dias_ciudad))
)

(defrule RECOPILAR_INFO::determinar_zona
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_zona))
  =>
  (bind ?d (pregunta_valor_posible "Tiene alguna zona preferente para el viaje? " si s no n))
  (if (or (eq ?d si)(eq ?d s))
    then
      (bind ?d (pregunta_valor_posible "Cual? " europa asia africa norteamerica sudamerica))
      (if (eq ?d europa)then(modify ?ref (zona "Europa")))
      (if (eq ?d africa)then(modify ?ref (zona "Africa")))
      (if (eq ?d asia)then(modify ?ref (zona "Asia")))
      (if (eq ?d sudamerica)then(modify ?ref (zona "SudAmerica")))
      (if (eq ?d norteamerica)then(modify ?ref (zona "NorteAmerica")))
  )
  (assert (test_zona))
)

(defrule RECOPILAR_INFO::determinar_presupuesto
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_presupuesto))
  =>
  (bind ?presu (pregunta_entero "Cuanto quieres gastar? "))
  (bind ?presu (+ ?presu(* ?presu (/ 10 100))))
  (modify ?ref (presupuesto ?presu))
  (assert (test_presupuesto))
)

(defrule RECOPILAR_INFO::datos_recopilados
    (declare (salience 1))
    =>
    (focus PROCESAR_DATOS)
    (printout t crlf)
    (printout t crlf)
    (printout t crlf)
    (format t "##########################################################################%n%n")
    (format t "     ESTAMOS PROCESANDO SU VIAJE DE ENSUEÑO, ESPERE UN MICROSEGUNDO ...   %n%n")
    (format t "##########################################################################%n%n")
)

;;; ################################ PROCESO DE DATOS ################################
(defrule PROCESAR_DATOS::inicializar_puntuacion
(declare (salience 10))
	=>
	(bind $?a (find-all-instances ((?inst Ciudad)) TRUE))
	(progn$ (?curr-con ?a)
		(make-instance (gensym) of ciudades_validas (ciudad ?curr-con)(puntos (send ?curr-con get-Valoracion))))
)

(defrule PROCESAR_DATOS::puntuarSegun-edad
  (declare (salience 8))
  (datos_grupo (tipo_grupo ?tg)(edad ?e))
  ?rec <- (object (is-a ciudades_validas)(ciudad ?c)(puntos ?p)(justificaciones $?j))
  (not (valorado-edad ?c))
  =>
  (bind $?temas (send ?c get-Tema))
  (progn$ (?tema ?temas)
    (if (eq ?tema Romantico) then
      (if (eq ?tg "Pareja") then
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas ancianas los viajes romanticos + 8")
        else
        (send ?rec put-puntos (+ (send ?rec get-puntos) 10))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas los viajes romanticos + 10"))))

    (if (eq ?tema Familiar) then
      (if (eq ?tg "Familia") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 10))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las familias los viajes familiares + 10")))

    (if (eq ?tema Cultural) then
      (if (eq ?tg "Individual") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los jovenes que viajan solos los viajes culturales + 2"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los adultos que viajan solos los viajes culturales + 8"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 5))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los ancianos que viajan solos los viajes culturales + 5")))
      (if (eq ?tg "Pareja") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 7))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas jovenes los viajes culturales + 7"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas adultas los viajes culturales + 8"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas ancianas los viajes culturales + 8")))
      (if (eq ?tg "Familia") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las familias los viajes culturales + 8"))
      (if (eq ?tg "Grupo") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 10))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los grupos los viajes culturales + 10")))

    (if (eq ?tema Ocio) then
      (if (eq ?tg "Individual") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 7))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los jovenes que viajan solos los viajes de ocio + 8"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los adultos que viajan solos los viajes de ocio + 6"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 5))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los ancianos que viajan solos los viajes de ocio + 5")))
      (if (eq ?tg "Pareja") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 7))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas jovenes los viajes de ocio + 7"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 6))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas adultas los viajes de ocio + 6"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 6))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas ancianas los viajes de ocio + 6")))
      (if (eq ?tg "Familia") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 7))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las familias los viajes de ocio + 7"))
      (if (eq ?tg "Grupo") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 8))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los grupos los viajes de ocio + 8")))

    (if (eq ?tema Descanso) then
      (if (eq ?tg "Individual") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los jovenes que viajan solos los viajes de descanso + 2"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 4))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los adultos que viajan solos los viajes de descanso + 4"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 7))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los ancianos que viajan solos los viajes de descanso + 7")))
      (if (eq ?tg "Pareja") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 3))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas jovenes los viajes de descanso + 3"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 4))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas adultas los viajes de descanso + 4"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 9))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas ancianas los viajes de descanso + 9")))
      (if (eq ?tg "Familia") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las familias los viajes de descanso + 2"))
      (if (eq ?tg "Grupo") then
        (if (eq ?e "Joven") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los grupos de jovenes los viajes de descanso + 2"))
        (if (eq ?e "Adulto") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 3))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los grupos de adultos los viajes de descanso + 3"))
        (if (eq ?e "Anciano") then
        (send ?rec put-puntos (+ (send ?rec get-puntos) 5))
        (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las grupos de ancianos los viajes de descanso + 5"))))

      (if (eq ?tema Exotico) then
        (if (eq ?tg "Individual") then
          (if (eq ?e "Joven") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los jovenes que viajan solos los viajes exoticos + 6"))
          (if (eq ?e "Adulto") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 4))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los adultos que viajan solos los viajes exoticos + 4"))
          (if (eq ?e "Anciano") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 7))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los ancianos que viajan solos los viajes exoticos + 3")))
        (if (eq ?tg "Pareja") then
          (if (eq ?e "Joven") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 3))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas jovenes los viajes exoticos + 5"))
          (if (eq ?e "Adulto") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 4))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas adultas los viajes exoticos + 4"))
          (if (eq ?e "Anciano") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 9))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las parejas ancianas los viajes exoticos + 2")))
        (if (eq ?tg "Familia") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las familias los viajes exoticos + 6"))
        (if (eq ?tg "Grupo") then
          (if (eq ?e "Joven") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 2))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los grupos de jovenes los viajes exoticos + 5"))
          (if (eq ?e "Adulto") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 3))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A los grupos de adultos los viajes exoticos + 4"))
          (if (eq ?e "Anciano") then
          (send ?rec put-puntos (+ (send ?rec get-puntos) 5))
          (slot-insert$ ?rec justificaciones (+ 1 (length$ $?j)) "A las grupos de ancianos los viajes exoticos + 2"))))
  )
  (assert (valorado-edad ?c))
)

(defrule PROCESAR_DATOS::puntuarSegun-continente
  (declare (salience 7))
  (datos_grupo (zona ?cont))
  ?rec <- (object (is-a ciudades_validas) (ciudad ?c) (puntos ?p) (justificaciones $?j))
  (not (valorado-continente ?c))
  =>
  (bind ?zona (send ?c get-Continente))
  (bind ?n (send ?c get-Nombre))
  (if (not (eq ?cont "")) then
    (if (not(eq ?cont ?zona)) then (send ?rec delete)
    )
  )
  (assert (valorado-continente ?c))
)

(defrule PROCESAR_DATOS::mejor-viaje
  (declare (salience 6))
  (datos_grupo (presupuesto ?presu)(num_ciudades ?n)(num_adultos ?a)(ninos ?e)(max_num_dias ?mnd)(max_dias_ciudad ?mdc))
  (not (mejor-viaje))
  =>
  (bind $?viajes (create$ ))
  (bind $?transportes (create$ ))
  (bind ?precio 0)
  (bind ?dias 0)
  (bind ?p 0)
  (bind $?objs (find-all-instances ((?inst ciudades_validas)) TRUE))
  (bind ?dest (max-puntos $?objs))
  (bind ?viajes (insert$ ?viajes (+ (length$ ?viajes) 1) ?dest))
  (bind ?p (+ ?p (send ?dest get-puntos)))
  (bind $?servicios (send (send ?dest get-ciudad) get-Serv))
  (bind $?servs (create$ ))
  (loop-for-count (?i 0 2) do
  (bind ?servact (min-precio-ocio $?servicios))
  (if (not (eq ?servact nil)) then (bind ?servs (insert$ ?servs (+ (length$ ?servs) 1) ?servact))
  (bind ?precio (+ ?precio (send ?servact get-Precio)))
  (bind $?servicios (delete-member$ $?servicios ?servact)))
  )
  (bind ?hotel (min-precio-hospedaje (send (send ?dest get-ciudad) get-Serv)))
  (bind ?servs (insert$ ?servs (+ (length$ ?servs) 1) ?hotel))
  (bind $?servicios (delete-member$ $?servicios ?hotel))
  (bind $?objs (delete-member$ $?objs ?dest))
  (bind ?precio (+ ?precio (send ?hotel get-Precio)))
  (bind ?dias (+ ?dias ?mdc))
  (while (and (< (length$ $?viajes) ?n) (and (< ?dias ?mnd) (< ?precio ?presu))) do
    (bind ?dest (max-puntos $?objs))
    (bind ?viajes (insert$ ?viajes (+ (length$ ?viajes) 1) ?dest))
    (bind ?p (+ ?p (send ?dest get-puntos)))
    (bind $?servicios (send (send ?dest get-ciudad) get-Serv))
    (loop-for-count (?i 0 2) do
      (bind ?servact (min-precio-ocio $?servicios))
      (if (not (eq ?servact nil)) then (bind ?servs (insert$ ?servs (+ (length$ ?servs) 1) ?servact))
      (bind ?precio (+ ?precio (send ?servact get-Precio)))
      (bind $?servicios (delete-member$ $?servicios ?servact)))
    )
    (bind ?hotel (min-precio-hospedaje (send (send ?dest get-ciudad) get-Serv)))
    (bind ?servs (insert$ ?servs (+ (length$ ?servs) 1) ?hotel))
    (bind $?objs (delete-member$ $?objs ?dest))
    (bind ?precio (+ ?precio (* ?mdc (send ?hotel get-Precio))))
    (bind ?dias (+ ?dias ?mdc))
  )
  (assert (viaje (ciudades $?viajes)(servicios $?servs)(precio ?precio)(dias ?dias)(puntuacion ?p)))
  (assert (mejor-viaje))
)

(defrule PROCESAR_DATOS::asignar-transportes
  (declare (salience 6))
  (viaje (ciudades $?c)(servicios $?servs)(precio ?p)(dias ?d)(puntuacion ?punt))
  (not (estoy-cansao))
  =>
  (bind $?coches (find-all-instances ((?inst Transporte))(eq ?inst:TipoT "Coche")))
  (bind ?coche (nth$ 1 $?coches))
  (bind ?precio ?p)
  (bind $?bcns (find-all-instances ((?inst Ciudad))(eq ?inst:Nombre "Barcelona")))
  (bind ?bcn (nth$ 1 $?bcns))
  (bind ?orig (nth$ 1 $?c))
  (bind ?dist (distancia ?bcn (send ?orig get-ciudad)))
  (bind ?precio (+ ?precio (* ?dist (send ?coche get-PrecioKm))))
  (loop-for-count (?i 2 (length$ $?c)) do
    (bind ?orig (nth$ (- ?i 1) $?c))
    (bind ?dest (nth$ ?i $?c))
    (bind ?dist (distancia (send ?orig get-ciudad) (send ?dest get-ciudad)))
    (bind ?precio (+ ?precio (* ?dist (send ?coche get-PrecioKm))))
  )
  (bind ?ult (nth$ (length$ $?c) $?c))
  (bind ?dist (distancia (send ?ult get-ciudad) ?bcn))
  (bind ?precio (+ ?precio (* ?dist (send ?coche get-PrecioKm))))
  (focus IMPRIMIR_SOL)
  (printout t crlf)
  (printout t crlf)
  (printout t crlf)
  (format t "##########################################################################%n%n")
  (format t "                       TE PRINTEO LA SOL NOMAS                       %n%n")
  (format t "##########################################################################")
  (printout t crlf)
  (printout t crlf)
  (assert (viaje (ciudades $?c)(medioViaje ?coche)(servicios $?servs)(precio ?precio)(dias ?d)(puntuacion ?punt)))
  (assert (estoy-cansao))
)


(defmessage-handler PROCESAR_DATOS::Ocio get-class()
  (return "Ocio")
)
(defmessage-handler PROCESAR_DATOS::Movilidad get-class()
  (return "Movilidad")
)
(defmessage-handler PROCESAR_DATOS::Hospedaje get-class()
  (return "Hospedaje")
)

;; ############################# IMPRIMIMOH LA SOLUCIONAO #########################

(defrule IMPRIMIR_SOL::finalizar-procesarDatos
  (viaje (ciudades $?c)(servicios $?servs)(precio ?p)(dias ?d)(puntuacion ?punt))
  (not (print))
  =>
  (loop-for-count (?i 1 (length$ $?c)) do
    (bind ?aux (nth$ ?i $?c))
    (send ?aux imprimir)
  )
  (printout t "Servicios: " crlf)
  (progn$ (?curr-serv $?servs)
    (send ?curr-serv imprimir-ocio)
  )
  (printout t "Precio (TOTAL): " ?p " BC" crlf)
  (printout t "Dias: " ?d crlf)
  (printout t "Puntuacion: " ?punt crlf)
  (assert (print))
)

(defmessage-handler IMPRIMIR_SOL::ciudades_validas imprimir()
  (send ?self:ciudad imprimir)
  (printout t "Puntuacion: " ?self:puntos crlf)
  (printout t "Justificaciones:" crlf)
  (progn$ (?j ?self:justificaciones)
    (printout t ?j crlf)
  )
)

(defmessage-handler IMPRIMIR_SOL::Ciudad imprimir()
  (printout t "Ciudad: " ?self:Nombre crlf)
  (printout t "Servicios de Movilidad:" crlf)
  (progn$ (?curr_serv ?self:Serv)
    (send ?curr_serv imprimir)
  )
)

(defmessage-handler IMPRIMIR_SOL::Servicio imprimir()
)

(defmessage-handler IMPRIMIR_SOL::Movilidad imprimir()
  (printout t "    Nombre: " (send ?self get-Nombre) crlf)
  (printout t "    Calidad: " (send ?self get-Calidad) crlf)
  (printout t "    Precio: " (send ?self get-Precio) " BC" crlf)
  (printout t crlf)
)

(defmessage-handler IMPRIMIR_SOL::Servicio imprimir-ocio()
(printout t "Nombre: " ?self:Nombre crlf)
(printout t "Calidad: " ?self:Calidad crlf)
(printout t "Precio: " ?self:Precio " BC" crlf)
(printout t crlf)
)

(defmessage-handler IMPRIMIR_SOL::Ocio imprimir-ocio()
  (printout t "Nombre: " (send ?self get-Nombre) crlf)
  (printout t "Tipo: " ?self:TipoOcio crlf)
  (printout t "Calidad: " (send ?self get-Calidad) crlf)
  (printout t "Precio: " (send ?self get-Precio) " BC" crlf)
  (printout t crlf)
)
