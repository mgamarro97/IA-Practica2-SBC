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
  (slot precio (type INTEGER))
  (slot dias (type INTEGER))
)

(deftemplate MAIN::datos_grupo
  ; Caracteristicas del Grupo
  (slot num_adultos (type INTEGER)(default 1))
  (slot tipo_grupo (type STRING)(default ?NONE))
  (slot edad (type STRING)(default ""))
  (slot cultura (type STRING)(default ""))
  (slot ninos (type INTEGER)(default 0))
  (slot edad_ninos (type STRING)(default ""))
  (slot objetivo (type STRING)(default ""))
  (slot evento_especial (type STRING)(default ""))
  (slot zona (type STRING)(default ""))
  (slot valoracion (type INTEGER)(default 0))

  ; Restricciones del Viaje
  (slot min_num_dias (type INTEGER)(default 1))
  (slot max_num_dias (type INTEGER)(default 1))
  (slot min_num_ciudades (type INTEGER)(default 1))
  (slot max_num_ciudades (type INTEGER)(default 1))
  (slot min_dias_ciudad (type INTEGER)(default 1))
  (slot max_dias_ciudad (type INTEGER)(default 1))

  (slot presupuesto (type INTEGER)(default 0))
  (multislot transporte (type STRING)(default ""))
  (slot calidad (type STRING)(default ""))

  ; Preferencias del Viaje
  (slot lugar_conocido (type INTEGER)(default 0))
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
    (printout t ":"$?allowed-values)
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

(deffunction PROCESAR_DATOS::distancia (?a ?b)
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

;--------------------------------------- Recopilacion de datos --------------------------------------
(defrule RECOPILAR_INFO::determinar_parametros
  (declare (salience 10))
  (not (datos_grupo))
  =>
  (bind ?d (pregunta_entero "Quantos adultos sois en el grupo? "))
  (bind ?grupo nil)
  (if (= ?d 1) then (bind ?grupo "Individual"))
  (if (= ?d 2) then (bind ?grupo "Pareja"))
  (if (and (> ?d 2)(< ?d 5)) then (bind ?grupo "Pequeño"))
  (if (and (>= ?d 5)(< ?d 8)) then (bind ?grupo "Mediano"))
  (if (>= ?d 8) then (bind ?grupo "Grande"))
  (assert (datos_grupo (tipo_grupo ?grupo)(num_adultos ?d)))
)

(defrule RECOPILAR_INFO::determinar_edad
  (declare (salience 8))
  ?ref <- (datos_grupo)
  (not (test_edad))
  =>
  (bind ?s "")
  (bind ?d (pregunta_entero "Que edad teneis? "))
  (if (< ?d 30) then (bind ?s "Joven"))
  (if (and(>= ?d 30)(< ?d 60)) then (bind ?s "Adulto"))
  (if (>= ?d 60) then (bind ?s "Anciano"))
  (modify ?ref (edad ?s))
  (assert (test_edad))
)

(defrule RECOPILAR_INFO::determinar_ninos
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_ninos))
  =>
  (bind ?s "")
  (bind ?d (pregunta_valor_posible "Hay ninos en el grupo? " si s no n))
  (if (or (eq ?d si)(eq ?d s))
    then
      (modify ?ref (tipo_grupo "Familia"))
      (modify ?ref (ninos (pregunta_entero "Cuantos? ")))
      (bind ?d (pregunta_entero "Que edad tienen los ninos? "))
      (if (< ?d 3) then (modify ?ref (edad_ninos "Pequeños")))
      (if (and (>= ?d 3)(< ?d 6)) then (modify ?ref (edad_ninos "Medianos")))
      (if (and (>= ?d 6)(< ?d 12)) then (modify ?ref (edad_ninos "Grandes")))
      (if (>= ?d 12) then (modify ?ref (edad_ninos "Adolescentes")))
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
    (modify ?ref (min_num_ciudades (pregunta_entero "Cuantas ciudades quereis ver como minimo? ")))
    (modify ?ref (max_num_ciudades (pregunta_entero "Cuantas ciudades quereis ver como maximo? ")))
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
      (bind ?d (pregunta_valor_posible "Cual? " europa asia africa norteamerica))
      (if (eq ?d europa)then(modify ?ref (zona "Europa")))
      (if (eq ?d africa)then(modify ?ref (zona "Africa")))
      (if (eq ?d asia)then(modify ?ref (zona "Asia")))
      (if (eq ?d norteamerica)then(modify ?ref (zona "NorteAmerica")))
  )
  (assert (test_zona))
)

(defrule RECOPILAR_INFO::determinar_criticas
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_criticas))
  =>
  (bind ?s "")
  (bind ?d (pregunta_valor_posible "Quieres ver viajes bien valorados por otros usuarios? " si s no n))
  (if (or (eq ?d si)(eq ?d s))
  then
    (modify ?ref (valoracion 1))
  )
)

(defrule RECOPILAR_INFO::determinar_presupuesto
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_presupuesto))
  =>
  (modify ?ref (presupuesto (pregunta_entero "Cuanto quieres gastar? ")))
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
    (format t "                              PROCESANDO DATOS ...                        %n%n")
    (format t "##########################################################################")
    (printout t crlf)
    (printout t crlf)
)

;;; ################################ PROCESO DE DATOS ################################
(defrule PROCESAR_DATOS::inicializar_puntuacion
(declare (salience 10))
	=>
	(bind $?a (find-all-instances ((?inst Ciudad)) TRUE))
	(progn$ (?curr-con ?a)
		(make-instance (gensym) of ciudades_validas (ciudad ?curr-con)(puntos 0))
	)
)

;;; ################################  RESTRICCIONES DEL PROBLEMA ################################
(defrule PROCESAR_DATOS::eliminar-dias
    (declare (salience 8))
    (datos_grupo (min_dias_ciudad ?min_c)(max_dias_ciudad ?max_c)(max_num_dias ?max))
    ?rec <- (object (is-a ciudades_validas) (ciudad ?c) (puntos ?p)(justificaciones $?j))
	(not (valorado-dias ?c))
	=>
   (bind ?t (send ?c get-Tamanyo))
   (bind ?n (send ?c get-Nombre))
    (if (or (> ?min_c ?t)(< ?max_c ?t)(< ?max ?t)) then
        (send ?rec delete)
        (printout t "La ciudad "?n" es demasiado grande" crlf)
    )
    (assert (valorado-dias ?c))
)

(defrule PROCESAR_DATOS::eliminar-continente
  (declare (salience 7))
  (datos_grupo (zona ?cont))
  ?rec <- (object (is-a ciudades_validas) (ciudad ?c) (puntos ?p)(justificaciones $?j))
  (not (valorado-continente ?c))
  =>
  (bind ?zona (send ?c get-Continente))
  (bind ?n (send ?c get-Nombre))
  (if (not(eq ?cont "")) then
    (if (not(eq ?cont ?zona)) then
      (send ?rec delete)
      (printout t "La ciudad "?n" no esta en la zona deseada" crlf)
    )
  )
  (assert (valorado-continente ?c))
)

;;; ################################  PREFERENCIAS DEL PROBLEMA ################################

(defrule PROCESAR_DATOS::filtrar-valoracion
  (declare (salience 6))
  (datos_grupo (valoracion ?v))
  ?rec <- (object (is-a ciudades_validas) (ciudad ?c) (puntos ?p)(justificaciones $?j))
  (not (valorado-critica ?c))
  =>
  (bind ?val (send ?c get-Valoracion))
  (bind ?n (send ?c get-Nombre))
  (if (= ?v 1) then
      (send ?rec put-puntos (+ ?p ?val))
  )
  (bind ?val (send ?c get-Valoracion))
  (printout t "La ciudad "?n" tiene "?val" puntos" crlf)
  (assert (valorado-critica ?c))
)

;;; ################################  CALCULO DE LA SOLUCION ################################
