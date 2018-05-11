(defmodule MAIN (export ?ALL))
(defmodule RECOPILAR_INFO (import MAIN ?ALL)(export ?ALL))
;(defmodule PROCESO_DATOS (import MAIN ?ALL)(export ?ALL))
(defmodule IMPRIMIR_SOL (import MAIN ?ALL)(export ?ALL))

(defclass MAIN::Puntuaciones
  (is-a USER) (role concrete)
    (multislot viaje (type INSTANCE)(create-accessor read-write))
    (slot puntuacion (type INTEGER)(default 0)(create-accessor read-write))
    (multislot justificaciones (type STRING)(create-accessor read-write))
)

(deftemplate MAIN::solucion
  (multislot parcialmente (type INSTANCE))
  (multislot adecuado (type INSTANCE))
  (multislot recomendable (type INSTANCE))
)

(deftemplate MAIN::datos_grupo
  ; Caracteristicas del Grupos
  (slot grupo (type STRING)(default ?NONE))
  (slot edad (type STRING)(default ""))
  (slot cultura (type STRING)(default ""))
  (slot ninos (type INTEGER)(default 0))
  (slot edad_ninos (type STRING)(default ""))
  (slot objetivo (type STRING)(default ""))
  (slot evento_especial (type STRING)(default ""))

  ; Restricciones del Viaje
  (slot min_dias (type INTEGER)(default 1))
  (slot max_dias (type INTEGER)(default 1))
  (slot min_ciudades (type INTEGER)(default 1))
  (slot max_ciudades (type INTEGER)(default 1))
  (slot min_dias_ciudad (type INTEGER)(default 1))
  (slot max_dias_ciudad (type INTEGER)(default 1))

  (slot presupuesto (type STRING)(default ""))
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
    (printout t "(si/s/no/n): ")
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
  (assert (datos_grupo (grupo ?grupo)))
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
      (modify ?ref (ninos 1))
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

(defrule RECOPILAR_INFO::determinar_dias_ciudad
  (declare (salience 7))
  ?ref <- (datos_grupo)
  (not (test_dias_ciudad))
  =>
  (modify ?ref (min_dias_ciudad (pregunta_entero "Minimo de dias por ciudad? ")))
  (modify ?ref (max_dias_ciudad (pregunta_entero "Maximo de dias por ciudad? ")))
  (assert (test_dias_ciudad))
)
