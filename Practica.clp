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
  (slot edad (type STRING)(default ?NONE))
  (slot cultura (type STRING)(default ?NONE))
  (slot ninos (type INTEGER)(default 0))
  (slot edad_ninos (type STRING)(default ?NONE))
  (slot objetivo (type STRING)(default ?NONE))
  (slot evento_especial (type STRING)(default ?NONE))

  ; Restricciones del Viaje
  (slot min_dias (type INTEGER)(default 1))
  (slot max_dias (type INTEGER)(default 1))
  (slot min_ciudades (type INTEGER)(default 1))
  (slot max_ciudades (type INTEGER)(default 1))
  (slot min_dias_ciudad (type INTEGER)(default 1))
  (slot max_dias_ciudad (type INTEGER)(default 1))

  (slot presupuesto (type FLOAT)(default 0.0))
  (slot transporte_no (type STRING)(default ?NONE))
  (slot calidad (type STRING)(default ?NONE))

  ; Preferencias del Viaje
  (multislot sacrificar (type STRING)(default ?NONE))
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
"Escribe una pregunta y lee uno de los valores posibles"
  (printout t ?question)
  (printout t " ")
  (printout ?allowed-values)
  (printout ": ")
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


; Recopilacion de datos
(defrule RECOPILAR_INFO::determinar_cantidad_grupo
    (declare (salience 10))
    (not (datos_grupo))
    =>
    (bind ?d (pregunta_entero "Cantidad del grupo? "))
    (bind ?grupo nil)
    (if (>= ?d 10) then (bind ?grupo "Grande"))
    (if (and (>= ?d 5)(< ?d 10)) then (bind ?grupo "Medio"))
    (if (and(< ?d 5)(> ?d 2)) then (bind ?grupo "Pequeno"))
    (if (= ?d 2) then (bind ?grupo "Pareja"))
    (if (= ?d 1) then (bind ?grupo "Individual"))
    (bind ?d (pregunta_entero "Pregunta full random "))
    (assert (datos_grupos ?grupo))
)

(defrule RECOPILAR_INFO::determinar_edad_grupo
  (declare (salience 8))
  ?ref <- (datos_grupo)
  (not (test_edad_grupo))
  =>
  (bind ?d (pregunta_entero "Edad media del grupo? "))
  (bind ?s "")
  (if (<= ?d 30) then (bind ?s "Joven"))
  (if (and (> ?d 30)(<= ?d 50)) then (bind ?s "Viejoven"))
  (if (and (> ?d 50)(<= ?d 65)) then (bind ?s "Mayor"))
  (if (> ?d 65) then (bind ?s "Inserso"))
  (modify ?ref (edad ?s))
  ;(printout t crlf)
  ;(printout t ?s)
  ;(printout t crlf)
  (assert (test_edad_grupo))
)

(defrule RECOPILAR_INFO::determinar_cultura
  (declare (salience 8))
  ?ref <- (datos_grupo)
  (not (test_cultura))
  =>
  (bind ?d (pregunta_valor_posible "Cultura del grupo? " ALTA MEDIA BAJA))
  (modify ?ref (cultura ?d))
  (assert (test_cultura))
)

(defrule RECOPILAR_INFO::determinar_ninos
  (declare (salience 8))
  ?ref <- (datos_grupo)
  (not (tienen_ninos))
  =>
  (bind ?d (pregunta_entero "Hay ninyos en el grupo? "))
  (if (!= ?d 0) then (modify ?ref (ninos 1))
    (bind ?d (pregunta_entero "Edad de los ninyos del grupo? "))
    (if (<= ?d 5) then (modify ?ref (edad_ninos "Peques")))
    (if (and (> ?d 5)(<= ?d 10)) then (modify ?ref (edad_ninos "Medianos")))
    (if (and (> ?d 10)(<= ?d 18)) then (modify ?ref (edad_ninos "Teen")))
    (if (> ?d 18) then (modify ?ref (edad_ninos "Grandes")))
  )
  (if (= ?d 0) then (modify ?ref (ninos 0)))
  (assert (tienen_ninos))
)
