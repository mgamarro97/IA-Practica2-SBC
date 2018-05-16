(defmodule MAIN (export ?ALL))
(defmodule RECOPILAR_INFO (import MAIN ?ALL)(export ?ALL))
(defmodule PROCESAR_DATOS (import MAIN ?ALL)(export ?ALL))
(defmodule IMPRIMIR_SOL (import MAIN ?ALL)(export ?ALL))

(defclass MAIN::ciudades_validas
  (is-a USER) (role concrete)
    (slot ciudad (type INSTANCE)(create-accessor read-write))
    (slot puntos (type INTEGER)(default 0)(create-accessor read-write))
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
  (slot ninos (type INTEGER)(default 0))
  (slot zona (type STRING)(default ""))

  ; Restricciones del Viaje
  (slot min_num_dias (type INTEGER)(default 1))
  (slot max_num_dias (type INTEGER)(default 1))
  (slot num_ciudades (type INTEGER)(default 1))
  (slot min_dias_ciudad (type INTEGER)(default 1))
  (slot max_dias_ciudad (type INTEGER)(default 1))

  (slot presupuesto (type INTEGER)(default 0))
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

;PREGUNTAS PARA RECOGER DATA DEL USUARIO

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

;(defrule RECOPILAR_INFO::determinar_edad
;  (declare (salience 8))
;  ?ref <- (datos_grupo)
;  (not (test_edad))
;  =>
;  (bind ?s "")
;  (bind ?d (pregunta_entero "Que edad teneis? "))
;  (if (< ?d 30) then (bind ?s "Joven"))
;  (if (and(>= ?d 30)(< ?d 60)) then (bind ?s "Adulto"))
;  (if (>= ?d 60) then (bind ?s "Anciano"))
;  (modify ?ref (edad ?s))
;  (assert (test_edad))
;)

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
;      (bind ?d (pregunta_entero "Que edad tienen los ninos? "))
;      (if (< ?d 3) then (modify ?ref (edad_ninos "Pequeños")))
;      (if (and (>= ?d 3)(< ?d 6)) then (modify ?ref (edad_ninos "Medianos")))
;      (if (and (>= ?d 6)(< ?d 12)) then (modify ?ref (edad_ninos "Grandes")))
;      (if (>= ?d 12) then (modify ?ref (edad_ninos "Adolescentes")))
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

;  (defrule RECOPILAR_INFO::determinar_dias
;    (declare (salience 7))
;    ?ref <- (datos_grupo)
;    (not (test_dias))
;    =>
;    (modify ?ref (min_num_dias (pregunta_entero "Cuantos dias va a durar el viaje como minimo? ")))
;    (modify ?ref (max_num_dias (pregunta_entero "Cuantos dias va a durar el viaje como maximo? ")))
;    (assert (test_dias))
;  )

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

;(defrule RECOPILAR_INFO::determinar_presupuesto
;  (declare (salience 7))
;  ?ref <- (datos_grupo)
;  (not (test_presupuesto))
;  =>
;  (modify ?ref (presupuesto (pregunta_entero "Cuanto quieres gastar? ")))
;  (assert (test_presupuesto))
;)

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
		(make-instance (gensym) of ciudades_validas (ciudad ?curr-con)(puntos 0))
	)
)

(defrule PROCESAR_DATOS::puntuarSegun-dias
  (declare (salience 8))
  (datos_grupo (min_dias_ciudad ?min_c)(max_dias_ciudad ?max_c))
  ?rec <- (object (is-a ciudades_validas) (ciudad ?c) (puntos ?p))
	(not (valorado-dias ?c))
	=>
  (bind ?t (send ?c get-Tamanyo))
  (bind ?n (send ?c get-Nombre))
  (if (or (> ?min_c ?t)(< ?max_c ?t))
    then
    (send ?rec put-puntos (- (send ?rec get-puntos) 200))
    else
    (send ?rec put-puntos (+ (send ?rec get-puntos) 200))
  )
  (assert (valorado-dias ?c))
)

(defrule PROCESAR_DATOS::puntuarSegun-continente
  (declare (salience 7))
  (datos_grupo (zona ?cont))
  ?rec <- (object (is-a ciudades_validas) (ciudad ?c) (puntos ?p))
  (not (valorado-continente ?c))
  =>
  (bind ?zona (send ?c get-Continente))
  (bind ?n (send ?c get-Nombre))
  (if (not (eq ?cont "")) then
    (if (not(eq ?cont ?zona))
      then
      (send ?rec put-puntos (- (send ?rec get-puntos) 500))
      else
      (send ?rec put-puntos (+ (send ?rec get-puntos) 500))
    )
    else
    (send ?rec put-puntos (+ (send ?rec get-puntos) 100))
  )
  (assert (valorado-continente ?c))
)

(defrule PROCESAR_DATOS::pasar-todo-viaje
  (declare (salience 6))
  (datos_grupo (num_adultos ?n)(ninos ?e))
  (not (viaje))
  =>
  (bind $?viajes (create$ ))
  (bind ?precio 0)
  (bind ?dias 0)
  (bind ?na ?n)
  (bind ?euw ?e)
  (bind $?objs (find-all-instances ((?inst ciudades_validas)) TRUE))
  (loop-for-count (?i 1 (length$ $?objs)) do
    (bind ?act (nth$ ?i $?objs))
    (bind ?viajes (insert$ ?viajes (+ (length$ ?viajes) 1) ?act))
    (bind ?t (send (send ?act get-ciudad) get-Tamanyo))
    (bind ?v (send (send ?act get-ciudad) get-Valoracion))
    (bind ?precio_nino (* 10 (* ?euw (+ ?t ?v))))
    (bind ?precio_adulto (* 10 (* (* 5 ?na) (+ ?t ?v))))
    (bind ?precio (+ ?precio (+ ?precio_nino ?precio_adulto)))
    (bind ?dias (+ ?dias ?t))
  )
  (assert (viaje(ciudades $?viajes)(precio ?precio)(dias ?dias)))
)

(defrule PROCESAR_DATOS::eliminar-cosis
  (viaje (ciudades $?c)(precio ?precio)(dias ?dias))
  (datos_grupo (num_ciudades ?n)(num_adultos ?a)(ninos ?e))
  (not (filtrameloto))
  =>
  (bind ?num_ciudades ?n)
  (bind ?na ?a)
  (bind ?euw ?e)
  (while (> (length$ $?c) ?n)
  do
    (bind ?peor_ciudad (min-puntos $?c))
    (bind ?t (send (send ?peor_ciudad get-ciudad) get-Tamanyo))
    (bind ?v (send (send ?peor_ciudad get-ciudad) get-Valoracion))
    (bind ?precio_nino (* 10 (* ?euw (+ ?t ?v))))
    (bind ?precio_adulto (* 10 (* (* 5 ?na) (+ ?t ?v))))
    (bind ?precio_restar (+ ?precio_nino ?precio_adulto))
    (bind ?precio (- ?precio ?precio_restar))
    (bind ?t (send (send ?peor_ciudad get-ciudad) get-Tamanyo))
    (bind ?dias (- ?dias ?t))
    (bind $?c (delete-member$ $?c ?peor_ciudad))
  )
  (focus IMPRIMIR_SOL)
  (format t "               ENHORABUENA, TENEMOS EL VIAJE IDEAL PARA USTED                  %n%n")
  (format t "##########################################################################")
  (printout t crlf)
  (printout t crlf)
  (assert (viaje (ciudades $?c)(precio ?precio)(dias ?dias)))
  (assert (filtrameloto))
)

;; ############################# IMPRIMIMOH LA SOLUCIONAO #########################

(defrule IMPRIMIR_SOL::finalizar-procesarDatos
  (viaje (ciudades $?c)(precio ?p)(dias ?d))
  (not (print))
  =>
  (loop-for-count (?i 1 (length$ $?c)) do
    (bind ?aux (nth$ ?i $?c))
    (send ?aux imprimir)
  )
  (printout t "Precio (TOTAL): " ?p " BC" crlf)
  (printout t "Dias: " ?d crlf)
  (assert (print))
)

(defmessage-handler IMPRIMIR_SOL::ciudades_validas imprimir()
  (send ?self:ciudad imprimir)
)

(defmessage-handler IMPRIMIR_SOL::Ciudad imprimir()
  (printout t "Ciudad: " ?self:Nombre)
  (printout t  " " ?self:Tamanyo " dia(s)" crlf)
  (printout t "Servicios:" crlf)
  (progn$ (?curr_serv ?self:Serv)
    (send ?curr_serv imprimir)
    (printout t crlf)
  )
)

(defmessage-handler IMPRIMIR_SOL::Hospedaje imprimir()
  (printout t "    Nombre: " ?self:Nombre crlf)
  (printout t "    Calidad: " (send ?self get-Calidad) crlf)
  (printout t "    Precio: " (send ?self get-Precio) " BC" crlf)
)

(defmessage-handler IMPRIMIR_SOL::Movilidad imprimir()
  (printout t "    Medio: " (send ?self get-Medio) crlf)
  (printout t "    Precio: " (send ?self get-Precio) " BC" crlf)
)

(defmessage-handler IMPRIMIR_SOL::Ocio imprimir()
  (printout t "    Nombre: " ?self:Nombre crlf)
  (printout t "    Tipo: " ?self:TipoOcio crlf)
  (printout t "    Target: " ?self:Target crlf)
  (printout t "    Calidad: " (send ?self get-Calidad))
  (printout t "    Precio: " (send ?self get-Precio))
)
