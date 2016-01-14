;--------------------------------------------------------------------------------
;Práctica 1ª de lisp
;Albergues de montaña.
;Operaciones:
; + Dando el nombre del refugio, dar si tiene alguna reserva.
; + Dando el tipo de refugio, dar una lista de todos los refugios del mismo tipo.
; + Dando una fecha (dia mes año), dar una lista con las reservas que comienzan
;   dicho día.
; + Como al anterior, pero devolviendo en la lista (nombre-refugio día mes año).
;
; Estructrua de la lista:
; (Numero-reserva
;    (TIPO . tipo)
;    (NOMBRE . nombre-refugio)
;    (INICIO dia mes año)
;    (FIN dia mes año)
;    (RESPONSABLE "nombre de la persona responsable")
; )
;--------------------------------------------------------------------------------
(setq ListaReservas
   '(
      (R001 (TIPO . Refugio) (NOMBRE . D001)
            (INICIO 01 02 03) (FIN 03 02 03)
            (RESPONSABLE "Spiderman")
      )
      (R002 (TIPO . Albergue) (NOMBRE . D002)
            (INICIO 01 02 03) (FIN 03 02 03)
            (RESPONSABLE "Lobezno")
      )
      (R003 (TIPO . Refugio) (NOMBRE . D003)
            (INICIO 01 02 03) (FIN 03 02 03)
            (RESPONSABLE "Daredevil")
      )
   )
)
;--------------------------------------------------------------------------------
(defun DarTipo (Nodo)
   (cond ((null Nodo) nil)
         ((atom (car Nodo)) (DarTipo (cdr Nodo)))
         (T (if (equal (caar Nodo) 'TIPO) (cdar Nodo) (DarTipo (cdr Nodo))))
   )
)
(defun DarNombre (Nodo)
   (cond ((null Nodo) nil)
         ((atom (car Nodo)) (DarNombre (cdr Nodo)))
         (T (if (equal (caar Nodo) 'NOMBRE) (cdar Nodo) (DarNombre (cdr Nodo))))
   )
)
(defun DarInicio (Nodo)
   (cond ((null Nodo) nil)
         ((atom (car Nodo)) (DarInicio (cdr Nodo)))
         (T (if (equal (caar Nodo) 'INICIO) (cdar Nodo) (DarInicio (cdr Nodo))))
   )
)
(defun DarFin (Nodo)
   (cond ((null Nodo) nil)
         ((atom (car Nodo)) (DarFin (cdr Nodo)))
         (T (if (equal (caar Nodo) 'FIN) (cdar Nodo) (DarFin (cdr Nodo))))
   )
)
(defun DarResponsable (Nodo)
   (cond ((null Nodo) nil)
         ((atom (car Nodo)) (DarResponsable (cdr Nodo)))
         (T (if (equal (caar Nodo) 'RESPONSABLE) (cdar Nodo) (DarResponsable (cdr Nodo))))
   )
)

;--------------------------------------------------------------------------------
(defun DarNodoActual (Lista) (car Lista))

;--------------------------------------------------------------------------------
; Dando el nombre del refugio, dar si tiene alguna reserva.
;--------------------------------------------------------------------------------
(defun TieneRefugioReserva (Lista Nombre)
   (cond ((null Lista) nil)
         (T (if (equal (DarNombre (DarNodoActual Lista)) Nombre)
                T
                (TieneRefugioReserva (cdr Lista) Nombre)
            )
         )
   )
)

;--------------------------------------------------------------------------------
; Dando el tipo de refugio, dar una lista de todos los refugios del mismo tipo.
;--------------------------------------------------------------------------------
(defun ListaRefugiosTipo (Lista Tipo)
   (cond ((null Lista) nil)
         (T (if (equal (DarTipo (DarNodoActual Lista)) Tipo)
                (cons (DarNodoActual Lista) (ListaRefugiosTipo (cdr Lista) Tipo))
                (ListaRefugiosTipo (cdr Lista) Tipo)
            )
         )
   )
)

;--------------------------------------------------------------------------------
; Dando una fecha (dia mes año), dar una lista con las reservas que comienzan
; dicho día.
;--------------------------------------------------------------------------------
(defun ListaFechaInicio (Lista dia mes anyo)
   (cond ((null Lista) nil)
         (T (if (equal (DarInicio (DarNodoActual Lista)) (list dia mes anyo))
                (cons (DarNodoActual Lista) (ListaFechaInicio (cdr Lista) dia mes anyo))
                (ListaFechaInicio (cdr Lista) dia mes anyo)
            )
         )
   )
)

;--------------------------------------------------------------------------------
; Como al anterior, pero devolviendo en la lista (nombre-refugio dia mes año).
;--------------------------------------------------------------------------------
(defun ListaFechaInicio2 (Lista dia mes anyo)
   (cond ((null Lista) nil)
         (T (if (equal (DarInicio (DarNodoActual Lista)) (list dia mes anyo))
                (cons (append (list (DarNombre (DarNodoActual Lista))) (DarInicio (DarNodoActual Lista)))
                      (ListaFechaInicio2 (cdr Lista) dia mes anyo)
                )
                (ListaFechaInicio2 (cdr Lista) dia mes anyo)
            )
         )
   )
)

;--------------------------------------------------------------------------------
; Pruebas
;--------------------------------------------------------------------------------
(DarTipo (car ListaReservas))
(DarNombre (car ListaReservas))
(DarInicio (car ListaReservas))
(DarFin (car ListaReservas))
(DarResponsable (car ListaReservas))

(TieneRefugioReserva ListaReservas 'D002)
(TieneRefugioReserva ListaReservas 'D004)
(ListaRefugiosTipo ListaReservas 'refugio)
(ListaRefugiosTipo ListaReservas 'albergue)
(ListaFechaInicio ListaReservas 1 2 3)
(ListaFechaInicio ListaReservas 1 3 3)
(ListaFechaInicio2 ListaReservas 1 2 3)
(ListaFechaInicio2 ListaReservas 1 3 3)
;--------------------------------------------------------------------------------
; Fin
;--------------------------------------------------------------------------------