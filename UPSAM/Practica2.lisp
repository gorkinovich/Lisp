;--------------------------------------------------------------------------------
;Práctica 2ª de lisp
;Control del tráfico en una ciudad. Se tiene una serie de cruces, con caminos a
;otros cruces. Y se ha de hacer una función a la que pasaremos una lista de
;cruces, que forman un camino a realizar. La función devolverá:
; + T : Cuando haya realizado el camino sin problemas.
; + No-Consecutivo : Cuando el siguiente cruce al que queremos ir no está
;   comunicado con el actual.
; + Dirección-Contraria : Cuando el siguiente cruce, está comunicado con el
;   actual, pero solo en dirección contraria.
;
; Estructura de la lista:
; (Nodo (siguientes nodosig1 ...))
;
;--------------------------------------------------------------------------------
(setq Cruces
   '((1 (siguientes 2))           (2 (siguientes 3 8))
     (3 (siguientes 4 14))        (4 (siguientes 5 9))
     (5 (siguientes 6))           (6 (siguientes 11))
     (7 (siguientes 1))           (8 (siguientes 7 13))
     (9 (siguientes 15))          (10 (siguientes 5 9))
     (11 (siguientes 10 17))      (12 (siguientes 7 13))
     (13 (siguientes 12 14 19))   (14 (siguientes 13 15))
     (15 (siguientes 14 16 21))   (16 (siguientes 10 15 17))
     (17 (siguientes 16))         (18 (siguientes 12))
     (19 (siguientes 18 21))      (21 (siguientes 22))
     (22 (siguientes 16))
   )
)

;--------------------------------------------------------------------------------
; Rutinas
;--------------------------------------------------------------------------------
(defun DarSiguientes (Plano Nodo)
   (if (null Plano)
      nil
      (if (equal (caar Plano) Nodo)
         (cadar Plano)
         (DarSiguientes (cdr Plano) Nodo)
      )
   )
)

;--------------------------------------------------------------------------------
(defun EstaNodo (Lista Nodo)
   (if (null Lista)
      nil
      (if (equal (car Lista) Nodo)
         T
         (EstaNodo (cdr Lista) Nodo)
      )
   )
)

;--------------------------------------------------------------------------------
(defun ComprobarCruce (Plano Orig Dest)
   (if (EstaNodo (DarSiguientes Plano Orig) Dest)
      T
      (if (EstaNodo (DarSiguientes Plano Dest) Orig)
         'Direccion-Contraria ;El origen esta en los siguientes del destino
         'No-Consecutivo      ;El origen y el destino no están comunicados
      )
   )
)

;--------------------------------------------------------------------------------
(defun ComprobarCamino (Plano Camino)
   (if (null (cdr Camino))
      T
      (if (equal T (setq Error (ComprobarCruce Plano (car Camino) (cadr Camino))))
         (ComprobarCamino Plano (cdr Camino))
         Error
      )
   )
)

;--------------------------------------------------------------------------------
; Pruebas
;--------------------------------------------------------------------------------
(ComprobarCamino Cruces '(1 2 3 4 5 6 11 10 9 15 14 13 19 18 12 7 1))
(ComprobarCamino Cruces '(4 5 10 9 4))
(ComprobarCamino Cruces '(1 14 22 1))

;--------------------------------------------------------------------------------
; Fin
;--------------------------------------------------------------------------------