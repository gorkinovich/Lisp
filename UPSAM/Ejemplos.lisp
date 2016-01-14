;--------------------------------------------------------------------------------
(defun eliminar (x)
  (cond ((null x) nil)
    ((atom x) (list x))
    ((not (atom x)) (append (eliminar (car x)) (eliminar (cdr x))))
    )
)
(eliminar '((Ivan) ((es) (un)) (cutre) ((que (no (sabe programar en lisp))) jej)))
;--------------------------------------------------------------------------------
(defun eliminar (x)
  (cond ((null x) nil)
    ((atom x) (list x))
    (T (append (eliminar (car x)) (eliminar (cdr x))))
    )
)
(eliminar '((Ivan) ((es) (un)) (cutre) ((que (no (sabe programar en lisp))) jej)))
;--------------------------------------------------------------------------------
(defun darcodigo (x)
    (if (numberp x) 'Numero 'Simbolo)
)
(defun codificar (x)
  (cond ((null x) nil)
    ((atom x) (darcodigo x))
    (T (cons (codificar (car x)) (codificar (cdr x))))
    )
)
(codificar '((GGG) ((444) (JJJJ)) (8888)))
;--------------------------------------------------------------------------------
(defun codificar (x)
  (cond ((null x) nil)
    ((atom x) (if (numberp x) 'Numero 'Simbolo))
    (T (cons (codificar (car x)) (codificar (cdr x))))
    )
)
(codificar '((GGG) ((444) (JJJJ)) (8888)))
;--------------------------------------------------------------------------------
;x = Lista
;n = Nivel
;Si X es nulo o es un atomo, entonces devolver n.
;Sino, pasar al siguiente nodo de la lista, pasandole
; el máximo entre el nivel anterior (n), o el nivel resultante
; de la profundida del nodo actual (car x).
(defun prof (x n)
  (if (or (null x) (atom x)) n
    (progn (setq m (+ (profundidad (car x)) 1))
           (prof (cdr x) (if (> n m) n m))
    )
  )
)
(defun profundidad (x) (prof x 0))

(profundidad '((hola)))
(profundidad '((hola) pepe juan))
(profundidad '(hola (pepe) juan))
(profundidad '(hola pepe (juan)))
(profundidad '((Ivan) ((es) (un)) (cutre) ((que (no (sabe programar en lisp))) jej)))
;--------------------------------------------------------------------------------