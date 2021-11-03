#lang racket

;TDA Access
;TDA Access va a representar que accesos tienen los usuarios a ciertos documentos

;Representacion
;es una lista de 2 elementos en el siguiente orden:
;[ nombre del usuario, lista de documentos que le han compartido al usuario ]
;esta lista de documentos compartidos tendra la siguiente forma
;[ usuario al que le pertenece el doc, idDoc, permiso dado ]

;Constructor
;Funcion que me construye la lista de accesos
;Dom: string
;Rec: list
(define (crearLista-access usuario usuarioDoc idDoc permiso)
  (list usuario (list(list usuarioDoc idDoc permiso)))
  )

;Pertenecia
;Funcion que me verifica si es una lista de accesos
;Dom: lista
;Rec: valor booleano

(define (esLista-access? lista)
  (if (and (=(length lista) 2)
           (string? (car lista))
           (list? (car (cdr lista)))
           )
      #t
      #f
      )
  )

;Selectores

;Funcion que me retorna el nombre del usuario al que le pertenece la lista de documentos compartidos
;Dom: lista
;Rec: string

(define (getNombre-access lista-access)
  (if (esLista-access? lista-access)
      (car lista-access)
      #f
      )
  )

;Funcion que me retorna la lista de documentos compartidos
;Dom: lista
;Rec: lista

(define (getLista-compartidos lista-access)
  (if (esLista-access? lista-access)
      (car (cdr lista-access))
      #f
      )
  )

;Modificadores

;Funcion que me modifica la lista de documentos compartidos
;Dom: lista de accesos X nueva lista de documentos compartidos
;Rec: lista de accesos

(define (setLista-compartidos lista-access nuevaLista-compartidos)
  (if (esLista-access? lista-access)
      (list (getNombre-access lista-access) nuevaLista-compartidos)
      lista-access
      )
  )

;OTRAS FUNCIONES

;Funcion qu

(define (access . x)
  x
  )


;Funcion que retorna la lista de accesos de un usuario en especifico
;Dom: lista X string
;Rec: lista
(define (listaAccesos-especifico listaAccesos usuario)
  (if (eq? (getNombre-access (car listaAccesos)) usuario)
      (car listaAccesos)
      (listaAccesos-especifico (cdr listaAccesos) usuario)
      )
  )
;Funcion que me verifica si existe un usuario en la lista de listas de accesos
;Dom: list X string
;Rec: valor booleano
(define (estaUsuario-access listaAccesos usuario)
  (if (null? listaAccesos)
      #f
      (if (eq? (getNombre-access  (car listaAccesos)) usuario)
          #t
          (estaUsuario-access (cdr listaAccesos) usuario)
          )
      )
  )

;Funcion que me retorna una nueva lista de documentos que le han compartido al usuario
;Dom: string X string X string X lista
;Rec: lista
(define (agregarPermiso usuarioDoc idDoc permiso listaAccess)
  (append (getLista-compartidos listaAccess) (list(list usuarioDoc idDoc permiso)))
  )

;Funcion que me retorna una lista que contiene las listas de los nuevos accesos
;Dom: list X string X integer X list X list
;Rec: list
(define (accesosNuevos listaAccesos usuarioDoc idDoc agregarAccesos nuevosAccesos);usuarioDoc es el que se logea
  (if (null? agregarAccesos)
      nuevosAccesos
      (if (eq? (estaUsuario-access listaAccesos (car (car agregarAccesos))) #f)
          (accesosNuevos listaAccesos usuarioDoc idDoc (cdr agregarAccesos) (append nuevosAccesos (list (crearLista-access (car (car agregarAccesos)) usuarioDoc idDoc (car (cdr (car agregarAccesos)))))))
          (accesosNuevos listaAccesos usuarioDoc idDoc (cdr agregarAccesos) (append nuevosAccesos
                                                                            (list(setLista-compartidos (listaAccesos-especifico listaAccesos (car (car agregarAccesos)))
                                                                                                  (agregarPermiso usuarioDoc idDoc (car (cdr (car agregarAccesos))) (listaAccesos-especifico listaAccesos (car (car agregarAccesos))))
                                                                                                  ))
                                                                            )
                   )
          )
      )
  )

;Funcion que me retorna la la lista actualizada de accesos
;Dom: list X list
;Rec: list
(define (nuevaLista-accesos listaAccesos AccesosActualizados )
  (if (null? listaAccesos)
      AccesosActualizados
      (if (eq? (estaUsuario-access AccesosActualizados (getNombre-access (car listaAccesos))) #f)
          (nuevaLista-accesos (cdr listaAccesos) (append AccesosActualizados (list (car listaAccesos))))
          (nuevaLista-accesos (cdr listaAccesos) AccesosActualizados)
          )
      )
  )

;(define listaAcc (list (list "user1" (list (list "user5" 1 "w")))))
;(define listaAcc (list (list "user1" (list (list "user5" 1 "w"))) (list "user50" (list (list "user5" 1 "w")))))
;(define agregarAcc (list (list "user2" "r") (list "user3" "w")))
;(define prueba (accesos listaAcc "user1" 4 agregarAcc (list)))
;(define creadosAcc (list (list "user1" "w") (list "user3" "r")))
;(define prueba2 (accesosNuevos listaAcc "user2" 10 creadosAcc (list)))
;(define nuevo (nuevaLista-accesos listaAcc prueba2))

(provide (all-defined-out))