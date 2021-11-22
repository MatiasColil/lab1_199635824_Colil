#lang racket

;TDA Access
;TDA Access va a representar los accesos que tienen los usuarios a ciertos documentos que les han compartido

;Representacion
;Es una lista de 2 elementos en el siguiente orden:
;[ nombre del usuario, lista de documentos que le han compartido al usuario ]

;La lista de documentos compartidos tendra la siguiente forma
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

(define (esLista-access? listaAccess)
  (if (and (=(length listaAccess) 2)
           (string? (car listaAccess))
           (list? (car (cdr listaAccess)))
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

;Funcion que recibe n argumentos y los retorna en una lista
;Dom: list
;Rec: list
(define (access . x)
  x
  )

;Funcion que retorna la lista de accesos de un usuario en especifico
;Dom: lista X string
;Rec: lista
(define (listaAccesos-especifico listaAccesos usuario)
  (if (null? listaAccesos)
      #f
      (if (eq? (getNombre-access (car listaAccesos)) usuario)
          (car listaAccesos)
          (listaAccesos-especifico (cdr listaAccesos) usuario)
          )
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

;Funcion que retorna a que usuario le pertenece el documento basado en el Id del documento
;Dom: list X string
;Rec: string
(define (userPertenencia listaAccesos idDoc)
  (if (eq? (car (cdr (car listaAccesos))) idDoc)
      (car (car listaAccesos))
      (userPertenencia (cdr listaAccesos) idDoc)
      )
  )

;Funcion que me verifica si un usuario tiene permiso a un documento en especifico
;Dom: string X string X integer X list
;Rec: valor booleano
(define (tienePermiso? usuario permiso idDoc listaAccesos)
  (if (null? listaAccesos)
      #f
      (if (eq? (getNombre-access (car listaAccesos)) usuario);
          (if (and (eq? (car (cdr (car (getLista-compartidos (car listaAccesos))))) idDoc)
                   (eq? (car (cdr (cdr (car (getLista-compartidos (car listaAccesos)))))) permiso)
                   )
              #t
              #f
              )
          (tienePermiso? usuario permiso idDoc (cdr listaAccesos))
          )
      )
  )

;Funcion que me verifica si un usuario a compartido un documento
;Dom: list X string
;Rec: valor booleano
(define (compartido? listaCompartidos usuario)
  (if (null? listaCompartidos)
      #f
      (if (eq? (car (car listaCompartidos)) usuario)
          #t
          (compartido? (cdr listaCompartidos) usuario)
          )
      )
  )

;Funcion que me retorna una nueva lista de documentos compartidos 
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

;Funcion que me retorna la lista actualizada de accesos
;Dom: list X list
;Rec: list
(define (nuevaLista-accesos listaAccesos AccesosActualizados)
  (if (null? listaAccesos)
      AccesosActualizados
      (if (eq? (estaUsuario-access AccesosActualizados (getNombre-access (car listaAccesos))) #f)
          (nuevaLista-accesos (cdr listaAccesos) (append AccesosActualizados (list (car listaAccesos))))
          (nuevaLista-accesos (cdr listaAccesos) AccesosActualizados)
          )
      )
  )

;Funcion que me elimina el acceso que un usuario a dado a sus documentos de la lista de documentos compartidos de un usuario
;Dom: list X string
;Rec: list
(define (eliminarAcceso listaCompartidos usuario)
  (filter (lambda (user)
            (not (eq? (car user) usuario)))
          listaCompartidos
          )
  )

;Funcion que revoca todos los accesos que un usuario a dado a sus documentos a otros usuarios
;Dom: list X string X list
;Rec: list
(define (accesosRevocados listaAccesos usuarioRevocar nuevaLista)
  (if (null? listaAccesos)
      nuevaLista
      (if (eq? (compartido? (getLista-compartidos (car listaAccesos)) usuarioRevocar) #t)
          (accesosRevocados (cdr listaAccesos) usuarioRevocar (append nuevaLista (list (setLista-compartidos
                                                                                        (car listaAccesos)
                                                                                        (eliminarAcceso (getLista-compartidos (car listaAccesos)) usuarioRevocar)
                                                                                        ))
                                                                      )
                            )
          (accesosRevocados (cdr listaAccesos) usuarioRevocar nuevaLista)
          )
      )
  )
;Funcion que me transforma la lista de permisos a string
;Dom: list X string
;Rec: string
(define (Compartidos->String listaPermisos stringVersiones)
  (if (null? listaPermisos)
      stringVersiones
      (Compartidos->String (cdr listaPermisos) (string-append stringVersiones "\n" "Usuario al que le pertenece el documento: "(car (car listaPermisos)) "\n"
                                                              "Id del documento: " (string-append (number->string (car (cdr (car listaPermisos))))) "\n"
                                                              "Tipo de permiso: " (list->string (list (car (cdr (cdr (car listaPermisos))))))
                                                              )
                           )
      )
  )
  
;Funcion que me transforma la lista de accesos de un usuario a string
;Dom: list X string
;Rec: string
(define (Access->String listaAccesos usuario)
  (cond
    [(null? listaAccesos) (string-append "no existen archivos compartidos con este usuario" " ")]
    [(eq? (getNombre-access (car listaAccesos)) usuario) (string-append "Accesos dados: " (Compartidos->String
                                                                                           (getLista-compartidos (car listaAccesos))
                                                                                           " ") "\n"
                                                                                                )
                                                         ]
    [else (Access->String (cdr listaAccesos) usuario)]
    )
  )

(provide (all-defined-out))