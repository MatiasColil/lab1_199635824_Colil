#lang racket
(require  "TDAFecha.rkt")
(require  "TDAParadigmadocs.rkt")

;TDA Documentos
;TDA en el cual voy a representar los documentos que cada usuario tiene a travez de una lista

;Representacion
;es una lista que tiene 5 elementos en el siguiente orden:
;[nombre del usuario, id del documento , nombre documento, lista de versiones del documento del usuario, fecha de creacion del documento]

; la lista de versiones del documento del usuario tiene la siguiente forma:
;[ ( (doc1 version1) ) , ......]
;la cual el usuario al ir modificando sus documentos sera vera de esta forma la lista
;[ ( (doc1 version1) , (doc1 version2), ... ) ]

;Constructor
;Funcion que me construye una lista documento
;Dom: string X lista

(define (crearDoc nombreUsuario idDocumento nombreDoc texto fecha)
  (list nombreUsuario idDocumento nombreDoc (list (list texto fecha)) fecha)
  )

;Pertenencia
;Funcion que me verifica si es una lista de documento
;Dom: lista
;Rec: valor booleano

(define (esDoc? listaDoc)
  (if (and (list? listaDoc)
           (= (length listaDoc) 5)
           (string? (car listaDoc))
           (integer? (car (cdr listaDoc)))
           (string? (car (cdr (cdr listaDoc))))
           (list? (car (cdr (cdr (cdr listaDoc)))))
           (esfecha? (car (cdr (cdr (cdr (cdr listaDoc))))))
           )
      #t
      #f
      )
  )

;Selectores

;Funcion que me retorna el nombre del usuario de la lista documento del usuario
;Dom: una lista
;Rec: un entero

(define (getUsuario-doc listaDocumento)
  (if (esDoc? listaDocumento)
      (car listaDocumento)
      #f
      )
  )

;Funcion que me retorna el ID del documento
;Dom: una lista
;Rec: un string

(define (getId-doc listaDocumento)
  (if (esDoc? listaDocumento)
      (car (cdr listaDocumento))
      #f
      )
  )

;Funcion que me retorna el nombre del documento
;Dom: una lista
;Rec: una lista

(define (getNombre-doc listaDocumento)
  (if (esDoc? listaDocumento)
      (car (cdr (cdr listaDocumento)))
      #f
      )
  )
;Funcion que me retorna la lista de versiones del documento
;Dom: una lista
;Rec: una fecha

(define (getLista-versiones listaDocumento)
  (if (esDoc? listaDocumento)
      (car (cdr (cdr (cdr listaDocumento))))
      #f
      )
  )

;Funcion que retorna la fecha de creacion del documento

(define (getFecha-doc listaDocumento)
  (if (esDoc? listaDocumento)
      (car (cdr (cdr (cdr (cdr listaDocumento)))))
      #f
      )
  )

;Modificador

;Funcion que me modifica la lista de versiones del documento
;Dom: una lista
;Rec: una lista

(define (setLista-doc listaUsuario-doc nuevaLista-doc)
  (if (esDoc? listaUsuario-doc)
      (list (getUsuario-doc listaUsuario-doc) (getId-doc listaUsuario-doc) (getNombre-doc listaUsuario-doc) nuevaLista-doc (getFecha-doc listaUsuario-doc))
      #f
      )
  )
      
;Funciones extras

;Funcion que me agrega una lista documento a una lista de listas de documentos
;Dom: list X list
;Rec: list
(define (agregarDocumento lista-documentos documento)
  (if (and (list? documento)
           (esDoc? documento)
           )
      (append lista-documentos (list documento))
      lista-documentos
      )
  )

;Funcion que me agrega una nueva version de un documento
;Dom: list X string
;Rec: list
(define (agregarNueva-versionDoc listaVersiones nuevaVersion)
  (if (eq? (null? nuevaVersion) #f)
      (append listaVersiones (list nuevaVersion))
      listaVersiones
      )
  )

;Funcion que me añade texto al final de un documento
;Dom: string X string
;Rec: list
(define (nuevaTexto versionActiva contenidoAgregar fecha)
  (list (encryptFn(string-append versionActiva " " contenidoAgregar)) fecha)
  )

;Funcion que me retorna la ultima version activa
;Dom: list
;Rec: string
(define (ultimaVersion listaVersiones)
  (car (car (reverse listaVersiones)))
  )

;Funcion que me verifica si ya existe un documento en una lista de listas de documentos
;Dom: list X string
;Rec: valor booleano
(define (existeDoc? listaDocumentos usuario id)
  (if (null? listaDocumentos)
      #f
      (if (and
           (eq? (getUsuario-doc  (car listaDocumentos)) usuario)
           (eq? (getId-doc (car listaDocumentos)) id))
          #t
          (existeDoc? (cdr listaDocumentos) usuario id)
          )
      )
  )

;Funcion que me retorna una lista de listas de documentos actualizada
;Dom: list X list
;Rec: list
;Recursion de cola

(define (nuevaLista-doc listaDocumentos listaDoc-actualizada )
  (if (null? listaDocumentos)
      listaDoc-actualizada
      (if (eq? (existeDoc? listaDoc-actualizada (getUsuario-doc (car listaDocumentos)) (getId-doc (car listaDocumentos)) ) #f)
          (nuevaLista-doc (cdr listaDocumentos) (append listaDoc-actualizada (list (car listaDocumentos))))
          (nuevaLista-doc (cdr listaDocumentos) listaDoc-actualizada)
          )
      )
  )

;Funcion que me retorna una lista de documentos de un usuario en especifico
;Dom: list X string X integer
;Rec: list
(define (listaDoc-especifico listaDocumentos usuario id)
  (if (and (eq? (getUsuario-doc (car listaDocumentos)) usuario)
           (eq? (getId-doc (car listaDocumentos)) id)
           )
      (car listaDocumentos)
      (listaDoc-especifico (cdr listaDocumentos) usuario id)
      )
  )

;Funcion que me agrega una nueva version de un documento a la lista de documentos de un usuario en especifico
;Dom: list X string X integer X string X list
;Rec: list
(define (agregarNueva-version listaDocumentos usuario id contenidoAgregar fecha)
  (list(setLista-doc (listaDoc-especifico listaDocumentos usuario id) (agregarNueva-versionDoc (getLista-versiones (listaDoc-especifico listaDocumentos usuario id))
                                                                                               (nuevaTexto (encryptFn(ultimaVersion (getLista-versiones (listaDoc-especifico listaDocumentos usuario id))))
                                                                                                             contenidoAgregar
                                                                                                             fecha
                                                                                                             )
                                                                                               )
                     )
       )
  )

;Funcion que me asigna un ID a un nuevo documento creado por un usuario
;Dom: list X string X integer
;Rec: integer
(define (asignarIdDoc listaDocumentos usuario nuevoId)
  (if (null? listaDocumentos)
      (+ 1 nuevoId)
      (if (eq? (getUsuario-doc (car listaDocumentos)) usuario)
          (if (eq? (>= (getId-doc (car listaDocumentos)) nuevoId) #t)
              (asignarIdDoc (cdr listaDocumentos) usuario (getId-doc (car listaDocumentos)))
              (asignarIdDoc (cdr listaDocumentos) usuario nuevoId)
              )
          (asignarIdDoc (cdr listaDocumentos) usuario nuevoId)
          )
      )
  )

;Funcion que me restaura una version anterior de un documento
;Dom: list X integer X integer
;Rec: list
(define (restaurarVersion listaVersiones idVersion contador)
  (if (= idVersion contador)
      (car listaVersiones)
      (restaurarVersion (cdr listaVersiones) idVersion (+ contador 1))
      )
  )

;Funcion que me agrega una version anterior de un documento a la lista de documentos de un usuario en especifico
;Dom: list X string X integer X string X list
;Rec: list

(define (agregarVersion-anterior listaDocumentos usuario idDoc idVersion)
  (list (setLista-doc (listaDoc-especifico listaDocumentos usuario idDoc) (agregarNueva-versionDoc (getLista-versiones (listaDoc-especifico listaDocumentos usuario idDoc))
                                                                                                   (restaurarVersion (getLista-versiones (listaDoc-especifico listaDocumentos usuario idDoc))
                                                                                                                     idVersion
                                                                                                                     0
                                                                                                                     )
                                                                                                   )
                      )
        )
  )

;Funcion que me retorna un documento si la frase que se esta buscando se encuentra en dicho documento
;Dom: list X string X list
;Rec: list
(define (buscarFrase listaVersiones frase acc)
  (if (null? listaVersiones)
      acc
      (if (string-contains? (encryptFn (car (car listaVersiones))) frase)
          (buscarFrase (cdr listaVersiones) frase (append acc (list (car listaVersiones))))
          (buscarFrase (cdr listaVersiones) frase acc)
          )
      )
  )

;Funcion que me retorna todos los documentos, junto con sus versiones, que un usuario tiene acceso, sea de escritura o lectura
;Dom: list X list X list X string X list
;Rec: list
(define (obtenerDocs listaDocumentos listaDocumentos2 listaPermisos usuario acc)
  (if (null? listaPermisos)
      acc
      (if (null? listaDocumentos)
          (cond
            [(null? listaPermisos) acc]
            [(eq? (car (cdr (cdr (car listaPermisos)))) #\w) (obtenerDocs listaDocumentos listaDocumentos2 (cdr listaPermisos) usuario (append acc (getLista-versiones (listaDoc-especifico listaDocumentos2
                                                                                                                                                                                      (car (car listaPermisos))
                                                                                                                                                                                      (car (cdr (car listaPermisos)))))))]
            [(eq? (car (cdr (cdr (car listaPermisos)))) #\r) (obtenerDocs listaDocumentos listaDocumentos2 (cdr listaPermisos) usuario (append acc (getLista-versiones (listaDoc-especifico listaDocumentos2
                                                                                                                                                                                      (car (car listaPermisos))
                                                                                                                                                                                      (car (cdr (car listaPermisos)))))))]
            [else (obtenerDocs listaDocumentos listaDocumentos2 (cdr listaPermisos) acc)]
            )
          (if (eq? (getUsuario-doc (car listaDocumentos)) usuario)
              (obtenerDocs (cdr listaDocumentos) listaDocumentos2 listaPermisos usuario (append acc (getLista-versiones (car listaDocumentos))))
              (obtenerDocs (cdr listaDocumentos) listaDocumentos2 listaPermisos usuario acc)
              )
          )
      )
  )

;Funcion que me transforma todas las versiones de un documento a string
;Dom: list X integer X string
;Rec: string
(define (Versiones->String listaVersiones numeroVersion stringVersiones)
  (if (null? listaVersiones)
      stringVersiones
      (Versiones->String (cdr listaVersiones) (+ 1 numeroVersion) (string-append stringVersiones "\n"
                                                             "version " (number->string numeroVersion) ": "(encryptFn(car (car listaVersiones))) "\n"
                                                             "fecha de version: "(string-join (map number->string (car (cdr (car listaVersiones)))))
                                                             )
                         )
      )
  )

;Funcion que me transforma todos los documentos de un usuario a string
;Dom: list X string X string
;Rec: string
(define (Documentos->String listaDocumentos usuario stringDocs)
  (cond
    [(and (null? listaDocumentos)
          (eq? stringDocs " ")) (string-append "este usuario no tiene documentos creados")]
    [(null? listaDocumentos) stringDocs]
    [(eq? (getUsuario-doc (car listaDocumentos)) usuario) (Documentos->String (cdr listaDocumentos)
                                                                            usuario
                                                                            (string-append stringDocs "\n"
                                                                                           "Nombre de documento: " (getNombre-doc (car listaDocumentos)) "\n"
                                                                                           "Versiones: " (Versiones->String (getLista-versiones (car listaDocumentos)) 0 " ") "\n"
                                                                                           )
                                                                            )
                                                        ]
    [else (Documentos->String (cdr listaDocumentos) usuario stringDocs)]
    )
  )

(provide (all-defined-out))