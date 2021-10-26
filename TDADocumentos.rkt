#lang racket
(require  "TDAFecha.rkt")

;TDA Documentos
;TDA en el cual voy a representar los documentos que cada usuario tiene a travez de una lista

;Representacion
;es una lista que tiene 5 elementos en el siguiente orden:
;[nombre del usuario, id del documento , nombre documento, lista de documentos del usuario, fecha]
; la lista de documentos del usuario tiene la siguiente forma:
;[ ( (doc1 version1) ) , ......]
;la cual el usuario al ir modificando sus documentos sera vera asi esta lista
;[ ( (doc1 version1) , (doc1 version2), ... ) ]

;Constructor
;Funcion que me construye la lista de documentos del usuario
;Dom: string X lista

(define (crearDoc nombreUsuario idDocumento nombreDoc texto fecha)
  (list nombreUsuario idDocumento nombreDoc (list (list texto )) fecha)
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

(define (getUsuario-doc listaUsuario)
  (if (esDoc? listaUsuario)
      (car listaUsuario)
      #f
      )
  )

;Funcion que me retorna el ID del documento
;Dom: una lista
;Rec: un string

(define (getId-doc listaUsuario)
  (if (esDoc? listaUsuario)
      (car (cdr listaUsuario))
      #f
      )
  )

;Funcion que el nombre del usuario que creo el documento
;Dom: una lista
;Rec: una lista

(define (getNombre-doc listaUsuario)
  (if (esDoc? listaUsuario)
      (car (cdr (cdr listaUsuario)))
      #f
      )
  )
;Funcion que me retorna la lista de versiones del documento
;Dom: una lista
;Rec: una fecha

(define (getLista-doc listaUsuario)
  (if (esDoc? listaUsuario)
      (car (cdr (cdr (cdr listaUsuario))))
      #f
      )
  )

;Funcion que retorna la fecha de creacion del documento

(define (getFecha-doc listaUsuario)
  (if (esDoc? listaUsuario)
      (car (cdr (cdr (cdr (cdr listaUsuario)))))
      #f
      )
  )



;Modificador
;Funcion que me modifica la lista de documentos
;Dom: una lista
;Rec: una lista

(define (setLista-doc listaUsuario-doc nuevaLista-doc)
  (if (esDoc? listaUsuario-doc)
      (list (getUsuario-doc listaUsuario-doc) (getId-doc listaUsuario-doc) (getNombre-doc listaUsuario-doc) nuevaLista-doc (getFecha-doc listaUsuario-doc))
      #f
      )
  )
      
;Funciones extras

(define (agregarNueva-versionDoc listaVersiones nuevaVersion)
  (if (eq? (null? nuevaVersion) #f)
      (append listaVersiones nuevaVersion)
      listaVersiones
      )
  )


(provide (all-defined-out))