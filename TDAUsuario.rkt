#lang racket
;TDA usuario

;CONSTRUCTOR
;Funcion que me crea una lista de usuario
;Dom: usuario x contraseña
;Rec: una lista
(define (crearLista-usuario usuario contraseña)
  (list usuario contraseña)
  )

;PERTENENCIA
;Funcion que me verifica si es una lista de usuarios
;Dom: una lista
;Rec: valor booleano

(define (esLista-usuarios? lista)
  (if (and (string? (car lista))
           (string? (car(cdr lista)))
           )
      #t
      #f
      )
  )

;SELECTORES

;Funcion que retorna el nombre del usuario
;Dom: lista
;Rec: string

(define (getUsuario lista)
  (if (list? lista)
      (car lista)
      #f
      )
  )

;Funcion que retorna la contraseña del usuario
;Dom: lista
;Rec: string

(define (getContraseña lista)
  (if (list? lista)
      (car (cdr lista))
      #f
      )
  )

;MODIFICADORES
;Funcion que me recibe un nuevo nombre y una lista de usuario y reemplaza el nombre de dicha lista
;Dom: string x lista
;Rec: una lista

(define (setNombre lista nuevoNombre)
  (if (list? lista)
      (if (string? nuevoNombre)
          (list nuevoNombre (getContraseña lista))
          #f
          )
      #f
      )
  )

;Funcion que me recibe un nuevo nombre y una lista de usuario y reemplaza el nombre de dicha lista
;Dom: string x lista
;Rec: una lista

(define (setContraseña lista nuevaContraseña)
  (if (list? lista)
      (if (string? nuevaContraseña)
          (list (getUsuario lista) nuevaContraseña)
          #f
          )
      #f
      )
  )

;OTRAS FUNCIONES

;Funcion que me agrega una lista de usuario a una lista de usuarios ya existente
;Dom: list x list
;Rec: list

(define (agregarUsuario lista nuevoUsuario-lista)
  (if (list? nuevoUsuario-lista)
      (if (esLista-usuarios? nuevoUsuario-lista)
          (list lista nuevoUsuario-lista)
          lista
          )
      lista
      )
  )






(provide (all-defined-out))