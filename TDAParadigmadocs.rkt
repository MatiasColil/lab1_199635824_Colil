#lang racket

(require  "TDAFecha.rkt")

;TDA paradigmadocs
;este TDA representa la plataforma de documentos colaborativos

;CONSTRUCTOR
;Funcion que me crea la plataforma 
;Dom:
;Rec: una lista

(define (crearPlataforma nombre fecha encrypFunction decrypFunction)
  (if (string? nombre)
      (if (esfecha? fecha)
          (list nombre fecha encrypFunction decrypFunction (list) (list) (list))
          ;los tres ultimos elemenos de la lista corresponden a lo siguiente en este orden: lista de usuarios, lista de usuarios activos, lista de documentos
          #f
          )
      #f
      )
  )

;PERTENENCIA
;Funcion que me verifica si es una plataforma de documentos
;Dom: una lista
;Rec: valor booleano

(define (esPlataforma? plataforma)
  (if (and (string? (car plataforma))
           (esfecha? (car (cdr plataforma)))
           )
      #t
      #f
      )
  )

;SELECTORES
;Funcion que retorna el nombre de la plataforma
;Dom: lista
;Rec: string

(define (getNombre plataforma)
  (if (esPlataforma? plataforma)
      (car plataforma)
      #f
      )
  )

;Funcion que retorna la fecha de la plataforma
;Dom: lista
;Rec: string

(define (getFecha plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr plataforma))
      #f
      )
  )

;Funcion que retorna la lista de usuarios 
;Dom: lista
;Rec: lista

(define (getLista-usuarios plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr (cdr (cdr (cdr plataforma)))))
      #f
      )
  )

;Funcion que retorna la lista de usuarios de activos
;Dom: lista
;Rec: lista

(define (getUsuarios-activos plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr (cdr (cdr (cdr(cdr plataforma))))))
      #f
      )
  )

;Funcion que retorna la lista de usuarios de activos
;Dom: lista
;Rec: lista

(define (getLista-documentos plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr (cdr (cdr (cdr(cdr (cdr plataforma)))))))
      #f
      )
  )
