#lang racket

(require  "TDAFecha.rkt")

;TDA paradigmadocs
;este TDA representa la plataforma de documentos colaborativos
;es una lista con los siguientes elementos
;(nombre de la plataforma(string), fecha(entero) ,funcion(encrypt), funcion(decrypt), lista de usuarios, lista de documentos, lista de accesos a documentos compartidos)

;CONSTRUCTOR
;Funcion que me crea la plataforma 
;Dom:
;Rec: una lista

(define (crearPlataforma nombre fecha encrypFn decrypFn)
  (if (string? nombre)
      (if (esfecha? fecha)
          (list nombre fecha encrypFn decrypFn (list) (list) (list))
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
           (=(length plataforma) 7)
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

;Funcion que retorna la funcion encryp
;Dom:
;rec:

(define (getEncryp plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr(cdr plataforma)))
      #f
      )
  )

;Funcion que retorna la funcion decryp
;Dom:
;rec:

(define (getDecryp plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr(cdr (cdr plataforma))))
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

(define (getLista-documentos plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr (cdr (cdr (cdr(cdr plataforma))))))
      #f
      )
  )

;Funcion que me retornar la lista de accesos
;Dom: lista
;Rec: lista
(define (getLista-access plataforma)
  (if (esPlataforma? plataforma)
      (car (cdr (cdr (cdr (cdr(cdr (cdr plataforma)))))))
      #f
      )
  )

;MODIFICADORES

;Funcion que me retorna la plataforma con una nueva lista de documentos
;Dom: plataforma X lista
;Rec: plataforma
(define (setLista-documentos plataforma nuevaLista-documentos)
  (if (esPlataforma? plataforma)
      (list (getNombre plataforma) (getFecha plataforma) (getEncryp plataforma) (getDecryp plataforma) (getLista-usuarios plataforma) nuevaLista-documentos (getLista-access plataforma))
      plataforma
      )
  )

;Funcion que me retorna la plataforma con una nueva lista de usuarios
;Dom: plataforma X lista
;Rec: plataforma
(define (setLista-usuarios plataforma nuevaLista-usuarios)
  (if (esPlataforma? plataforma)
      (list (getNombre plataforma) (getFecha plataforma) (getEncryp plataforma) (getDecryp plataforma) nuevaLista-usuarios (getLista-documentos plataforma)(getLista-access plataforma))
      plataforma
      )
  )

;Funcion que me retorna la plataforma con una nueva lista de accesos
;Dom: plataforma x lista
;Rec: plataforma

(define (setLista-access plataforma nuevaLista-access)
  (if (esPlataforma? plataforma)
      (list (getNombre plataforma) (getFecha plataforma) (getEncryp plataforma) (getDecryp plataforma) (getLista-usuarios plataforma) (getLista-documentos plataforma) nuevaLista-access)
      plataforma
      )
  )

;OTRAS FUNCIONES

;Funcion que me "encripta" un texto (funcion que se encuentra en el enunciado del laboratorio)
;Dom: string
;Rec: list

(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

(provide (all-defined-out))