#lang racket

;TDA fecha
;TDA que representa una fecha en el siguiente formato dia/mes/año y se ordena en una lista en dicho orden
;Representacion
;(entero entero entero)
;(list dia mes año)

;Constructor
;Funcion que me construye el TDA fecha
;Dominio: enteros
;Recorrido: lista

(define (fecha dia mes anio)
  (if (and (integer? dia) (integer? mes) (integer? anio))
      (list dia mes anio)
      #f
      )
  )

;Pertenencia
;Funcion que verifica que el TDA es una fecha
;Dom: una lista
;Rec: un booleano

(define (esfecha? fecha)
  (if (and (and (>= (car fecha) 1) (<= (car fecha) 31))
           (and (>= (car (cdr fecha)) 1) (<= (car (cdr fecha)) 12))
           (and ( not (= (car (cdr (cdr fecha))) 0)))
       )
      #t
      #f
  )
)

;SELECTORES

;Funcion que retorna el dia de una fecha
;Dom= una lista
;Rec= un entero
             
(define (getDia fecha)
  (if (esfecha? fecha)
      (car fecha)
      #f
   )
 )


;Funcion que retornar un mes de una fecha
;Dom= una lista
;Rec = un entero

(define (getMes fecha)
  (if (esfecha? fecha)
      (car (cdr fecha))
      #f
   )
 )

;Funcion que retorna el año de una fecha
;Dom= una lista
;Rec = un entero

(define (getAnio fecha)
  (if (esfecha? fecha)
      (car (cdr (cdr fecha)))
      #f
   )
)

;MODIFICADORES

;Funcion que crea una nueva fecha a partir de una fecha ingresada como argumento y reemplaza el valor del dia
;Dom= una lista y un entero
;Rec= una lista

(define (setDia fecha nuevoDia)
  (if (esfecha? fecha)
      (fecha nuevoDia (getMes fecha) (getAnio fecha))
      null
   )
)

;Funcion que crea una nueva fecha a partir de una fecha ingresada como argumento y reemplaza el valor del mes
;Dom= una lista y un entero
;Rec: una lista

(define (setMes fecha nuevoMes)
  (if (esfecha? fecha)
      (fecha (getDia fecha) nuevoMes (getAnio fecha))
      null
   )
)

;Funcion que crea una nueva fecha a partir de una fecha ingresada como argumento y reemplaza el valor del año
;Dom= una lista y un entero
;Rec: una lista

(define (setAnio fecha nuevoAnio)
  (if (esfecha? fecha)
      (fecha (getDia fecha) (getMes fecha) nuevoAnio)
      null
   )
)

(provide (all-defined-out))