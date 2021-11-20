#lang racket
(require  "TDAFecha.rkt")

;TDA usuario
;TDA usuario que va a representar a un usuario de una plataforma de documentos colaborativos
;Representacion: es una lista de 4 elementos que representan lo siguiente en este orden
;[ nombre del usuario, contraseña del usuario, estado de actividad/inactividad, fecha de creacion del usuario]

;CONSTRUCTOR
;Funcion que me crea una lista de usuario
;Dom: fecha x usuario x contraseña
;Rec: una lista
(define (crearLista-usuario usuario contraseña login fecha)
  (list usuario contraseña login fecha);login es valor 0 si esta deslogeado o 1 si esta logeado
  )

;PERTENENCIA
;Funcion que me verifica si es una lista de usuarios
;Dom: una lista
;Rec: valor booleano

(define (esLista-usuarios? listaUsuario)
  (if (and (string? (car listaUsuario))
           (string? (car(cdr listaUsuario)))
           (integer? (car(cdr(cdr listaUsuario))))
           (esfecha? (car(cdr(cdr(cdr listaUsuario)))))
           (=(length listaUsuario) 4)
           )
      #t
      #f
      )
  )

;SELECTORES

;Funcion que retorna el nombre del usuario
;Dom: lista
;Rec: string

(define (getUsuario listaUsuario)
  (if (esLista-usuarios? listaUsuario)
      (car listaUsuario)
      #f
      )
  )

;Funcion que retorna la contraseña del usuario
;Dom: lista
;Rec: string

(define (getContraseña listaUsuario)
  (if (esLista-usuarios? listaUsuario)
      (car (cdr listaUsuario))
      #f
      )
  )

;Funcion que me retorna el valor de login del usuario
;Dom: lista;Rec: string
(define (getLogin listaUsuario)
  (if (esLista-usuarios? listaUsuario)
      (car (cdr (cdr listaUsuario)))
      #f
      )
  )

;Funcion que me retorna la fecha de creacion del usuarios
;Dom: lista
;Rec: fecha
(define (getFecha-usuario listaUsuario)
  (if (esLista-usuarios? listaUsuario)
      (car (cdr (cdr (cdr listaUsuario))))
      #f
      )
  )

;MODIFICADORES

;Funcion que me recibe un nuevo nombre y una lista de usuario y reemplaza el nombre de dicha lista
;Dom: list X string
;Rec: list

(define (setNombre listaUsuario nuevoNombre)
  (if (list? listaUsuario)
      (if (string? nuevoNombre)
          (list nuevoNombre (getContraseña listaUsuario) (getLogin listaUsuario) (getFecha-usuario listaUsuario))
          listaUsuario
          )
      listaUsuario
      )
  )

;Funcion que me recibe una nueva contraseña y una lista de usuario y reemplaza la contraseña en dicha lista
;Dom: list X string
;Rec: list

(define (setContraseña listaUsuario nuevaContraseña)
  (if (list? listaUsuario)
      (if (string? nuevaContraseña)
          (list (getUsuario listaUsuario) nuevaContraseña (getLogin listaUsuario) (getFecha-usuario listaUsuario))
          listaUsuario
          )
      listaUsuario
      )
  )

;Funcion que me recibe un nuevo estado de actividad/inactividad y una lista de usuario y lo reemplaza en dicha lista
;Dom: list X string
;Rec: list

(define (setLogin listaUsuario nuevoLogin)
  (if (list? listaUsuario)
      (if (integer? nuevoLogin)
          (list (getUsuario listaUsuario) (getContraseña listaUsuario) nuevoLogin (getFecha-usuario listaUsuario))
          listaUsuario
          )
      listaUsuario
      )
  )


;OTRAS FUNCIONES

;Funcion que me agrega una lista de usuario a una lista de listas usuarios ya existente
;Dom: list x list
;Rec: list

(define (agregarUsuario lista nuevoUsuario-lista)
  (if (list? nuevoUsuario-lista)
      (if (esLista-usuarios? nuevoUsuario-lista)
          (append lista (list nuevoUsuario-lista))
          lista
          )
      lista
      )
  )

;Funcion que me busca si ya existe este usuario
;Dom: list x string
;Rec: valor booleano
;Se utiliza recursion natural

(define (estaUsuario? lista-usuarios usuario)
  (if (null? lista-usuarios)
      #f
      (if (string=? (getUsuario (car lista-usuarios)) usuario)
          #t
          (if (null? (cdr lista-usuarios))
              #f
              (estaUsuario? (cdr lista-usuarios) usuario)
              )
          )
      )
  )

;Funcion que me verifica si un usuario en especifico esta logeado
;Dom: list X string X string
;Rec: valor booleano
;Se utiliza recursion natural

(define (estaLogeado? lista-usuarios nombre contraseña)
  (if (null? lista-usuarios)
      #f
      (if (and (equal? (getUsuario (car lista-usuarios)) nombre)
               (equal? (getContraseña (car lista-usuarios)) contraseña)
               )
          #t
          (estaLogeado? (cdr lista-usuarios) nombre contraseña)
          )
      )
  )

;Funcion que me busca si existe algun usuario logeado
;Dom: list
;Rec: valor booleano
(define (alguienLogeado? listaUsuarios)
  (if (null? listaUsuarios)
      #f
      (if (eq? (getLogin (car listaUsuarios)) 1)
          #t
          (alguienLogeado? (cdr listaUsuarios))
          )
      )
  )

;Funcion que me retorna el nombre del usuario que esta logeado
;Dom: list
;Rec: string
(define (usuarioLogeado lista-usuarios)
  (if (eq? (getLogin (car lista-usuarios)) 1)
      (getUsuario (car lista-usuarios))
      (usuarioLogeado (cdr lista-usuarios))
      )
  )

;Funcion que me retorna la lista de usuario de un usuario en especifico
;Dom: list X string
;Rec: list
(define (listaUser-especifico listaUsuarios usuario)
  (if (eq? (getUsuario (car listaUsuarios)) usuario)  
      (car listaUsuarios)
      (listaUser-especifico (cdr listaUsuarios) usuario)
      )
  )
  

;Funcion que retorna una lista modificada sin el usuario que se quiere logear
;Dom: string X string X string
;Rec: una lista
;Se utiliza recursion de cola

(define (eliminarUsuario listaUsuarios nombre acc)
  (if (null? listaUsuarios)
      acc
      (if (equal? (getUsuario (car listaUsuarios)) nombre)
          (eliminarUsuario (cdr listaUsuarios) nombre acc)
          (eliminarUsuario (cdr listaUsuarios) nombre (append acc (list(car listaUsuarios))))
          )
      )
  )
  
;Funcion que me crea una nueva lista de listas de usuario
;Dom: list X string X string X integer
;Rec: list

(define (nuevaLista-usuarios listaUsuarios nombre estadoLog)
   (append (eliminarUsuario listaUsuarios nombre (list)) (list (setLogin (listaUser-especifico listaUsuarios nombre) estadoLog)))
  )

;Funcion que me retorna un string con los datos del usuario
;Dom: list X string
;Rec: string
(define (Usuario->String listaUsuarios usuario)
  (cond
    [(eq? (getUsuario (car listaUsuarios)) usuario) (string-append "Nombre de usuario: " (getUsuario (car listaUsuarios))"\n"
                                                                   "Fecha de creacion usuario: " (string-join (map number->string (getFecha-usuario (car listaUsuarios)))) "\n"
                                                                   )
                                                    ]
    [(Usuario->String (cdr listaUsuarios) usuario)]
    )
  )

(provide (all-defined-out))