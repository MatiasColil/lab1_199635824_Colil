#lang racket

(require  "TDAFecha.rkt")
(require  "TDAUsuario.rkt")
(require  "TDAParadigmadocs.rkt")
(require  "TDADocumentos.rkt")
(require  "TDAAccess.rkt")
;---------------------- Requisitos Funcionales ------------------------------


;FUNCION REGISTER
;Funcion que me registra un nuevo usuario, en el caso de que este usuario ya exista se retorna la misma plataforma sin modificar
;Dom: plataforma x usuario(string) x contraseña(string)
;Rec: plataforma actualizado
;Recursion natural

(define (register plataforma usuario contraseña fecha)
  (if (equal? (estaUsuario? (getLista-usuarios plataforma) usuario) #f)
      (setLista-usuarios plataforma (agregarUsuario (getLista-usuarios plataforma) (crearLista-usuario usuario contraseña 0 fecha (asignarID (getLista-usuarios plataforma) 0))))
      plataforma
      )
  )

;FUNCION LOGIN
;Funcion que cambia el estado de actividad del usuario, en otras palabras logeada
;Dom: paraddigmadocs (plataforma) X usuario (string) X contraseña (string)
;Rec: funcion

(define (login plataforma usuario contraseña operacion)
  (if (eq? (estaLogeado? (getLista-usuarios plataforma) usuario contraseña 0) #t)
      (setLista-usuarios plataforma (nuevaLista (getLista-usuarios plataforma) usuario contraseña (fechaEspecifica (getLista-usuarios plataforma) usuario) 1 (IdEspecifico (getLista-usuarios plataforma) usuario)))
      (+ 1 2)
      )
  )

;FUNCION CREATE
;Funcion que me crea un documento que esta asociado al nombre del usuario
;Dom: paradigmadocs X fecha X nombre documento (string) X contenido documento (string)
;Rec: paradigmadocs modificado

(define (create plataforma nombreDoc contenido fecha)
  (if (and (string? nombreDoc)
           (string? contenido)
           (esfecha? fecha)
           )
      (setLista-documentos (setLista-usuarios plataforma (nuevaLista (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) (usuarioPass (getLista-usuarios plataforma)) (fechaEspecifica (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma))) 0 (IdEspecifico (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)))))
                           (agregarDocumento (getLista-documentos plataforma) (crearDoc (usuarioLogeado (getLista-usuarios plataforma)) 1 nombreDoc ((getEncryp plataforma) contenido) fecha)))
      plataforma
      )
  )

;FUNCION SHARE

(define (share plataforma idDoc access . accessess);
  (setLista-access (setLista-usuarios plataforma (nuevaLista (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) (usuarioPass (getLista-usuarios plataforma)) (fechaEspecifica (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma))) 0 (IdEspecifico (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)))))
                   (nuevaLista-accesos (getLista-access plataforma)
                                                  (accesosNuevos (getLista-access plataforma)
                                                                 (usuarioLogeado (getLista-usuarios plataforma))
                                                                 idDoc
                                                                 (append (list access) accessess)
                                                                 (list)
                                                                 )
                                                  )
                   )
  )
  

;(define emptyGDocs (crearPlataforma "gDocs" (crearFecha 25 10 2021) encryptFn encryptFn))
;(define gDocs1(register (register (register emptyGDocs "user1" "pass1" (crearFecha 25 10 2021)) "user2" "pass2" (crearFecha 25 10 2021)) "user3" "pass3" (crearFecha 25 10 2021)))
;(define prueba (login gDocs1 "user2" "pass2" "A"))
;(define prueba2 (create prueba "A" "a2" (list 1 2 2020)))
;(define prueba4 (share prueba3 10 (access "user11" #\r) (access "user20" #\w) (access "user50" #\c)
;(define prueba5 (share prueba4 5 (access "user11" #\w) (access "user20" #\c) (access "user100" #\c)))