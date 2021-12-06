#lang racket

(require  "TDAFecha.rkt")
(require  "TDAAccess.rkt")
(require  "TDAUsuario.rkt")
(require  "TDAParadigmadocs.rkt")
(require  "TDADocumentos.rkt")
;---------------------- Requisitos Funcionales ------------------------------


;FUNCION REGISTER
;Funcion que me registra un nuevo usuario, en el caso de que este usuario ya exista se retorna la misma plataforma sin modificar
;Dom: plataforma x usuario(string) x contraseña(string)
;Rec: plataforma actualizado
;Recursion natural

(define (register plataforma usuario contraseña fecha)
  (if (equal? (estaUsuario? (getLista-usuarios plataforma) usuario) #f)
      (setLista-usuarios plataforma (agregarUsuario (getLista-usuarios plataforma) (crearLista-usuario usuario contraseña 0 fecha)))
      plataforma
      )
  )

;FUNCION LOGIN
;Funcion que cambia el estado de actividad del usuario, en otras palabras logeada
;Dom: paraddigmadocs (plataforma) X usuario (string) X contraseña (string)
;Rec: funcion

(define (login plataforma usuario contraseña operacion)
  (if (eq? (estaLogeado? (getLista-usuarios plataforma) usuario contraseña) #t)
      (cond
        [(eq? operacion create) (create (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [(eq? operacion share) (share (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [(eq? operacion add) (add (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [(eq? operacion restoreVersion) (restoreVersion (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [(eq? operacion revokeAllAccess) (revokeAllAccess (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [(eq? operacion search) (search (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [(eq? operacion paradigmadocs->string) (paradigmadocs->string (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) usuario 1)))]
        [else plataforma]
        )
      operacion
      )
  )

;FUNCION CREATE

;Funcion que me crea un documento que esta asociado al nombre del usuario
;Dom: paradigmadocs X fecha X nombre documento (string) X contenido documento (string)
;Rec: paradigmadocs modificado

(define (create plataforma)
  (lambda (nombreDoc contenido fecha)
    (if (and (string? nombreDoc)
             (string? contenido)
             (esfecha? fecha)
             )
        (setLista-documentos (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
                             (agregarDocumento (getLista-documentos plataforma) (crearDoc (usuarioLogeado (getLista-usuarios plataforma)) (asignarIdDoc (getLista-documentos plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0) nombreDoc ((getEncryp plataforma) contenido) fecha)))
        plataforma
        )
    )
  )

;FUNCION SHARE
;Funcion que me deja comaprtir documentos con otro usuario
;Dom: paradigmadocs X integer X list
;Rec: paradigmadocs modificado
(define (share plataforma )
  (lambda ( idDoc access . accessess)
    (setLista-access (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
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
  )

;FUNCION ADD
;Funcion que me deja añadir nuevo contenido a un documento ya existente
;Dom: paradigmadocs X integer X list X string
;Rec: paradigmadocs modificado
(define (add plataforma)
  (lambda (idDoc fecha contenidoTexto)
    (if (eq? (existeDoc? (getLista-documentos plataforma) (usuarioLogeado (getLista-usuarios plataforma)) idDoc) #t)
        (setLista-documentos (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
                             (nuevaLista-doc (getLista-documentos plataforma)
                                             (agregarNueva-version (getLista-documentos plataforma)
                                                                   (usuarioLogeado (getLista-usuarios plataforma))
                                                                   idDoc
                                                                   contenidoTexto
                                                                   fecha
                                                                   )
                                             )
                             )
        (if (tienePermiso? (usuarioLogeado (getLista-usuarios plataforma)) #\w idDoc (getLista-access plataforma))
            (setLista-documentos (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
                                 (nuevaLista-doc (getLista-documentos plataforma)
                                                 (agregarNueva-version (getLista-documentos plataforma)
                                                                       (userPertenencia (getLista-compartidos (listaAccesos-especifico (getLista-access plataforma) (usuarioLogeado (getLista-usuarios plataforma)))) idDoc)
                                                                       idDoc
                                                                       contenidoTexto
                                                                       fecha
                                                                       )
                                                 )
                                 )
            (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
            )
        )
    )
  )


;FUNCION RESTORE VERSION
;Funcion que me permite restaurar una version antigua de un documento
;Dom: paradigmadocs X integer X integer
;Rec: paradigmadocs modificado
(define (restoreVersion plataforma)
  (lambda (IdDoc IdVersion)
    (setLista-documentos (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
                         (nuevaLista-doc (getLista-documentos plataforma)
                                         (agregarVersion-anterior (getLista-documentos plataforma)
                                                                  (usuarioLogeado (getLista-usuarios plataforma))
                                                                  IdDoc
                                                                  IdVersion
                                                                  )
                                         )
                         )
    )
  )
;FUNCION REVOKE ALL ACCESS
;Funcion que revoca todos los accesos que un usuario a dado a sus documentos
;Dom: paradigmadocs
;Rec: paradigmadocs modificado
(define (revokeAllAccess plataforma)
  (setLista-access (setLista-usuarios plataforma (nuevaLista-usuarios (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)) 0))
                   (nuevaLista-accesos (getLista-access plataforma)
                                       (accesosRevocados (getLista-access plataforma)
                                                         (usuarioLogeado (getLista-usuarios plataforma))
                                                         (list)
                                                         )
                                       )
                   )
  )

;FUNCION SEARCH
;Funcion que retorna todos los documentos que tienen cierta frase o palabra
;Dom: paradigmadocs X string
;Rec: list
(define (search plataforma)
  (lambda (searchText)
    (buscarFrase (obtenerDocs (getLista-documentos plataforma)
                              (getLista-documentos plataforma)
                              (if (getLista-compartidos (listaAccesos-especifico (getLista-access plataforma) (usuarioLogeado (getLista-usuarios plataforma))))
                                  (getLista-compartidos (listaAccesos-especifico (getLista-access plataforma) (usuarioLogeado (getLista-usuarios plataforma))))
                                  (list)
                                  )
                              (usuarioLogeado (getLista-usuarios plataforma))
                              (list)
                              )
                 searchText
                 (list)
                 )
    )
  )

;FUNCION PARADIGMADOCS->STRING
;Funcion que retorna un string con todo el contenido que un usuario tiene en paradigmadocs
;Dom: paradigmadocs
;Rec: string
(define (paradigmadocs->string plataforma)
  (if (eq? (alguienLogeado? (getLista-usuarios plataforma)) #t)
      (string-append
       (Usuario->String (getLista-usuarios plataforma) (usuarioLogeado (getLista-usuarios plataforma)))
                
       (Documentos->String (getLista-documentos plataforma) (usuarioLogeado (getLista-usuarios plataforma)) " ") "\n"
       
       (Access->String (getLista-access plataforma) (usuarioLogeado (getLista-usuarios plataforma))) "\n"
       )       
      (string plataforma (getLista-usuarios plataforma) " ")
      )
  )
 
;FUNCIONES EXTRAS

(define (string plataforma listaUsuarios stringPlataforma)
  (if (null? listaUsuarios)
      stringPlataforma
      (string plataforma (cdr listaUsuarios) (string-append stringPlataforma "\n" "\n"
                                                            (Usuario->String listaUsuarios (getUsuario (car listaUsuarios)))
                                                            (Documentos->String (getLista-documentos plataforma) (getUsuario (car listaUsuarios)) " ")
                                                            (Access->String (getLista-access plataforma) (getUsuario (car listaUsuarios)))
                                                            )
              )
      )
  )

;Ejemplos:

;------------- Ejemplos creacion de plataforma -----------------------
(define docsColavorativos (crearPlataforma "Google Docs" (crearFecha 25 10 2021) encryptFn encryptFn))
(define wordOnline (crearPlataforma "microsoft word" (crearFecha 29 11 2021) encryptFn encryptFn))
;(define collabTarreo (crearPlataforma Mi_plataforma (crearFecha 25 10 2021) encryptFn encryptFn)) ;-> ejemplo no valido

;------------- Ejemplos de register ----------------------------------
(define gDocs (register docsColavorativos "s1mple" "navi" (crearFecha 28 10 2021)))
(define Word (register (register wordOnline "zywoo" "vitality" (crearFecha 28 10 2021)) "nik0" "g2" (crearFecha 25 11 2021)))
;(define Word (register (register wordOnline "zywoo" "vitality" (crearFecha 28 10 2021)) "electronic" "navi" (crearFecha 25 1122 2021))) ;-> ejemplo no valido

;Tener en consideracion que para testear el requerimiento login tiene que utilizarse con otra operacion
;------------- Ejemplos de login y create ----------------------------
(define gDocs1 ((login gDocs "s1mple" "navi" create) "blast premier" "s1mple GOAT" (crearFecha 22 11 2021)))
(define Word1 ((login Word "nik0" "g2" create) "major final" "deagle shoot blast" (crearFecha 25 10 2021)))
(define Word2 ((login Word1 "zywoo" "vitality" create) "zywoo 1v9 " "blast premier final" (crearFecha 29 10 2021)))
;(define Word1 ((login Word "zywoo" "vitaliiity" create) "blast premier final" " zywoo 1 v 9" (crearFecha 29 10 2021))) ;-> ejemplo no valido

;------------- Ejemplos de login y share -----------------------------
(define Word3 ((login Word1 "nik0" "g2" share) 1 (access "zywoo" #\w)))
(define Word4 ((login Word2 "zywoo" "vitality" share) 1 (access "nik0" #\w)))
;(define Word5 ((login Word2 "zywoo" "vitality" share) 1 (access "nik0" w))) ;-> ejemplo no valido

;------------- Ejemplos de login y add -------------------------------
(define gDocs2 ((login gDocs1 "s1mple" "navi" add) 1 (crearFecha 23 11 2021) "navination"))
(define Word6 ((login Word3 "zywoo" "vitality" add) 1 (crearFecha 23 11 2021) "editando doc de otro usuario"))
;(define gDocs3 ((login gDocs1 "s1mple" "navi" add) 2 (crearFecha 23 11 2021) "editar un archivo que no existe no funciona")) ;-> ejemplo no valido

;------------- Ejemplos de login y restore version -------------------
(define gDocs3 ((login gDocs2 "s1mple" "navi" restoreVersion) 1 0))
(define Word7 ((login Word6 "nik0" "g2" restoreVersion) 1 0))
;(define gDocs4 ((login gDocs2 "s1mple" "navi" restoreVersion) 1 1)) ;-> ejemplo no valido, no existe la version a restaurar

;------------- Ejemplos de login y revoke all access -----------------
(define Word8 (login Word3 "nik0" "g2" revokeAllAccess))
(define Word9 (login Word4 "zywoo" "vitality" revokeAllAccess))
;(define gDocs5 (login gDocs3 "s1mple" "navi" revokeAllAccess))

;------------- Ejemplos de login y search ----------------------------
(define gDocs6 ((login gDocs2 "s1mple" "navi" search) "navi"))
(define Word10 ((login Word4 "nik0" "g2" search) "blast"))
;(define gDocs7 ((login gDocs2 "s1mple" "navi" search) navi));->ejemplo no valido

;------------- Ejemplos de login y paradigmadocs -> string ----------------------------
(define Word11 (login Word4 "nik0" "g2" paradigmadocs->string))
(define gDocs7 (paradigmadocs->string gDocs2))
(define Word12 (paradigmadocs->string Word4))
