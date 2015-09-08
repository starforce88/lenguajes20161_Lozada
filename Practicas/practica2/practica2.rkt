#lang plai
;Ejercicio 1
(define-type Array
  [MArray (n number?) (lista list?)])

;Ejercicio 2
(define-type MList
  [MEmpty]
  [MCons (n any/c) (l MList?)])

;Ejercicio 3
(define-type NTree
  [TLEmpty]
  [NodeN (n number?) (lista (listof NTree?))])

;Ejercicio 4
(define-type Position
  [2D-Point (x number?) (y number?)])

;Ejercicio 5
(define-type Figure
  [Circle (c Position?) (r number?)]
  [Square (e Position?) (l number?)]
  [Rectangle (e Position?) (a number?) (l number?)])

;Ejercicio 6
(define (reemplaza lista num val)
  (if(= 0 num)
     (cons val (cdr lista))
     (cons (car lista) (reemplaza (cdr lista) (- num 1) val))))

(define (setvalueA arreglo pos val)
  (type-case Array arreglo
    [MArray (n lista)
              (if(or (= pos (length lista)) (> pos (length lista)))
                 (error 'setvalueA "Posicion invalida")
                 (reemplaza lista pos val))]))

;Ejercicio 7
(define (MArray2MList arreglo)
  (type-case Array arreglo
    [MArray (n lista)
            (if (empty? lista)
                (MEmpty)
                (MCons (car lista) (MArray2MList (MArray n (cdr lista)))))]))

;Ejercicio 8
;Ejercicio 9
;Ejercicio 10
;Ejercicio 11
;Ejercicio 12
;Ejercicio 13
(define-type Coordinates
  [GPS (lat number?) (long number?)])

(define-type Location
  [building (name string?) (loc GPS?)])

;; Coordenadas GPS
(define gps-satelite (GPS 19.510482 -99.23411900000002))
(define gps-ciencias (GPS 19.3239411016 -99.179806709))
(define gps-zocalo (GPS 19.432721893261117 -99.13332939147949))
(define gps-perisur (GPS 19.304135 -99.19001000000003))
(define plaza-satelite (building "Plaza Satelite" gps-satelite))
(define ciencias (building "Facultad de Ciencias" gps-ciencias))
(define zocalo (building "Zocalo" gps-zocalo))
(define plaza-perisur (building "Plaza Perisur" gps-perisur))
(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))

(define (haversine coord1 coord2)
  (if (Coordinates? coord1)
      (if (Coordinates? coord2)
          )))