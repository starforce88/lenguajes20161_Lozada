#lang plai
;Ejercicio 1
(define-type Array
  [MArray (n number?) (lista (listof number?))])

;Ejercicio 2
(define-type MList
  [MEmpty]
  [MCons (n number?) (l MList?)])

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