#lang plai

(require "practica3-base.rkt")

;Ejercicio 1 zones
(define (zones min max)
  (list (resting min (calc-rest-max min max))
        (warm-up (calc-min 0 min max) (calc-max 0 min max))
        (fat-burning (calc-min 1 min max) (calc-max 1 min max))
        (aerobic (calc-min 2 min max) (calc-max 2 min max))
        (anaerobic (calc-min 3 min max) (calc-max 3 min max))
        (maximum (calc-min 4 min max) (calc-max 4 min max))))

(define (range min max)
  (- max min))

(define (calc-rest-max min max)
  (- (* 0.5 (range min max)) 1))

(define (calc-min i min max)
  (+ min (* (range min max) (+ 0.5 (* i 0.1)))))

(define (calc-max i min max)
  (+ min (- (* (range min max) (+ 0.5 (* (+ 1 i) 0.1))) 1)))

(test (zones 50 180) (list (resting 50 64.0) (warm-up 115.0 127.0) (fat-burning 128.0 140.0) (aerobic 141.0 153.0) (anaerobic 154.0 166.0) (maximum 167.0 179.0)))
(test (zones 40 190) (list (resting 40 74.0) (warm-up 115.0 129.0) (fat-burning 130.0 144.0) (aerobic 145.0 159.0) (anaerobic 160.0 174.0) (maximum 175.0 189.0)))
(test (zones 50 180) (list (resting 50 64.0) (warm-up 115.0 127.0) (fat-burning 128.0 140.0) (aerobic 141.0 153.0) (anaerobic 154.0 166.0) (maximum 167.0 180)))
(test (zones 40 190) (list (resting 40 74.0) (warm-up 115.0 129.0) (fat-burning 130.0 144.0) (aerobic 145.0 159.0) (anaerobic 160.0 174.0) (maximum 175.0 190)))
(test (zones 40 180) (list (resting 40 69.0) (warm-up 110.0 123.0) (fat-burning 124.0 137.0) (aerobic 138.0 151.0) (anaerobic 152.0 165.0) (maximum 166.0 179.0)))

;Ejercicio 2 get-zone
(define my-zones (zones 50 180))

(define (get-zone nombre zones)
  (cond 
    [(symbol? nombre) 
     (case nombre
       ['resting (busca-resting zones)]
       ['warm-up (busca-warm zones)]
       ['fat-burning (busca-burning zones)]
       ['aerobic (busca-aerobic zones)]
       ['anaerobic (busca-anaerobic zones)]
       ['maximum (busca-maximum zones)])]
    [else (error 'get-zones "El nombre debe ser un simbolo")]))

(define (busca-resting zones)
  (cond
    [(empty? zones) '()]
    [else 
     (type-case HRZ (car zones)
       [resting (min max) (car zones)]
       [else (busca-resting (cdr zones))])]))

(define (busca-warm zones)
  (cond
    [(empty? zones) '()]
    [else 
     (type-case HRZ (car zones)
       [warm-up (min max) (car zones)]
       [else (busca-warm (cdr zones))])]))

(define (busca-burning zones)
  (cond
    [(empty? zones) '()]
    [else 
     (type-case HRZ (car zones)
       [fat-burning (min max) (car zones)]
       [else (busca-burning (cdr zones))])]))

(define (busca-aerobic zones)
  (cond
    [(empty? zones) '()]
    [else 
     (type-case HRZ (car zones)
       [aerobic (min max) (car zones)]
       [else (busca-aerobic (cdr zones))])]))

(define (busca-anaerobic zones)
  (cond
    [(empty? zones) '()]
    [else 
     (type-case HRZ (car zones)
       [anaerobic (min max) (car zones)]
       [else (busca-anaerobic (cdr zones))])]))

(define (busca-maximum zones)
  (cond
    [(empty? zones) '()]
    [else 
     (type-case HRZ (car zones)
       [maximum (min max) (car zones)]
       [else (busca-maximum (cdr zones))])]))

(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'maximum my-zones) (aerobic 141.0 153.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180))
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))

;Ejercicio 3 bpm->zone
(define (bpm->zone lista zones)
  (cond
    [(empty? lista) '()]))

;Ejercicio 9 ninBT
(define (ninBT arbol)
  (type-case BTree arbol
    [EmptyBT() 0 ]
    [BNode(fun li elem ld) 
          (cond
            [(and (not(EmptyBT? li)) (not(EmptyBT? ld))) (+ 1 (ninBT li) (ninBT ld))]
            [(and (not(EmptyBT? li)) (EmptyBT? ld)) (+ 1 (ninBT li))]
            [(and (EmptyBT? li) (not (EmptyBT? ld)) (+ 1 (ninBT ld)))]
            [(and (EmptyBT? li) (EmptyBT? ld)) 0])]))

(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 0)
(test (ninBT (EmptyBT)) 0)
(test (ninBT (EmptyBT)) 3)

;Ejercicio 10 nlBT
(define (nlBT arbol)
  (type-case BTree arbol
    [EmptyBT() 0]
    [BNode(func li elem ld) (cond 
                        [(and (not (EmptyBT? li)) (not (EmptyBT? ld))) (+  (nlBT li) (nlBT ld))]
                        [(and  (EmptyBT? li) (EmptyBT? ld)) (+ 1 (nlBT li) (nlBT ld))]
                        )]))

(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (nlBT (BNode < (EmptyBT) 2 (EmptyBT))) 1)
(test (nlBT (BNode < (EmptyBT) 2 (EmptyBT))) 2)
(test (nlBT (EmptyBT)) 0)

;Ejercicio 11 nnBT
(define (nnBT arbol)
  (+ (ninBT arbol) (nlBT arbol)))

(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (nnBT (BNode < (EmptyBT) 1 (EmptyBT))) 3)
(test (nnBT (BNode < (EmptyBT) 1 (EmptyBT))) 1)
(test (nnBT (EmptyBT)) 0)

;Ejercicio 12 mapBT
(define (mapBT fun arbol) 
  (type-case BTree arbol 
    [EmptyBT() (EmptyBT)] 
    [BNode (func li elem ld) 
           (if (procedure? fun) 
               (BNode func (mapBT fun li) (fun elem) (mapBT fun ld)) 
               (error 'fun "no es una funcion"))]))

(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT))))
(test (mapBT add1 (EmptyBT)) (EmptyBT))

;Ejercicio 13
;(define w 2)
(define (preorderBT arbol)
  (type-case BTree arbol
    [EmptyBT() '()]
    [BNode (func li elem ld) (cons elem (append (preorderBT li) (preorderBT ld)))]))
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (preorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(test (preorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))

(define (inorderBT arbol)
  (type-case BTree arbol
    [EmptyBT() '()]
    [BNode (func li elem ld) (append (inorderBT li) (cons elem (inorderBT ld)))]))
(test (inorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(test (inorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (inorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))

(define (postorderBT arbol)
  (type-case BTree arbol
    [EmptyBT() '()]
    [BNode (func li elem ld) (append (postorderBT li) (append (postorderBT ld) (cons elem '())))]))
(test (postorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
(test (postorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (postorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))