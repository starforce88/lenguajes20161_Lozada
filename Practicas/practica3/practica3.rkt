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