#lang plai

;Ejercicio 1
(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

;test pow
(test (pow 2000 0) 1)
(test (pow 2 3) 8)
(test (pow 4 4) 1)
(test (pow 4 4) 256)
(test (pow 2000 2) 9)

;Ejercicio 2
(define (average lista)
  (/ (suma-lista lista 0) (length lista)))

(define (suma-lista lista total)
  (if (empty? lista)
      total
      (suma-lista (cdr lista) (+ (car lista) total))))

;test average
(test (average '(5)) 5)
(test (average '(1 2 3 4 5 6 7 8 9 10)) 5.5)
(test (average '(1 2 3 4)) 5)
(test (average '(1 2 3)) 2)
(test (average '(1)) 1)

;Ejercicio 3
(define (primes n)
  (invierte (primes-aux (lista-hasta n 1) '()) '()))

(define (invierte lista acc)
  (if (empty? lista)
      acc
      (invierte (cdr lista) (cons (car lista) acc))))

(define (primes-aux lista acc)
  (if (empty? lista)
      acc
      (if (= 2 (length (divs (car lista))))
          (primes-aux (cdr lista) (cons (car lista) acc))
          (primes-aux (cdr lista) acc))))

(define (lista-hasta x y)
  (if(< x y)
     '()
     (cons y (lista-hasta x (+ 1 y)))))

(define (filtra-divs x lista)
  (if (empty? lista)
      '()
      (if(= (modulo x (car lista)) 0)
         (cons (car lista) (filtra-divs x (cdr lista)))
         (filtra-divs x (cdr lista)))))

(define (divs n)
  (filtra-divs n (lista-hasta n 1)))

;test primes

(test (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(test (primes 11) '(2 3 5 7 11))
(test (primes 1) '())
(test (primes 10) '())
(test (primes 25) '(2 3 5 7 11 13 17 19))

;Ejercicio 4
(define (zip lista1 lista2)
  (if(empty? lista1)
     '()
     (if (empty? lista2)
         '()
         (cons (list (car lista1) (car lista2)) 
               (zip (cdr lista1) (cdr lista2))))))

;test zip
(test  (zip '(1 2) '(3 4)) '((1 3) (2 4)))
(test  (zip '(1 2) '()) '((1 3) (2 4)))
(test  (zip '() '(3 4)) '((1 3) (2 4)))
(test  (zip '(1 2) '()) '())
(test  (zip '(1 2) '(3 4 5)) '((1 3) (2 4)))

;Ejercicio 5
(define (reduce proc lista)
  (if(procedure? proc)
     (if (empty? (cdr lista))
         (car lista)
         (proc (car lista) (reduce proc (cdr lista))))
     (error 'reduce "No es una funcion")))

;test reduce
(test (reduce + '(1 2 3 4 5 6 7 8 9 10)) 55)
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9))) '((1 (4 7)) (2 (5 8)) (3 (6 9))))

;Ejercicio 6
(define (mconcat lista1 lista2)
  (if(empty? lista1)
     (if(empty? lista2)
        '()
        (cons (car lista2) (mconcat '() (cdr lista2))))
     (cons (car lista1) (mconcat (cdr lista1) lista2))))

;Ejercicio 7
(define (mmap proc lista)
  (if(procedure? proc)
     (if(empty? lista)
        '()
        (cons (proc (car lista)) (mmap proc (cdr lista))))
     (error 'mmap "No es una funcion")))

;Ejercicio 8
(define (mfilter pred lista)
  (if(procedure? pred)
     (if(empty? lista)
        '()
        (if(pred (car lista))
                 (cons (car lista) (mfilter pred (cdr lista)))
                 (mfilter pred (cdr lista))))
     (error 'mfilter "No es un predicado")))

;Ejercicio 9
(define (any? pred lista)
  (if(procedure? pred)
     (if(empty? lista)
        #f
        (or (pred (car lista)) (any? pred (cdr lista))))
     (error 'any? "No es un predicado")))

;Ejercicio 10
(define (every? pred lista)
  (if(procedure? pred)
     (if(empty? lista)
        #t
        (and (pred (car lista)) (every? pred (cdr lista))))
     (error 'every? "No es un predicado")))

;Ejercicio 11
(define (mpowerset lista)
  (if(empty? lista)
     (list lista)
     (mconcat (mmap (lambda(l)
                      (cons (car lista) l))
                    (mpowerset (cdr lista)))
              (mpowerset (cdr lista)))))

