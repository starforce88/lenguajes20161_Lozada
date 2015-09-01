#lang plai

;Ejercicio 1
(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

;Ejercicio 2
(define (average lista)
  (/ (suma-lista lista 0) (length lista)))

(define (suma-lista lista total)
  (if (empty? lista)
      total
      (suma-lista (cdr lista) (+ (car lista) total))))

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

;Ejercicio 4
(define (zip lista1 lista2)
  (if(empty? lista1)
     '()
     (if (empty? lista2)
         '()
         (cons (list (car lista1) (car lista2)) 
               (zip (cdr lista1) (cdr lista2))))))

;Ejercicio 5
(define (reduce proc lista)
  (if(procedure? proc)
     (if (empty? (cdr lista))
         (car lista)
         (proc (car lista) (reduce proc (cdr lista))))
     (error 'reduce "No es una funcion")))

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