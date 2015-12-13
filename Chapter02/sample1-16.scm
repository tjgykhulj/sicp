;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
(define (make-rat n d)
  (if (< d 0)
      (cons (-n) (-d))
      (cons n d)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;; point : x y
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
;; segment : start end
(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)  (/ (+ a b) 2))

(define (mid-point seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))
;; test
(define start (make-point 1 3))
(define end (make-point 3 2))
(define seg (make-segment start end))
(print-point (mid-point seg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Ex3 looks like Ex2, so I skip it.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4
(define (cons x y)
  (lambda (m) (m x y)))

(define test (cons 1 2))

(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
(car test) ;; result = 1
(cdr test) ;; result = 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))

(define test (cons 3 2))
(car test)
(cdr test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define inc2 (lambda (x) (+ x 2)))
(((add-1 zero) inc2) 1)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
((zero inc2) 1)
((one inc2) 1)
((two inc2) 1)
