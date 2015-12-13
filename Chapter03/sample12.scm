;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (cons 1 2))
(define y (list 3 4))
(set-car! x y)
(set-cdr! x 0)
x


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y '(c d))
(define z (append x y))
z
(define w (append! x y))
w
(cdr x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle '(a b c)))
;; z is a cycle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define x '(a b c 10))
(mystery x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;16
(define x '(1 2 (3 4)))

(define (count-pairs x)
  (if (not (pair? x)) 0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;17
(define (count-pairs x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x)
             (false? (memq x memo-list)))
        (inner (car x)
               (inner (cdr x)
                      (cons x memo-list)))
        memo-list))
(inner x '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;18\19
(define (judge-cycle x)
  (define (iter x init)
    (cond ((null? x) #f)
          ((eq? (car x) init) #t)
          (else (iter (cdr x) init))))
  (iter (cdr x) (car x)))

;TEST
(define x '(3 4))
(set-cdr! (last-pair x) x)
(judge-cycle x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
  (memq (car x) (cdr x)))


(set-cdr! (last-pair x) x)
(judge-cycle x)
