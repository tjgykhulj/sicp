;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define the-emtpy-stream '())
;; (define stream-null? null?)

;; (define (cons-stream a b) (cons a (delay b)))
;; (define (stream-car stream) (car stream))
;; (define (stream-cdr stream) (force (cdr stream)))

;; (define (memo-proc proc)
;;   (let ((already-run? false) (result false))
;;     (lambda ()
;;       (if (not already-run?)
;;           (begin (set! result (proc))
;;                  (set! already-run? true)
;;                  result)
;;           result))))

;; (define (delay exp) (memo-proc (lambda () exp)))
;; (define (force delayed-object) (delayed-object))

;; (define (stream-enumerate-interval l r)
;;   (if (> l r)
;;       the-empty-stream
;;       (cons-stream l (stream-enumerate-interval (+ l 1) r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST
(define one-to-ten (stream-enumerate-interval 1 10))
(define x1
  (stream-filter (lambda (x) (= (remainder x 3) 0)) one-to-ten))
x1
(stream-cdr x1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;50
(define (stream-map proc . args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (s) (stream-car s)) args))
       (apply stream-map
              (cons proc (map (lambda (s) (stream-cdr s)) args))))))

(define five-to-fourteen (stream-enumerate-interval 5 14))
(stream-map + one-to-ten five-to-fourteen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))

;; (define (stream-for-each proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-for-each proc (stream-cdr s)))))
                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;52
(define sum 0)
(define (accum x)
  (set! sum (+ sum x))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(stream-ref y 7)
(display-stream z)
(display-stream y)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;目前见过最装逼的筛法
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (> (remainder x (stream-car stream)) 0))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 5)

/;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;我的世界观已经被刷新了
(define (add-streams s1 s2) (stream-map + s1 s2))
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs) fibs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;优化的筛法
(define primes
  (cons-stream
   2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((= (remainder n (stream-car ps)) 0) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;53
(define s (cons-stream 1 (add-streams s s)))
;;应该是2的幂次

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

(stream-ref factorials 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;55
(define (partial-sums S)
  (define iter
    (cons-stream (car S) (add-streams iter (stream-cdr S))))
  iter)

(define x (partial-sums factorials))
(stream-ref x 3)
;; 1! + 2! + 3! + 4! = 33

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;56
;;没什么意思，就merge( xx2 merger(xx3 xx5))一下即可


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define x (expand 1 7 10))
(stream-ref x 3)
;;radix=10时返回1.0/7的结果（从个位到小数）

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;59
(define (-streams a) (stream-map - a))
(define (div-streams a b) (stream-map / a b))

(define one (cons-stream 1 one))
(define (integerate-series a)
  (define iter
    (mul-streams a (div-streams one integers)))
  iter)

(define x (integerate-series factorials))
(stream-ref x 3);;4!*1/4=6

(define exp-series
  (cons-stream 1 (integerate-series exp-series)))

(define sine-series
  (cons-stream 0 (integerate-series cosine-series)))
(define cosine-series
  (cons-stream 1 (integerate-series (-streams sine-series))))

(stream-ref sine-series 3)
(stream-ref cosine-series 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;60
(define (mul-series s1 s2)
  (cons-stream (*
        (stream-car s1)
        (stream-car s2))
           (stream-map + (scale-stream
                  (stream-cdr s2)
                  (stream-car s1))
               (mul-series
                (stream-cdr s1)
                s2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sqrt-stream x)
  (define (average a b) (/ (+ a b) 2))
  (define (sqrt-improve guess)
    (average guess (/ x guess)))
  (define guesses
    (cons-stream 1.0
                 (stream-map sqrt-improve guesses)))
  guesses)

(define x (sqrt-stream 5))
(stream-ref x 8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-ref pi-stream 8)
;3.2523659347188767

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;加速流
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define super-pi-stream (euler-transform pi-stream))
(stream-ref super-pi-stream 8)
;3.1418396189294033

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
(define super-super-pi-stream
  (accelerated-sequence euler-transform pi-stream))

(stream-ref super-super-pi-stream 8)
;3.1415926535897953

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;64
(define (sqrt x tolerance)
  (define (sqr x) (* x x))
  (define (stream-limit s tolerance)
    (if (< (abs (- (sqr (stream-car s)) x)) tolerance)
        (stream-car s)
        (stream-limit (stream-cdr s) tolerance)))

  (stream-limit (sqrt-stream x) tolerance))

(sqrt 100 0.001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ln2)
  (define (seq n)
    (cons-stream n (stream-map - (seq (+ n 1)))))
  (partial-sums (div-streams one (seq 1))))

(define super-super-ln2-stream
  (accelerated-sequence euler-transform (ln2)))

(- (stream-ref (ln2) 8) (log 2))
;;效果不是非常好的样子，只能精确到-2次方
(- (stream-ref super-super-ln2-stream 8) (log 2))
;;效果很好，已经接近到-15次方了

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;66
(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define x (pairs integers integers))

(define (work x i)
  (let ((ans (stream-car x)))
    (if (= (car ans) 16) i
        (work (stream-cdr x) (+ i 1)))))
(work x 0)
;;65534   (100,1)想必在2^100-1项

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define i -1)
(define (work)
  (set! i (+ i 1))
  (stream-ref x i))
(work)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;68
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (interleave
                 (stream-map (lambda (x) (list (stream-car t) x))
                             (stream-cdr u))
                 (pairs (stream-cdr t) (stream-cdr u))))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define x (triples integers integers integers))
(define i -1)
(work)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;70
(define (sum args)
  (if (null? args) 0
      (+ (car args) (sum (cdr args)))))

(define (interleave s1 s2)
  (let ((as1 (stream-car s1))
        (as2 (stream-car s2)))
    (if (< (sum as1) (sum as2))
        (cons-stream as1 (interleave (stream-cdr s1) s2))
        (cons-stream as2 (interleave s1 (stream-cdr s2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;71
(define (weight x)
  (define (cube x) (* x x x))
  (+ (cube (car x)) (cube (cadr x))))

(define (interleave s1 s2)
  (let ((as1 (stream-car s1))
        (as2 (stream-car s2)))
    (if (< (weight as1) (weight as2))
        (cons-stream as1 (interleave (stream-cdr s1) s2))
        (cons-stream as2 (interleave s1 (stream-cdr s2))))))

(define x (pairs integers integers))
(define (Ramanujan pre x n)
  (if (= n 0) pre
      (let ((value (weight (stream-car x))))
        (if (= value pre)
            (Ramanujan value (stream-cdr x) (- n 1))
            (Ramanujan value (stream-cdr x) n)))))

(Ramanujan 0 x 1) ;1729
(Ramanujan 0 x 2) ;4104
(Ramanujan 0 x 3) ;13832
(Ramanujan 0 x 4) ;20683
(Ramanujan 0 x 5) ;32832

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC r c dt)
  (define (proc i v)
    (add-streams (scale-stream i r)
                 (integral (scale-stream i (/ 1 c)) v dt)))
  proc)

(define RC1 (RC 5 1 0.5))
(define x (RC1 one 3))
(stream-ref x 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;77
(define (integral delay-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delay-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand))
                      initial-value)
                   dt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;78
(define (solve-2nd a b dt y0 dy0) 
  (define y (integral (delay dy) y0 dt)) 
  (define dy (integral (delay ddy) dy0 dt)) 
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b))) 
  y)

(define x (solve-2nd 1 1 0.001 1 2))
(stream-ref x 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;80
(define (RLC R L C dt) 
  (define (rcl vc0 il0) 
    (define vc (integral (delay dvc) vc0 dt)) 
    (define il (integral (delay dil) il0 dt)) 
    (define dvc (scale-stream il (- (/ 1 C)))) 
    (define dil (add-streams (scale-stream vc (/ 1 L)) 
                             (scale-stream il (- (/ R L))))) 
    (define (merge-stream s1 s2) 
      (cons-stream (cons (stream-car s1) (stream-car s2)) 
                   (merge-stream (stream-cdr s1) (stream-cdr s2)))) 
    (merge-stream vc il)) 
  rcl) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))

(define a (stream-withdraw 10000 integers))
(stream-ref a 2)
                       
