;; 求和函数，代入term(x)与next(x) x=a..b
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (cube-sum x n)
  (define (inc x) (+ x 1))
  (define (cube x) (* x x x))
  (sum cube x inc n))

;; 求pi的近似值
(define (pi-sum x n)
  (define (pi-next x) (+ x 4))
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (* 8 (sum pi-term x pi-next n)))


(define (cube x) (* x x x))

;; 取f函数在(a,b)上的积分
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;; 用simpson法求积分
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  
  (define (next k) (+ k 1))
  (define (term k)
    (*
     (cond ((or (= k 0) (= k n)) 1)
           ((odd? k) 4)
           (else 2))
     (y k)))
  (* (sum term (exact->inexact 0) next n) (/ h 3)))


;; 另写累积函数
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value
                            term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum cube 1 (lambda (x) (+ x 1)) 10)
