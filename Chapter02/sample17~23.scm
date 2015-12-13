(define test (list 1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;17
(define (last-pair lists)
  (let ((next (cdr lists)))
    (if (null? next)
        (car lists)
        (last-pair next))))

(last-pair test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;18
(define (reverse lists)
  (define (iter lists result)
    (if (null? lists)
        result
        (iter (cdr lists) (cons (car lists) result))))
  (iter lists '()))

(reverse test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;19
(define (first-denomination list) (car list))
(define (except-first-denomination list) (cdr list))
(define (no-more? list) (null? list))
  
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 (list 100 50 20 10 5 2 1 0.5))
;; pass. result = 104561
(cc 100 (list 50 25 10 5 1))
;; pass. result = 292

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;20
(define (same-parity . lst)
  (filter (if (even? (car lst))
              even?
              odd?)
          lst))

(same-parity 1 2 3 4 5)
(same-parity 3 3 2 2 1 1 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;21
;; it's helpful to do sth. to all of list: map abs (list -3 4 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(square-list (list 1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23
(for-each (lambda(x) (newline) (display x))
          (list 1 2 3 4))

(define (foreach p lst)
  (cond ((not (null? lst))
        (p (car lst))
        (foreach p (cdr lst)))))

(foreach (lambda(x) (newline) (display x))
         (list 1 2 3 4))
