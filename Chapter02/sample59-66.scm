(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set)))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1)
               (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;59
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s1)
                    (union-set (cdr s1) s2)))))

(define s1 '(1 2 3))
(define s2 '(2 3 4 5))
(intersection-set s1 s2)
(union-set s1 s2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;60
;; 这个实在有点无聊，就跳过吧
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2)) '()
      (let ((x1 (car s1))
            (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr s1) (cdr s2))))
              ((< x1 x2) (intersection-set (cdr s1) s2))
              ((> x2 x2) (intersection-set s1 (cdr s2)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;61
(define (adjoin-set x set)
  (cond ((null? set) '(x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (element-of-set? x (cdr set))))))

(adjoin-set 1 s2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (x2 (car s2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr s1) (cdr s2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr s1) s2)))
                 ((> x1 x2)
                  (cons x2 (union-set s1 (cdr s2)))))))))

(union-set s1 s2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define s1 '())
(define s1 (adjoin-set 3 s1))
(define s1 (adjoin-set 4 s1))
(define s1 (adjoin-set 2 s1))

(define s2 '())
(define s2 (adjoin-set 1 s2))
(define s2 (adjoin-set 2 s2))
(define s2 (adjoin-set 2 s2))
(define s2 (adjoin-set 3 s2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;63
;; tree->list-1 O(N^2)
;; tree->list-2 O(N)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;64\65
;; 合并O(N) 将结果转为列表O(N) 转为树O(N)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;66
;; 在make-tree类型末尾加个值即可，其它从element-of-set中搬来
;; 结果可以改成返回一个cadddr set
(define (lookup x set)
  (cond ((null? set) false)
        ((= x (entry set)) set)
        ((< x (entry set)) (lookup x (left-branch set)))
        ((> x (entry set)) (lookup x (right-branch set)))))

(lookup 3 s2)

