#lang slideshow
(require (planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

;;上下、左右转
(paint (flip-vert einstein))
(paint (flip-horiz einstein))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((small (right-split painter (- n 1))))
        (beside painter (below small small)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((small (up-split painter (- n 1))))
        (below painter (beside small small)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(paint (corner-split einstein 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((small ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 small small))))))
(define r-split (split beside below))
(define u-split (split below beside))
(paint (r-split einstein 2))
(paint (u-split einstein 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;46
