#lang slideshow
;  global and local subroutines with greater and lesser abstractions

(define (square n)
    (filled-rectangle n n))
  
(define (colorsquare size color)
    (colorize (filled-rectangle size size) color))

(define (red-square size) (colorsquare size "red"))

(define (checker p1 p2)
    (let ([p12 (hc-append p1 p2)]
          [p21 (hc-append p2 p1)])
      (vc-append p12 p21)))

(define (four p) (checker p p))

(define (checkerboard p)
    (let* ([rp (colorsquare p "red")]
           [bp (colorsquare p "black")]
           [c (checker rp bp)]
           [c4 (checker c c)])
      (four c4)))


(define (cb size)
  (let* ([s (lambda(color) (colorize (filled-rectangle size size) color))]
         [f (lambda(x) (vc-append (hc-append x x) (hc-append x x)))]
         [rs (s "red")]
         [bs (s "black")]
         [c (vc-append (hc-append rs bs) (hc-append bs rs))])
    (f (f c))))

;(checkerboard 10)

;(cb 20)
