#lang racket
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (for-each proc items)
  (cond ((not (null? items)) (proc (car items))))
  (cond ((not (null? items)) (for-each proc (cdr items))))
  true)
;; frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))


(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (car (cdr f)))
(define (edge2-frame f)
  (car (cdr (cdr f))))

;; segment
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;; vector
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect n ve)
  (make-vect (* (xcor-vect ve) n) (* (ycor-vect ve) n)))

;; a
(define (frame-coord-map frame)
  (lambda (vect)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect vect)
                           (edge1-frame frame))
               (scale-vect (ycor-vect vect)
                           (edge2-frame frame))))))

(define (vector-to-posn v)
  (make-posn (* 500 (car v)) (* 500 (cdr v))))

(define (segments->painter segment-list)   
  (lambda (frame)     
    (for-each     
     (lambda (segment)
       (line         
        (vector-to-posn ((frame-coord-map frame) (start-segment segment)))         
        (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))      
      segment-list)))

(define frame1 (make-frame (make-vect 0 0) (make-vect 0 1) (make-vect 1 0)))

(define segment-list-d (list (make-segment (make-vect .25 0) (make-vect .35 .5))
                             (make-segment (make-vect .35 .5) (make-vect .3 .6))
                             (make-segment (make-vect .3 .6) (make-vect .15 .4))
                             (make-segment (make-vect .15 .4) (make-vect 0 .65))
                             (make-segment (make-vect 0 .65) (make-vect 0 .85))
                             (make-segment (make-vect 0 .85) (make-vect .15 .6))
                             (make-segment (make-vect .15 .6) (make-vect .3 .65))
                             (make-segment (make-vect .3 .65) (make-vect .4 .65))
                             (make-segment (make-vect .4 .65) (make-vect .35 .85))
                             (make-segment (make-vect .35 .85) (make-vect .4 1))
                             (make-segment (make-vect .4 1) (make-vect .6 1))
                             (make-segment (make-vect .6 1) (make-vect .65 .85))
                             (make-segment (make-vect .65 .85) (make-vect .6 .65))
                             (make-segment (make-vect .6 .65) (make-vect .75 .65))
                             (make-segment (make-vect .75 .65) (make-vect 1 .35))
                             (make-segment (make-vect 1 .35) (make-vect 1 .15))
                             (make-segment (make-vect 1 .15) (make-vect .6 .45))
                             (make-segment (make-vect .6 .45) (make-vect .75 0))
                             (make-segment (make-vect .75 0) (make-vect .6 0))
                             (make-segment (make-vect .6 0) (make-vect .5 .3))
                             (make-segment (make-vect .5 .3) (make-vect .4 0))
                             (make-segment (make-vect .4 0) (make-vect .25 0))

                             (make-segment (make-vect .5 .75) (make-vect .5 .77))
                             (make-segment (make-vect .45 .75) (make-vect .55 .75))
                             
                             ))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))


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

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
   (let ((combine4 (square-of-four flip-vert rotate180
                                   identity flip-horiz)))
     (combine4 (corner-split painter n))))


(define (split fs ss)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split fs ss) painter (- n 1))))
          (fs painter (ss smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))




(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 .0)
                     (make-vect .0 .0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) (f ((repeated f (- n 1)) x)))))
(define (rotate270 painter)
  ((repeated rotate90 3) painter))

(define (rotate180 painter)
  ((repeated rotate90 2) painter))

((square-limit (segments->painter segment-list-d) 1) frame1)
;; ((segments->painter segment-list-d) frame1)
