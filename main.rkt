#! /usr/bin/env racket
#lang racket

;; Using an L-system we create an image of a plant

(require racket/draw)

(define width 600)
(define height 500)
(define target (make-bitmap width height))
(define dc     (new bitmap-dc% [bitmap target]))

(define depth   6)             ; How many times the rewrite rules are applied
(define A       '(F + - S G))                                        ; The set of symbols
(define omega   '(F))                                                ; The start symbol
(define rewrite (hash 'F '(F S + F G F S - F S + F G S - F G G F)))  ; The rewrite rules
; (define rewrite (hash 'F '(F S + F S + F G - F G S - F G F)))      ; The rewrite rules of a different, more bushy plant

;; Generate all draw rules
(define draw-rules
  (for/fold ([symbols omega])
            ([i       depth])
    (flatten
      (map (lambda (x)
             (define rw (hash-ref rewrite x #f))
             (if rw
               rw
               x))
             symbols))))

;; Turns rules into lines
(define (draw-plant rules [o (* pi (/ 28 180))])
  (define (compute-xy angle)
    (values (cos angle) (sin angle)))
  (define (d x y a x* y* a* r m)
    (if (empty? r)
      m
      (match (car r)
        ('F (define-values (x+ y+) (compute-xy a))
            (d (+ x x+) (+ y y+) a        x*          y*          a*          (cdr r) (cons `(,x ,y ,(+ x x+) ,(+ y y+)) m)))
        ('+ (d x        y        (+ a o)  x*          y*          a*          (cdr r) m))
        ('- (d x        y        (- a o)  x*          y*          a*          (cdr r) m))
        ('S (d x        y        a        (cons x x*) (cons y y*) (cons a a*) (cdr r) m))
        ('G (d (car x*) (car y*) (car a*) (cdr x*)    (cdr y*)    (cdr a*)    (cdr r) m))
        (_
         (raise-argument-error 'd "Alphabet" (car r)))
    )))
  (d 0 0 (- (/ pi 2)) '() '() '() rules '()))

(define n (draw-plant draw-rules))

;; Find boundaries of the drawing
(define-values (max-x min-x max-y min-y)
  (for/fold ([max-x -inf.0]
             [min-x +inf.0]
             [max-y -inf.0]
             [min-y +inf.0])
            ([i n])
    (match i
      ([list x y x* y*] (values (max x x* max-x) (min x x* min-x) (max y y* max-y) (min y y* min-y)))
      (_ 0))))

;; Scale the plant so it fits in the image
(define dx            (- max-x min-x))
(define dy            (- max-y min-y))
(define scale-factor  (min (/ height dy) (/ width dx 2)))  ; Divide by two because we center the plant in x
(define scaled        (map (lambda (x) (map (lambda (y) (* y scale-factor)) x)) n))
(define sc*           (map (lambda (x)
                        (match x
                          ([list x y x* y*]
                           (list (+ x (/ width 2)) (+ y height) (+ x* (/ width 2)) (+ y* height))))) scaled))

;; Actually draw the plant, this part is impure and has mutation
(for ([i sc*])
  (match i
    ([list x y x* y*]
     (send dc draw-line x y x* y*))
    (_ "")))

(send target save-file "file.png" 'png)
