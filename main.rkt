#! /usr/bin/env racket
#lang racket

(require racket/draw)

(define target (make-bitmap 300 300))
(define dc (new bitmap-dc% [bitmap target]))

(define A '(F + - S G))
(define omega '(F))
(define rewrite '(F S + F G F S - F S + F G S - F G G F))

(define draw-rules
  (for/fold ([symbols omega])
            ([i       6])
    (flatten
      (map (lambda (x)
             (match x
               ('F rewrite)
               (_  x)))
             symbols))))

(define (compute-xy distance angle)
  (values (* distance (cos angle)) (* distance (sin angle))))

(define (find-xy xy xy* angle angle* rules)
  (if (empty? rules)
    xy
    (match (first rules)
      ('F (define-values (x y) (compute-xy 1 angle))
          (find-xy (list (+ x (first xy)) (+ y (second xy))) xy* angle angle* (rest rules)))
      ('+ (find-xy xy xy* (+ angle (* pi (/ 28 180)))                  angle* (rest rules)))
      ('- (find-xy xy xy* (- angle (* pi (/ 28 180)))                  angle* (rest rules)))
      ('S (find-xy xy (cons xy xy*) angle                 (cons angle angle*) (rest rules)))
      ('G (find-xy (first xy*) (rest xy*) (first angle*)  (rest angle*)    (rest rules)))
      (_ 0))
  ))

(define (draw-it* xy angle rules [scale 1])
  (writeln scale)
  (define (draw-it xy xy* angle angle* rules)
    (if (empty? rules)
      xy
      (match (first rules)
        ('F (define-values (x y) (compute-xy scale angle))
            (send dc draw-line (first xy) (second xy) (+ x (first xy)) (+ y (second xy)))
            (draw-it (list (+ x (first xy)) (+ y (second xy))) xy* angle angle* (rest rules)))
        ('+ (draw-it xy xy* (+ angle (* pi (/ 28 180)))                  angle* (rest rules)))
        ('- (draw-it xy xy* (- angle (* pi (/ 28 180)))                  angle* (rest rules)))
        ('S (draw-it xy (cons xy xy*) angle                 (cons angle angle*) (rest rules)))
        ('G (draw-it (first xy*) (rest xy*) (first angle*)  (rest angle*)    (rest rules)))
        (_ 0))
    ))
  (draw-it xy empty angle empty rules))

(define length-y (second (find-xy '(150 300) '() (/ pi 2) '() draw-rules)))
(writeln length-y)
(draw-it* '(150 300) (- (/ pi 2)) draw-rules (/ 300 (- length-y 300)))

(send target save-file "file.png" 'png)
