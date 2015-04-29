#!r6rs
(import (rnrs base) (rnrs io simple) (rnrs r5rs))

(define (writeln/return x)
  (write x)
  (newline)
  x)

(define (get-1euro l) (car l))
(define (get-50cents l) (cadr l))
(define (get-20cents l) (caddr l))
(define (get-10cents l) (cadddr l))

(define (sum-of-money l)
  (+ (get-1euro l) (* (get-50cents l) 0.5) (* (get-20cents l) 0.2) (* (get-10cents l) 0.1)))

;(define (give-change l num)
;  (if (> num (sum-of-money l))
;      #f
; TODO
