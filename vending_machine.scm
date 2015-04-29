#!r6rs
(import (rnrs base) (rnrs io simple) (rnrs r5rs))

(define (writeln/return x)
  (write x)
  (newline)
  x)

(define available-money '(10 10 10 10))
(define as-before '())

(define (get-1euro l) (car l))
(define (get-50cents l) (cadr l))
(define (get-20cents l) (caddr l))
(define (get-10cents l) (cadddr l))

(define (sum-of-money l)
  (+ (* (get-1euro l) 100) (* (get-50cents l) 50) (* (get-20cents l) 20) (* (get-10cents l) 10)))

(define (give-change l num)
  (let ((diff (- (sum-of-money l) num)))
    (if (zero? diff)
        #f
        (begin
          (set! available-money (list (+ (get-1euro available-money) (get-1euro l))
                                      (+ (get-50cents available-money) (get-50cents l))
                                      (+ (get-20cents available-money) (get-20cents l))
                                      (+ (get-10cents available-money) (get-10cents l))))
          (give-back diff)))))

(define (give-back n)
  (let give-back-loop ((l-return '(0 0 0 0))
                       (num n))
    (if (zero? num)
        (begin
          (display "Give change finished")(newline)
          l-return)
        (if (and (>= num 100) (> (get-1euro available-money) 0))
            (begin
              (set! available-money (list (- (get-1euro available-money) 1) (get-50cents available-money) (get-20cents available-money) (get-10cents available-money)))
              (give-back-loop (list (+ (get-1euro l-return) 1) 0 0 0) (- num 100)))
            (if (and (>= num 50) (> (get-50cents available-money) 0))
                (begin
                  (set! available-money (list (get-1euro available-money) (- (get-50cents available-money) 1) (get-20cents available-money) (get-10cents available-money)))
                  (give-back-loop (list (get-1euro l-return) (+ (get-50cents l-return) 1) 0 0) (- num 50)))
                (if (and (>= num 20) (> (get-20cents available-money) 0))
                    (begin
                      (set! available-money (list (get-1euro available-money) (get-50cents available-money) (- (get-20cents available-money) 1) (get-10cents available-money)))
                      (give-back-loop (list (get-1euro l-return) (get-50cents l-return) (+ (get-20cents l-return) 1) 0) (- num 20)))
                    (if (and (>= num 10) (> (get-10cents available-money) 0))
                        (begin
                          (set! available-money (list (get-1euro available-money) (get-50cents available-money) (get-20cents available-money) (- (get-10cents available-money) 1)))
                          (give-back-loop (list (get-1euro l-return) (get-50cents l-return) (get-20cents l-return) (+ (get-10cents l-return) 1)) (- num 10)))
                        (display "Not enough money to give back"))))))))
