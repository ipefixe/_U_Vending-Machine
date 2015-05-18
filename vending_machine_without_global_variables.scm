#!r6rs
(import (rnrs base) (rnrs io simple) (rnrs r5rs))

; Kevin Boulala & Maxime Dubois
; Distributeur de boissons chaudes
; PFA
; Réalisation en PLT-Scheme

(define (writeln/return x)
  (write x)
  (newline)
  x)

(define (get-1euro l) (car l))
(define (get-50cents l) (cadr l))
(define (get-20cents l) (caddr l))
(define (get-10cents l) (cadddr l))

(define (get-black-coffee l) (car l))
(define (get-white-coffee l) (cadr l))
(define (get-hot-chocolate l) (caddr l))
(define (get-tea l) (cadddr l))

(define (sum-of-money l)
  (+ (* (get-1euro l) 100) (* (get-50cents l) 50) (* (get-20cents l) 20) (* (get-10cents l) 10)))

(define (give-change l num available-money)
  (let ((diff (- (sum-of-money l) num)))
    (if (<= diff 0)
        (list (list (+ (get-1euro available-money) (get-1euro l))
                                      (+ (get-50cents available-money) (get-50cents l))
                                      (+ (get-20cents available-money) (get-20cents l))
                                      (+ (get-10cents available-money) (get-10cents l))) 
              #t)
        (begin
          (set! available-money (list (+ (get-1euro available-money) (get-1euro l))
                                      (+ (get-50cents available-money) (get-50cents l))
                                      (+ (get-20cents available-money) (get-20cents l))
                                      (+ (get-10cents available-money) (get-10cents l))))
          (give-back diff available-money)))))

(define (give-back n available-money)
  (let give-back-loop ((copy-available-money available-money)
                       (l-return '(0 0 0 0))
                       (num n))
    (if (zero? num)
        (begin
          (display "Give change finished")(newline)
          (display l-return)(newline)
          (list copy-available-money l-return))
        (if (and (>= num 100) (> (get-1euro copy-available-money) 0))
            (give-back-loop (list (- (get-1euro copy-available-money) 1) (get-50cents copy-available-money) (get-20cents copy-available-money) (get-10cents copy-available-money))
                            (list (+ (get-1euro l-return) 1) 0 0 0) 
                            (- num 100))
            (if (and (>= num 50) (> (get-50cents copy-available-money) 0))
                (give-back-loop (list (get-1euro copy-available-money) (- (get-50cents copy-available-money) 1) (get-20cents copy-available-money) (get-10cents copy-available-money))
                                (list (get-1euro l-return) (+ (get-50cents l-return) 1) 0 0) 
                                (- num 50))
                (if (and (>= num 20) (> (get-20cents copy-available-money) 0))
                    (give-back-loop (list (get-1euro copy-available-money) (get-50cents copy-available-money) (- (get-20cents copy-available-money) 1) (get-10cents copy-available-money))
                                    (list (get-1euro l-return) (get-50cents l-return) (+ (get-20cents l-return) 1) 0) 
                                    (- num 20))
                    (if (and (>= num 10) (> (get-10cents copy-available-money) 0))
                        (give-back-loop (list (get-1euro copy-available-money) (get-50cents copy-available-money) (get-20cents copy-available-money) (- (get-10cents copy-available-money) 1))
                                        (list (get-1euro l-return) (get-50cents l-return) (get-20cents l-return) (+ (get-10cents l-return) 1))
                                        (- num 10))
                        (begin 
                          (display "Not enough money to give back")(newline)
                          (list available-money #f)))))))))

(define vending-machine
  (let ((available-drink '(30 1 5 30))
        (available-money '(0 10 10 10))
        (money-as-before '()))
    (lambda (drink money)
      (newline)(display "Available drink ")(display available-drink)(newline)
      (display "Available money ")(display available-money)(newline)
      (cond ((equal? money 'as-before) (if (null? money-as-before)
                                           #f
                                           (vending-machine drink money-as-before)))
            ((< (sum-of-money money) 50)
             (begin
               (set! money-as-before '())
               #f))
            (else
             (call/cc (lambda (exit)
                        (cond ((equal? drink 'black-coffee) (if (zero? (get-black-coffee available-drink))
                                                                (begin 
                                                                  (set! money-as-before money)
                                                                  #f)
                                                                (begin
                                                                  (let ((tmp '()))
                                                                    (set! tmp (give-change money 50 available-money))
                                                                    (if (equal? (cadr tmp) #f)
                                                                        (begin
                                                                          (set! tmp (give-back (sum-of-money money) available-money))
                                                                          (set! available-money (car tmp))
                                                                          (exit #f))
                                                                        (set! available-money (car tmp)))
                                                                    (set! available-drink (list (- (get-black-coffee available-drink) 1) (get-white-coffee available-drink) (get-hot-chocolate available-drink) (get-tea available-drink)))
                                                                    (set! money-as-before '())
                                                                    (cdr tmp))
                                                                  (display "Black-coffee")(newline))))
                              ((equal? drink 'white-coffee) (if (zero? (get-white-coffee available-drink))
                                                                (begin
                                                                  (set! money-as-before money)
                                                                  #f)
                                                                (begin
                                                                  (let ((tmp '()))
                                                                    (set! tmp (give-change money 50 available-money))
                                                                    (if (equal? (cadr tmp) #f)
                                                                        (begin
                                                                          (set! tmp (give-back (sum-of-money money) available-money))
                                                                          (set! available-money (car tmp))
                                                                          (exit #f))
                                                                        (set! available-money (car tmp)))
                                                                    (set! available-drink (list (get-black-coffee available-drink) (- (get-white-coffee available-drink) 1) (get-hot-chocolate available-drink) (get-tea available-drink)))
                                                                    (set! money-as-before '())
                                                                    (cdr tmp))
                                                                  (display "White-coffee")(newline))))
                              ((equal? drink 'hot-chocolate) (if (zero? (get-hot-chocolate available-drink))
                                                                 (begin
                                                                   (set! money-as-before money)
                                                                   #f)
                                                                 (begin
                                                                   (let ((tmp '()))
                                                                     (set! tmp (give-change money 50 available-money))
                                                                     (if (equal? (cadr tmp) #f)
                                                                        (begin
                                                                          (set! tmp (give-back (sum-of-money money) available-money))
                                                                          (set! available-money (car tmp))
                                                                          (exit #f))
                                                                        (set! available-money (car tmp)))
                                                                     (set! available-drink (list (get-black-coffee available-drink) (get-white-coffee available-drink) (- (get-hot-chocolate available-drink) 1) (get-tea available-drink)))
                                                                     (set! money-as-before '())
                                                                     (cdr tmp))
                                                                   (display "Hot chocolate")(newline))))
                              ((equal? drink 'tea) (if (zero? (get-tea available-drink))
                                                                 (begin
                                                                   (set! money-as-before money)
                                                                   #f)
                                                                 (begin
                                                                   (let ((tmp '()))
                                                                     (set! tmp (give-change money 50 available-money))
                                                                     (if (equal? (cadr tmp) #f)
                                                                        (begin
                                                                          (set! tmp (give-back (sum-of-money money) available-money))
                                                                          (set! available-money (car tmp))
                                                                          (exit #f))
                                                                        (set! available-money (car tmp)))
                                                                     (set! available-drink (list (get-black-coffee available-drink) (get-white-coffee available-drink) (get-hot-chocolate available-drink) (- (get-tea available-drink) 1)))
                                                                     (set! money-as-before '())
                                                                     (cdr tmp))
                                                                   (display "Tea")(newline))))
                              (else (begin
                                      (set! money-as-before money)
                                      #f))))))))))
    
(define (run)
  (vending-machine 'black-coffee '(1 0 0 0))
  (vending-machine 'black-coffee '(1 0 0 0))
  (vending-machine 'hot-chocolate '(0 1 0 0))
  (vending-machine 'hot-chocolate '(0 1 0 0))
  (vending-machine 'hot-chocolate '(0 0 3 0))
  (vending-machine 'hot-chocolate '(0 1 0 0))
  (vending-machine 'tea '(0 1 0 0)))