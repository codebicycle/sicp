;; ex 1.1

; 10
; 12
; 8
; 3
; 6
; a
; b
; 19
; #f
; 4
; 16
; 6
; 16

;; ex 1.2

(/  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7)))

;; ex 1.3
;; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))

(define (sum_sqare_largest a b c)
  (define sum_squares (+ (square a) (square b) (square c)))
  (define square_min (square (min a b c)))
  (- sum_squares square_min)
 )

(sum_sqare_largest 1 2 3)

;; ex 1.4

; The operator is chosen based on b's value in the if statement.
; The body of the procedure becomes (+ a b) if b > 0 and (- a b) otherwise.

;; ex 1.5

; Applicative-order evaluation - evaluate the arguments and then apply
; produces an infinite loop when the second argument of the test procedure is evaluated.
; Normal-order evaluation - fully expand and then reduce (evaluate the arguments only when needed)
; will produce the value 0. The alternative from the if statement will never be evaluated.


;; sqrt

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- x (square guess))) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

;; ex 1.6

; if special form has the capability to short-circuit, to evaluate only one branch and not the other.
; new-if on the other hand does not have this capability so both branches are evaluated.
; In the square root program even when the guess is good enough the alternative branch of the new-if is
; evaluated producing an infinite loop

;; ex 1.7

(define (sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (good-enough? new-guess guess)
    guess
    (sqrt-iter new-guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? new-guess guess)
  (<= (abs (- new-guess guess)) (* guess 0.001)))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
