; ============ IMP-equivalent in SMT-LIB ============
; Encodes the semantics of test.mdc as SMT-LIB2 logical constraints.

(define-fun square ((x Int)) Int
  (* x x))

(define-fun abs ((x Int)) Int
  (ite (>= x 0)
       x
       (- x)))

(declare-const x Int)

(assert (= x -10))            ; def x = 10

(declare-const y Int)
;(assert (= y (abs x)))

(assert (= y (ite (>= x 0)
       x
       (- x))))

(assert (not (> y x)))

(check-sat)