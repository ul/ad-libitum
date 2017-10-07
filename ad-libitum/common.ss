#!chezscheme
(library (ad-libitum common (1))
  (export voodoo λ id >>> >> -> ->>
          compose ∘ pi two-pi π 2π
          random-amplitude
          midi2frq frq2midi
          amp2db db2amp
          *channels* *sample-rate* *sample-angular-period*)
  (import (chezscheme)
          (only (srfi s1 lists) reduce)
          (srfi s26 cut)
          (prefix (ad-libitum sound) sound:))

  (alias *sample-rate* sound:*sample-rate*)
  (alias *channels* sound:*channels*)

  (define (voodoo)
    (collect-maximum-generation 254)
    (collect-generation-radix 2)
    ;; (eval-when (compile) (optimize-level 3))
    (optimize-level 2)
    )

  ;; symbols
  (alias λ lambda)

  ;; can't live without
  (define (id x) x)

  (define (compose . fns)
    (define (make-chain fn chain)
      (λ args (call-with-values (cut apply fn args) chain)))
    (reduce make-chain values fns))
  
  (alias ∘ compose)
  (define pi (inexact (* (asin 1.0) 2)))
  (define two-pi (fl* 2.0 pi))
  (alias π pi)
  (alias 2π two-pi)
  
  (define (mod1 x)
    (mod x 1.0))
  
  (define (random-amplitude)
    (- (random 2.0) 1.0))
  
  (define (midi2frq pitch)
    (if (<= pitch 0.0) 0.0
        (* 440.0 (expt 2.0 (/ (- pitch 69.0) 12.0)))))
  
  (define (frq2midi freq)
    (if (<= freq 0.0) 0.0
        (+ (* 12.0 (log (/ freq 440.0) 2.0)) 69.0)))
  
  (define (amp2db x)
    (* 20.0 (log x 10)))
  
  (define (db2amp x)
    (expt 10.0 (/ x 20.0)))

  (define *sample-angular-period* (/ 2π *sample-rate*))

  ;; threading

  (define-syntax (>>> stx)
    (syntax-case stx ()
      [(_ it x) #'x]
      [(_ it x (y ...) rest ...)
       #'(let ([it x])
           (>>> it (y ...) rest ...))]))

  (define-syntax (>> stx)
    (syntax-case stx ()
      [(k rest ...)
       (with-syntax ([^ (datum->syntax #'k '^)])
         #'(>>> ^ rest ...))]))

  (define-syntax ->
    (syntax-rules ()
      [(_ x) x]
      [(_ x (y z ...) rest ...)
       (-> (y x z ...) rest ...)]))

  (define-syntax ->>
    (syntax-rules ()
      [(_ x) x]
      [(_ x (y ...) rest ...)
       (->> (y ... x) rest ...)])))
