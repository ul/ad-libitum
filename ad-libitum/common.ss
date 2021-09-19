#!chezscheme
(library (ad-libitum common (1))
  (export voodoo λ id >>> >> -> ->>
          compose ∘ pi two-pi π 2π clamp
          choice random-choice
          random-amplitude
          amp->dB dB->amp midi-pitch->frequency frequency->midi-pitch
          *channels* *sample-rate* *sample-angular-period*)
  (import (chezscheme)
          (only (srfi s1 lists) reduce)
          (srfi s26 cut)
          (prefix (ad-libitum sound) sound:))

  (alias *sample-rate* sound:*sample-rate*)
  (alias *channels* sound:*channels*)

  (define (voodoo)
    (collect-maximum-generation 6)
    (collect-generation-radix 2)
    ;; (eval-when (compile) (optimize-level 3))
    (optimize-level 2)
    )

  ;; symbols
  (alias λ lambda)

  ;; can't live without
  (define (id x) x)

  ;; <compose>
  (define (compose . fns)
    (define (make-chain fn chain)
      (λ args (call-with-values (cut apply fn args) chain)))
    (reduce make-chain values fns))
  
  (alias ∘ compose)
  ;; </compose>

  ;; <basic-math>
  (define pi (inexact (* (asin 1.0) 2)))
  (define two-pi (* 2.0 pi))
  (alias π pi)
  (alias 2π two-pi)
  
  (define (random-amplitude)
    (- (random 2.0) 1.0))
  
  (define (clamp value start end)
    (cond
     [(< value start) start]
     [(> value end) end]
     [else value]))
  ;; </basic-math>

  (define (amp->dB x)
    (* 20.0 (log x 10.0)))
  
  (define (dB->amp x)
    (expt 10.0 (/ x 20.0)))
  
  (define (midi-pitch->frequency m)
    (* 440.0 (expt 2.0 (/ (- m 69.0) 12.0))))
  
  (define (frequency->midi-pitch f)
    (+ 69 (exact (round (* 12.0 (log (/ f 440.0) 2.0))))))

  ;; <choice>
  (define (choice list n)
    (list-ref list (mod n (length list))))
  
  (define (random-choice list)
    (list-ref list (random (length list))))
  ;; </choice>


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
