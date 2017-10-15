#!chezscheme
(library (ad-libitum signal (1))
  (export signal ~< <~ define-signal define~ constant silence ∅ live-signal live-value
          signal-sum signal-prod signal-diff signal-div
          +~ *~ -~ /~ ∑ ∏ mix pan)
  (import (chezscheme)
          (srfi s26 cut)
          (ad-libitum common))
  ;; <signal>
  (define-syntax (signal stx)
    (syntax-case stx ()
      [(k body ...)
       (with-syntax ([time (datum->syntax #'k 'time)]
                     [channel (datum->syntax #'k 'channel)])
         #'(λ (time channel) body ...))]))
  
  (alias ~< signal)
  
  (define-syntax (define-signal stx)
    (syntax-case stx ()
      [(k args body ...)
       (with-syntax ([time (datum->syntax #'k 'time)]
                     [channel (datum->syntax #'k 'channel)])
         #'(define args
             (λ (time channel)
               body ...)))]))
  
  (alias define~ define-signal)
  ;; </signal>

  ;; <constant>
  (define~ (constant amplitude) amplitude)
  ;; </constant>

  ;; <silence>
  (define~ silence 0.0)
  (alias ∅ silence)
  ;; </silence>

  (define-syntax (<~ stx)
    (syntax-case stx ()
      [(k signal)
       (with-syntax ([time (datum->syntax #'k 'time)]
                     [channel (datum->syntax #'k 'channel)])
         #'(signal time channel))]))

  ;; <live-signal>
  (define~ (live-signal symbol) (<~ (top-level-value symbol)))
  ;; </live-signal>

  ;; <live-value>
  (define~ (live-value symbol) (top-level-value symbol))
  ;; </live-value>

  ;; <signal-operators>
  (define~ (signal-sum* x y)
    (+ (<~ x) (<~ y)))
  
  (define (signal-sum x . xs)
    (fold-left signal-sum* x xs))
  
  (define~ (signal-prod* x y)
    (* (<~ x) (<~ y)))
  
  (define (signal-prod x . xs)
    (fold-left signal-prod* x xs))
  
  (define (signal-diff x . xs)
    (let ([y (apply signal-sum xs)])
      (~< (- (<~ x) (<~ y)))))
  
  (define (signal-div x . xs)
    (let ([y (apply signal-prod xs)])
      (~< (/ (<~ x) (<~ y)))))
  
  (alias +~ signal-sum)
  (alias *~ signal-prod)
  (alias -~ signal-diff)
  (alias /~ signal-div)
  
  (define ∑ (cut apply signal-sum <...>))
  
  (define ∏ (cut apply signal-prod <...>))
  
  ;; normalizing +~
  (define (mix . args)
    (*~ (∑ args) (constant (inexact (/ (sqrt (length args)))))))
  
  (define~ (pan p)
    (let ([p (* 0.5 (+ 1.0 (<~ p)))])
      (if (zero? channel)
          (- 1.0 p)
          p)))
  ;; </signal-operators>

  )
