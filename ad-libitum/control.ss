(library (ad-libitum control (1))
  (export control-signal instant-transition linear-transition *linear-transition*)
  (import (chezscheme)
          (ad-libitum common)
          (ad-libitum signal)
          (only (ad-libitum sound) now)
          )
  ;; <control-signal>
  (define *sound-barrier* 0.03)
  
  (define (linear-transition Δt)
    (λ (δt x y)
      (if (< δt Δt)
          (+ x (* (- y x) (/ δt Δt)))
          y)))
  
  (define instant-transition
    (λ (δt x y)
      y))
  
  (define *linear-transition* (linear-transition *sound-barrier*))
  
  (define control-signal
    (case-lambda
      [(x)
       (if (number? x)
           (control-signal id x *linear-transition*)
           (control-signal x 0.0 *linear-transition*))]
      [(f x)
       (control-signal f x *linear-transition*)]
      [(f x transition)
       (let ([start (now)]
             [x x]
             [y x])
         (values
          (~< (transition (- time start) x y))
          (λ args
            (set! x (transition (- (now) start) x y))
            (set! y (apply f args))
            (set! start (now)))))]))
  ;; </control-signal>

  )
