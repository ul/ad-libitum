(library (ad-libitum oscillator (1))
  (export phasor phasor*
          sine cosine square pulse tri saw sampler
          sine* cosine* square* pulse* tri* saw* sampler*
          sine** cosine** square** pulse** tri** saw** sampler**
          )
  (import (chezscheme)
          (ad-libitum common)
          (ad-libitum signal))

  ;; TODO make actual
  (define *channels* 2)

  (define (phasor frequency phase0)
    (let ([previous-time (make-vector *channels* 0.0)]
          [previous-phase (make-vector *channels* 0.0)])
      (~<
        (let* ([time-delta (- time (vector-ref previous-time channel))]
               [phase-delta (* time-delta (<~ frequency))]
               [next-phase (mod1 (+ (vector-ref previous-phase channel) phase-delta))])
          (vector-set! previous-time channel time)
          (vector-set! previous-phase channel next-phase)
          (mod1 (+ next-phase (<~ phase0)))))))
  
  (define (phasor* frequency)
    (phasor frequency ∅))
  (define~ (sine phase)
    (sin (* two-pi (<~ phase))))
  
  (define~ (cosine phase)
    (cos (* two-pi (<~ phase))))
  
  (define~ (square phase)
    (if (< (<~ phase) 0.5)
        1.0
        -1.0))
  
  ;; when `pulse-width' is `(constant 0.5)' it's identical to `square-wave'
  (define~ (pulse pulse-width phase)
    (if (< (<~ phase) (<~ pulse-width))
        1.0
        -1.0))
  
  (define~ (tri phase)
    (let ([phase (<~ phase)])
      (if (< phase 0.5)
          (- (* 4.0 phase) 1.0)
          (+ (* -4.0 phase) 3.0))))
  
  (define~ (saw phase)
    (- (* 2.0 (<~ phase)) 1.0))
  
  (define (sampler table phase)
    (let ([n (fixnum->flonum (vector-length table))])
      (~< (vector-ref table (flonum->fixnum (fltruncate (fl* (<~ phase) n)))))))
  
  (define sine* (∘ sine phasor))
  (define cosine* (∘ cosine phasor))
  (define square* (∘ square phasor))
  (define pulse* (∘ pulse phasor))
  (define tri* (∘ tri phasor))
  (define saw* (∘ saw phasor))
  (define (sampler* table frequency phase)
    (sampler table (phasor frequency phase)))
  
  (define sine** (∘ sine phasor*))
  (define cosine** (∘ cosine phasor*))
  (define square** (∘ square phasor*))
  (define pulse** (∘ pulse phasor*))
  (define tri** (∘ tri phasor*))
  (define saw** (∘ saw phasor*))
  (define (sampler** table frequency)
    (sampler table (phasor* frequency)))
  )
