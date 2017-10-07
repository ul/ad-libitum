(library (ad-libitum oscillator (1))
  (export phasor
          sine cosine square pulse tri saw sampler
          sine* cosine* square* pulse* tri* saw*
          )
  (import (chezscheme)
          (ad-libitum common)
          (ad-libitum signal))

  (define (dynamic-phasor frequency phase0)
    (let ([previous-phase (make-vector *channels* 0.0)])
      (~<
       (let* ([phase-delta (/ (<~ frequency) *sample-rate*)]
              [next-phase (-> (vector-ref previous-phase channel)
                              (+ phase-delta)
                              (mod 1.0))])
         (vector-set! previous-phase channel next-phase)
         (-> (<~ phase0)
             (+ next-phase)
             (mod 1.0))))))
  
  (define~ (static-phasor frequency phase0)
    (-> time (* frequency) (+ phase0) (mod 1.0)))
  
  (define phasor
    (case-lambda
      [(frequency phase0)
       (if (number? frequency)
           (static-phasor frequency phase0)
           (dynamic-phasor frequency phase0))]
      [(frequency)
       (if (number? frequency)
           (static-phasor frequency 0.0)
           (dynamic-phasor frequency ∅))]))
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
  (define pulse*
    (case-lambda
      [(pulse-width frequency phase0)
       (pulse pulse-width (phasor frequency phase0))]
      [(pulse-width frequency)
       (pulse* pulse-width frequency ∅)]))
  (define tri* (∘ tri phasor))
  (define saw* (∘ saw phasor))
  )
