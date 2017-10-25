#!chezscheme
(library (ad-libitum oscillator (1))
  (export phasor ///
          sine cosine square pulse tri saw sampler unroll
          sine/// cosine/// square/// pulse/// tri/// saw/// sampler///
          )
  (import (chezscheme)
          (srfi s42 eager-comprehensions)
          (ad-libitum common)
          (ad-libitum signal))

  ;; <phasor>
  (define (dynamic-phasor frequency phase0)
    (let ([previous-times (make-channel-vector 0.0)]
          [previous-phases (make-channel-vector 0.0)])
      (~<
       (let* ([previous-time (channel-ref previous-times)]
              [phase-delta (if (< previous-time time)
                               (/ (<~ frequency) *sample-rate*)
                               0.0)]
              [next-phase (-> (channel-ref previous-phases)
                              (+ phase-delta)
                              (mod 1.0))])
         (channel-set! previous-times time)
         (channel-set! previous-phases next-phase)
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
  
  (alias /// phasor)
  ;; </phasor>

  ;; <waveforms>
  (define~ (sine phase)
    (sin (* 2π (<~ phase))))
  
  (define~ (cosine phase)
    (cos (* 2π (<~ phase))))
  
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
    (let* ([N (vector-length (vector-ref table 0))]
           [N-1 (- N 1)]
           [n (fixnum->flonum N)])
      (~< (let ([position (* n (<~ phase))])
            (let ([i (-> position
                         (fltruncate)
                         (flonum->fixnum)
                         (clamp 0 N-1))]
                  [a (mod position 1.0)]
                  [table (channel-ref table)])
              (+ (* (- 1.0 a) (vector-ref table i))
                 (* a (vector-ref table (mod (+ i 1) N)))))))))
  
  (define (unroll signal base-frequency)
    (let* ([n (-> *sample-rate* (/ base-frequency) (round) (exact))]
           [table (make-channel-vector)])
      (do-ec (: channel *channels*)
             (channel-set! table (make-vector n)))
      ;; channel is in inner loop because many `signal' functions
      ;; rely on ordered sample-by-sample execution
      (do-ec (: sample n)
             (: channel *channels*)
             (vector-set!
              (channel-ref table)
              sample
              (signal (/ sample *sample-rate*) channel)))
      table))
  
  (define sine/// (∘ sine phasor))
  (define cosine/// (∘ cosine phasor))
  (define square/// (∘ square phasor))
  (define pulse///
    (case-lambda
      [(pulse-width frequency phase0)
       (pulse pulse-width (phasor frequency phase0))]
      [(pulse-width frequency)
       (pulse pulse-width (phasor frequency ∅))]))
  (define tri/// (∘ tri phasor))
  (define saw/// (∘ saw phasor))
  (define sampler///
    (case-lambda
      [(table frequency) (sampler table (phasor frequency))]
      [(table frequency phase0) (sampler table (phasor frequency phase0))]))
  ;; </waveforms>

  )
