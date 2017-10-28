(library (ad-libitum envelope (1))
  (export adsr impulse
          transition instant-transition linear-transition quadratic-transition
          )
  (import (chezscheme)
          (srfi s26 cut)
          (ad-libitum common)
          (ad-libitum signal)
          (only (ad-libitum sound) now)
          )
  ;; <adsr>
  (define~ (adsr start end attack decay sustain release)
    (let ([end (<~ end)])
      (if (<= end time)
          ;; NOTE OFF
          (let ([Δt (- time end)]
                [r (<~ release)])
            (if (and (positive? r)
                     (<= Δt r))
                (* (- 1.0 (/ Δt r)) (<~ sustain))
                0.0))
          ;; NOTE ON
          (let ([start (<~ start)])
            (if (<= start time)
                (let ([Δt (- time start)]
                      [a (<~ attack)])
                  (if (and (positive? a)
                           (<= Δt a))
                      (/ Δt a)
                      (let ([Δt (- Δt a)]
                            [d (<~ decay)]
                            [s (<~ sustain)])
                        (if (and (positive? d)
                                 (<= Δt d))
                            (- 1.0 (* (- 1.0 s) (/ Δt d)))
                            s))))
                0.0)))))
  ;; </adsr>

  ;; <impulse>
  (define~ (impulse start apex)
    (let ([start (<~ start)])
      (if (<= start time)
          (let ([h (/ (- time start)
                      (- (<~ apex) start))])
            (* h (exp (- 1.0 h))))
          0.0)))
  ;; </impulse>

  ;; <transition>
  (define (transition curve Δt signal)
    (let ([starts (make-channel-vector (now))]
          [previous-values (make-channel-vector 0.0)]
          [current-values (make-channel-vector 0.0)]
          [next-values (make-channel-vector 0.0)])
      (~<
       (let ([Δt (<~ Δt)]
             [current-value (<~ signal)]
             [next-value (channel-ref next-values)])
         (unless (= current-value next-value)
           (channel-set! previous-values (channel-ref current-values))
           (channel-set! next-values current-value)
           (channel-set! starts time))
         (let ([current-value
                (let ([δt (- time (channel-ref starts))])
                  (if (and (positive? Δt) (< δt Δt))
                      (let ([previous-value (channel-ref previous-values)])
                        (+ previous-value
                           (curve (/ δt Δt) (- current-value previous-value))))
                      current-value))])
           (channel-set! current-values current-value)
           current-value)))))
  
  (define (instant-curve a Δx)
    Δx)
  
  (define (linear-curve a Δx)
    (* a Δx))
  
  (define (quadratic-curve a Δx)
    (* (expt a 4.0) Δx))
  
  (define instant-transition (cut transition instant-curve unit <>))
  (define linear-transition (cut transition linear-curve <> <>))
  (define quadratic-transition (cut transition quadratic-curve <> <>))
  ;; </transition>

  )
