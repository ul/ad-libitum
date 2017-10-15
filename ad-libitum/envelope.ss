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
    (let ([starts (make-vector *channels* (now))]
          [previous-values (make-vector *channels* 0.0)]
          [next-values (make-vector *channels* 0.0)])
      (~<
       (let ([Δt (<~ Δt)]
             [current-value (<~ signal)]
             [next-value (vector-ref next-values channel)])
         (unless (= current-value next-value)
           (vector-set! previous-values channel next-value)
           (vector-set! next-values channel current-value)
           (vector-set! starts channel time))
         (let ([δt (- time (vector-ref starts channel))])
           (if (< δt Δt)
               (let ([previous-value (vector-ref previous-values channel)])
                 (+ previous-value
                    (curve (/ δt Δt) (- current-value previous-value))))
               current-value))))))
  
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
