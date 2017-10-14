(library (ad-libitum envelope (1))
  (export adsr impulse)
  (import (chezscheme)
          (ad-libitum common)
          (ad-libitum signal))
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
  )
