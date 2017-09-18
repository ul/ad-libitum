(define my-freq (constant tuner-frequency))

(define (modulator freq) (sine-wave (phase* freq)))

(define (make-overtone wave frequency phase0)
  (+~ (*~ (wave (phase frequency phase0)) (constant 0.4))
      (*~ (wave (phase (*~ (constant 2.0) frequency) phase0)) (constant 0.2))
      (*~ (wave (phase (*~ (constant 3.0) frequency) phase0)) (constant 0.1))
      (*~ (wave (phase (*~ (constant 4.0) frequency) phase0)) (constant 0.1))
      (*~ (wave (phase (*~ (constant 0.5) frequency) phase0)) (constant 0.2))
      ))

(define my-overtone (make-overtone sine-wave (live-signal 'my-freq) silence))
(define my-overtone-2 (make-overtone tri-wave (live-signal 'my-freq) silence))
(define my-overtone-3 (make-overtone square-wave (live-signal 'my-freq) silence))

(define my-dsp
  (sine-wave (phase*
              (live-signal 'my-freq)
              ;; (modulator (constant 432.0))
              )))

;; TODO add normalizing version of +~
;; TODO sin is heavy, add table unroller

(define osc220 (sine-wave (phase* (constant 220.0))))
(define osc440 (sine-wave (phase* (constant 440.0))))
(define osc110 (sine-wave (phase* (constant 110.0))))
(define osc0.25 (sine-wave (phase* (constant 0.25))))

(define my-dsp
  (/~
   (*~ osc220 osc440)
   (*~ (+~ osc110 (constant 2.0))
       (+~ osc0.25 (constant 1.0)))))

(define my-dsp
  (*~ (constant 0.3)
      (+~ #;my-overtone my-overtone-2 #;my-overtone-3)))

(sound:set-dsp! (live-signal 'my-dsp))
(sound:set-dsp! tuner)
(sound:hush!)

;;;

(define (swap-frequency)
  (set! my-freq (constant (random-choice (make-scale 440.0 #;tuner-frequency pentatonic-scale)))))

(define (be-playful-with-frequency)
  (swap-frequency)
  (schedule
   (add-duration (current-time)
                 (random-choice
                  (list
                   second
                   half-second
                   quarter-second
                   quarter-second
                   quarter-second
                   quarter-second
                   ;; 1/8-second
                   ;; 1/8-second
                   ;; 1/8-second
                   ;; 1/8-second
                   ;; 1/16-second
                   ;; 1/16-second
                   ;; 1/16-second
                   ;; 1/16-second
                   ;; 1/32-second
                   ;; 1/32-second
                   )))
   'be-playful-with-frequency))

(be-playful-with-frequency)
