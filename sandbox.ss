(alias ! constant)

(define (var~ symbol)
  (λ (time channel)
    (top-level-value symbol)))

(define my-freq tuner-frequency)

(define my-freq~ (var~ 'my-freq))

(define wave-base 22.0)

(define wave-base~ (var~ 'wave-base))

(define (~ m)
  (make-overtone
   (random-choice (list sine-wave square-wave tri-wave saw-wave))
   (*~ (/~ (! m) wave-base~) my-freq~)
   silence))

(define my-dsp
  (-~
   (*~ (~ 220.0) (~ 440.0))
   (*~ (~ 222.0) (~ 110.0))
   (*~ (+~ (~ 110.0) (! 2.0))
       (+~ (~ 0.05)  (! 1.0)))))

(sound:set-dsp! (live-signal 'my-dsp))

;;;

(define (swap-frequency)
  (set! my-freq (random-choice (make-scale 440.0 pentatonic-scale))))

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

;; (sound:set-dsp! tuner)
;; (h!)

;; TODO + Harmony Search for params!
