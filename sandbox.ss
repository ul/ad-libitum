(define my-freq (constant tuner-frequency))

(define (modulator freq) (sine-wave (phase* freq)))

(define my-dsp
  (sine-wave (phase
              (live-signal 'my-freq)
              (modulator (constant 432.0)))))

(sound:set-dsp! (live-signal 'my-dsp))
(sound:set-dsp! tuner)
(sound:hush!)

;;;

(define second (make-time 'time-duration 0 1))
(define half-second (make-time 'time-duration 500000000 0))
(define quarter-second (make-time 'time-duration 250000000 0))
(define 1/8-second (make-time 'time-duration 125000000 0))
(define 1/16-second (make-time 'time-duration 62500000 0))
(define 1/32-second (make-time 'time-duration 31250000 0))

(define (random-choice list)
  (list-ref list (random (length list))))

(define chromatic-scale-half-step
  (expt 2 1/12))

(define second-interval (expt chromatic-scale-half-step 2))
(define third-interval (expt chromatic-scale-half-step 4))
(define perfect-fourth-interval (expt chromatic-scale-half-step 5))
(define perfect-fifth-interval (expt chromatic-scale-half-step 7))
(define major-sixth-interval (expt chromatic-scale-half-step 9))
(define major-seventh-interval (expt chromatic-scale-half-step 11))
(define perfect-octave-interval (expt chromatic-scale-half-step 12))
(define minor-second-interval (expt chromatic-scale-half-step 1))
(define minor-third-interval (expt chromatic-scale-half-step 3))
(define minor-sixth-interval (expt chromatic-scale-half-step 8))
(define minor-seventh-interval (expt chromatic-scale-half-step 11))
(define triton-interval (expt chromatic-scale-half-step 11))

;; TODO excercise: represent scales as whole/half steps

(define chromatic-scale '(1 2 3 4 5 6 7 8 9 10 11 12))
(define pentatonic-scale '(1 3 5 8 10))
(define major-scale '(1 3 5 6 8 10 12))
(define minor-scale '(1 3 4 6 8 9 11))

(define (make-scale base-frequency scale)
  (map (lambda (x) (* base-frequency (expt chromatic-scale-half-step (- x 1)))) scale))

(make-scale tuner-frequency pentatonic-scale)

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
                   1/8-second
                   1/8-second
                   1/16-second
                   1/16-second
                   1/16-second
                   1/16-second
                   1/32-second
                   1/32-second
                   )))
   'be-playful-with-frequency))

(be-playful-with-frequency)
