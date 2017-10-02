(begin
  (define (make-note frequency duration)
    (let ([frequency (~< frequency)])
      (let-values ([(start end) (fix-duration (* 0.25 duration))])
        (*~
         (~< 1.5)
         (make-overtone (map constant '(0.4 0.2 0.1)) ;; PLAY
                        ;; (cut osc:pulse (amp2phase (osc:sine* (~< 200.0))) <>) ;; PLAY
                        (λ (phase)
                          (/~
                           (osc:sine* #;frequency (~< 44.0))
                           (+~ (~< 2.0) (osc:sine phase))
                           #;(osc:pulse (+~ (~< 0.1)
                                          (*~ (~< 0.05) noise:white )) phase)))
                        frequency
                        ∅)
         ;; (env:adsr start end (~< 0.01) (~< 0.05) (~< 0.4) (~< 0.01))
         (env:impulse start end)
         ))))

  (define-values (my-dsp play-note) (make-polyphony 6 make-note)))

(define (be-playful-with-frequency)
  (play-note (random-choice (make-scale 880.0 pentatonic-scale)) ;; PLAY
             (random-choice (list
                             1/8-second
                             1/8-second
                             1/16-second
                             1/16-second
                             1/16-second
                             1/16-second
                             )))
  (schedule
   (+ (now)
      (random-choice
       (list
        half-second
        half-second
        quarter-second
        quarter-second
        quarter-second
        quarter-second
        1/8-second
        1/8-second
        )))
   'be-playful-with-frequency))

(be-playful-with-frequency)

(sound:set-dsp! (live-signal 'my-dsp))
