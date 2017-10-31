(begin
  (define (make-note frequency duration)
    (let ([frequency (~< frequency)])
      (let-values ([(start end) (fix-duration (* 0.25 duration))])
        (*~
         (make-overtone (map constant '(0.4 0.2 0.1)) ;; PLAY
                        ;; (cut osc:pulse (amplitude->phase (osc:sine/// (~< 200.0))) <>) ;; PLAY
                        (λ (phase)
                          (/~
                           (osc:sine/// #;frequency (~< 44.0))
                           (+~ (~< 2.0) (osc:sine phase))
                           #;(osc:pulse (+~ (~< 0.1)
                           (*~ (~< 0.05) noise:white )) phase)))
                        frequency
                        ∅)
         ;; (env:adsr start end (~< 0.01) (~< 0.05) (~< 0.4) (~< 0.01))
         (env:impulse start end)
         ))))

  (define-values (my-dsp play-note) (inst:make-polyphony 6 make-note)))

(define (be-playful-with-frequency)
  (play-note (random-choice (scale:make-scale 880.0 scale:pentatonic-scale)) ;; PLAY
             (random-choice (list
                             1/8
                             1/8
                             1/16
                             1/16
                             1/16
                             1/16
                             )))
  (schedule
   (+ (now)
      (random-choice
       (list
        1/2
        1/2
        1/4
        1/4
        1/4
        1/4
        1/8
        1/8
        )))
   'be-playful-with-frequency))

(be-playful-with-frequency)

(ctrl:define-control frequency 440.0)
(ctrl:define-control transition 15.0)
(ctrl:define-control feedback 0.0)
(ctrl:define-control delay 0.0)
(define feedback~ (env:linear-transition transition~ feedback~))
(define delay~ (env:linear-transition transition~ delay~))
(define frequency~ (env:linear-transition transition~ frequency~))
(quick-test feedback~)

(play! (mono (filter:echo* delay~ feedback~ (live-signal 'my-dsp))))
(h!)

(delay-set! 1.0)
(delay-set! 0.1)
(delay-set! 0.03)
(delay-set! 0.01)
(feedback-set! 0.999)
(feedback-set! 0.5)
(feedback-set! 0.1)
(transition-set! 1.0)

(midi:start now)

(midi:set-cc! (λ (t knob value channel)
                (case knob
                  [(19) (delay-set! (/ value 16.0))]
                  [(23) (feedback-set! (/ value 128.0))]
                  [(27) (transition-set! (/ value 8.0))]
                  [(31) (frequency-set! (midi-pitch->frequency value))]
                  ;; [(31) (midi-set-frequency! (midi-pitch->frequency value))]
                  )
                )
              )

(play! (*~ (~< 0.1) (osc:sine/// frequency~)))
(h!)
