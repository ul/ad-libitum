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

(define-values (frequency set-frequency!) (ctrl:make-control 440.0))
(define-values (transition set-transition!) (ctrl:make-control 15.0))
(define-values (feedback set-feedback!) (ctrl:make-control 0.0))
(define-values (dly set-delay!) (ctrl:make-control 0.0))
(define feedback~ (env:linear-transition transition feedback))
(define delay~ (env:linear-transition transition dly))
(define frequency~ (env:linear-transition transition frequency))
(quick-test feedback~)

(play! (mono (filter:echo* delay~ feedback~ (live-signal 'my-dsp))))
(h!)

(set-delay! 1.0)
(set-delay! 0.1)
(set-delay! 0.03)
(set-delay! 0.01)
(set-feedback! 0.999)
(set-feedback! 0.5)
(set-feedback! 0.1)
(set-transition! 1.0)

(midi:start now)

(define (debounce Δt f)
  (let* ([i 0]
         [f* (λ (i* args)
               (when (= i i*)
                 (apply f args)))])
    (λ args
      (set! i (+ i 1))
      (schedule (+ (now) (Δt)) f* i args))))

;; (define Δt* 0.1)
;; (define (Δt) Δt*)
;; (define midi-set-delay! (debounce Δt set-delay!))
;; (define midi-set-feedback! (debounce Δt set-feedback!))
;; (define midi-set-transition! (debounce Δt set-transition!))
;; (define midi-set-frequency! (debounce Δt set-frequency!))

(midi:set-cc! (λ (t knob value channel)
                (case knob
                  [(19) (set-delay! (/ value 16.0))]
                  [(23) (set-feedback! (/ value 128.0))]
                  [(27) (set-transition! (/ value 8.0))]
                  [(31) (set-frequency! (midi-pitch->frequency value))]
                  ;; [(31) (midi-set-frequency! (midi-pitch->frequency value))]
                  )
                )
              )

(play! (*~ (~< 0.1) (osc:sine/// frequency~)))
(h!)
