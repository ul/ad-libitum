(define frequency 440.0)
(define my-freq~ (live-value 'frequency))
(define wave-base 22.0)
(define wave-base~ (live-value 'wave-base))

(define (~ m)
  (make-overtone
   (map constant '(0.4 0.2 0.1 0.2))
   (random-choice (list osc:sine osc:square osc:tri osc:saw))
   (*~ (/~ (~< m) wave-base~) my-freq~)
   ∅))

(define my-dsp
  (-~
   (*~ (~ 220.0) (~ 440.0))
   (*~ (~ 222.0) (~ 110.0))
   (*~ (+~ (~ 110.0) (~< 2.0))
       (+~ (~ 0.05)  (~< 1.0)))))

(my-dsp 3.1234 0)

(sound:set-dsp! (live-signal 'my-dsp))
(h!)

;;;

(define start 0.0)
(define apex 1.0)
(define frequency 440.0)

(begin
  (define (make-instrument start apex frequency)
    (let ([start (live-value start)]
          [apex (live-value apex)]
          [frequency (live-value frequency)]
          [lfo (osc:sine** (~< 220.0))])
      (*~ (pan lfo) (live-signal 'my-dsp) (env:impulse start apex))
      ;; (*~ #;(pan lfo) (live-signal 'my-dsp) #;(adsr start apex (~< 0.1) (~< 0.03) (~< 1.0) (~< 0.02)))
      ))
  (define inst (make-instrument 'start 'apex 'frequency)))

(define (make-play-note start apex frequency)
  (λ (freq dur)
    (set-top-level-value! frequency freq)
    (set-top-level-value! start (now))
    (set-top-level-value! apex (+ (now) dur))))

(define play-note (make-play-note 'start 'apex 'frequency))

(sound:set-dsp! (live-signal 'inst))

(play-note 440.0 1/16-second)

(define my-dsp (mix (osc:sine** (live-value 'frequency))
                    (osc:sine** (*~ (~< 2.0) (live-value 'frequency)))
                    (osc:sine** (+~ (~< 2.0) (live-value 'frequency)))
                    (osc:sine** (~< 220.0))
                    (osc:sine** (~< 110.0))
                    (osc:sine** (~< 50.0))
                    (osc:sine** (~< 432.0))
                    ;; (osc:sine** (~< 441.0))
                    ))

(define my-dsp (make-overtone (map constant '(0.4 0.2 0.1 0.1))
                              osc:sine
                              (constant 440.0)
                              silence))

(sound:set-dsp! (live-signal 'my-dsp))
(sound:set-dsp! tuner)
(h!)

;;;;;;;

(define (make-overlap wave freq shift)
  (mix
   (wave (osc:phasor (constant freq) silence))
   (wave (osc:phasor (constant freq) (constant shift)))))

(define my-dsp
  (make-overlap osc:saw frequency 0.0123))
(h!)

(define my-dsp (osc:pulse (osc:phasor (~< 440.0) ∅)
                           (*~ (~< 0.5)
                               (+~ (osc:pulse*
                                    (osc:phasor* (~< 432.0))
                                    (osc:sine** (~< 1.0)))
                                   (~< 1.0)))))
(define my-dsp (osc:sine* (~< 440.0) (osc:square** (~< 220.0))))
(sound:set-dsp! (live-signal 'my-dsp))
(h!)

;;;

(define (be-playful-with-frequency)
  (play-note (random-choice (make-scale 440.0 pentatonic-scale))
             (random-choice
              (list
               second
               ;; half-second
               ;; quarter-second
               ;; quarter-second
               ;; quarter-second
               ;; quarter-second
               ;; 1/8-second
               ;; 1/8-second
               ;; 1/8-second
               ;; 1/8-second
               1/16-second
               1/16-second
               1/16-second
               1/16-second
               1/32-second
               1/32-second
               1/32-second
               1/32-second
               1/32-second
               1/32-second
               1/32-second
               1/32-second
               )))
  (schedule
   (+ (now)
      (random-choice
       (list
        second
        half-second
        ;; quarter-second
        ;; quarter-second
        ;; quarter-second
        ;; quarter-second
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

;;;;
