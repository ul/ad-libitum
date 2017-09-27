(define frequency 440.0)
(define my-freq~ (live-value 'frequency))
(define wave-base 22.0)
(define wave-base~ (live-value 'wave-base))

(define (~ m)
  (make-overtone
   (map constant '(0.4 0.2 0.1 0.2))
   (random-choice (list sine-wave square-wave tri-wave saw-wave))
   (*~ (/~ (~< m) wave-base~) my-freq~)
   silence))

(define my-dsp
  (-~
   (*~ (~ 220.0) (~ 440.0))
   (*~ (~ 222.0) (~ 110.0))
   (*~ (+~ (~ 110.0) (~< 2.0))
       (+~ (~ 0.05)  (~< 1.0)))))

(my-dsp 3.1234 0)

(sound:set-dsp! (live-signal 'my-dsp))

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

(define start 0.0)
(define apex 1.0)
(define frequency 440.0)

(define (make-instrument start apex frequency)
  (let ([start (live-value start)]
        [apex (live-value apex)]
        [frequency (live-value frequency)]
        [lfo (sine-wave (phase* (~< 1.5)))])
    (*~ (pan lfo) (tri-wave (phase* frequency)) (impulse start apex))))

(begin
  (define (make-instrument start apex frequency)
    (let ([start (live-value start)]
          [apex (live-value apex)]
          [frequency (live-value frequency)]
          [lfo (sine-wave (phase* (~< 220.0)))])
      (*~ (pan lfo) (live-signal 'my-dsp) (impulse start apex))
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
(sound:set-dsp! (live-signal 'my-dsp))
(sound:set-dsp! tuner)
(play-note 440.0 1/16-second)


(define my-dsp (mix (sine-wave (phase* (live-value 'frequency)))
                    (sine-wave (phase* (*~ (~< 2.0) (live-value 'frequency))))
                    (sine-wave (phase* (+~ (~< 2.0) (live-value 'frequency))))
                    (simple-osc 220.0)
                    (simple-osc 110.0)
                    (simple-osc 50.0)
                    (simple-osc 432.0)
                    ;; (simple-osc 441.0)
                    ))


(define my-dsp (make-overtone (map constant '(0.4 0.2 0.1 0.1))
                              sine-wave
                              (constant 440.0)
                              silence))

(sound:set-dsp! (live-signal 'my-dsp))
(sound:set-dsp! tuner)
(h!)

;;;;;;;

((phase (constant 220.0) silence) 0.123 0)

(define (make-overlap wave freq shift)
  (mix
   (wave (phase (constant freq) silence))
   (wave (phase (constant freq) (constant shift)))))

(define my-dsp
  (make-overlap saw-wave frequency 0.0123))
(h!)

(alias ∅ silence)
(define my-dsp (pulse-wave (phase (~< 440.0) ∅)
                           (*~ (~< 0.5)
                               (+~ (pulse-wave
                                    (phase* (~< 432.0))
                                    (simple-osc 1.0))
                                   (~< 1.0)))))
(define my-dsp (sine-wave (phase (~< 440.0)
                                 (square-wave (phase (~< 220.0) ∅)
                                             ))))
(sound:set-dsp! (live-signal 'my-dsp))
(h!)
