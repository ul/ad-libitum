(alias ! constant)
(alias var~ live-constant)

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
   (+ (now)
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

;;;;;;;;;;;;;;;;;

(define start 0.0)
(define end 1.0)
(define frequency 440.0)

(define inst (simple-intrument 'start 'end 'frequency 0.05 0.01 0.4 0.1))
(define play-note (make-play-note 'start 'end 'frequency))

(sound:set-dsp! (live-signal 'inst))
(play-note 440.0 second)

(define (be-playful-with-frequency)
  (play-note (random-choice (make-scale 440.0 pentatonic-scale))
             (random-choice
              (list
               ;; second
               ;; half-second
               quarter-second
               quarter-second
               quarter-second
               quarter-second
               1/8-second
               1/8-second
               1/8-second
               1/8-second
               ;; 1/16-second
               ;; 1/16-second
               ;; 1/16-second
               ;; 1/16-second
               ;; 1/32-second
               ;; 1/32-second
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
