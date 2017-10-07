(define~ (amp2phase s)
  (* 0.5 (+ 1.0 (<~ s))))

(define start 0.0)
(define start~ (live-value 'start))
(define apex 1.0)
(define apex~ (live-value 'apex))
(define frequency 440.0)
(define frequency~ (live-value 'frequency))

(define my-dsp
  (*~
   (pan (osc:sine* 5000.0)) ;; PLAY
   (make-overtone (map constant '(0.2 0.1 0.2 0.1 0.4)) ;; PLAY
                  (cut osc:pulse (amp2phase (osc:sine* 200.0)) <>) ;; PLAY
                  frequency~
                  silence)))

(define inst (*~ (live-signal 'my-dsp) (env:impulse start~ apex~)))

(define (play-note freq dur)
  (set! frequency freq)
  (set! start (now))
  (set! apex (+ (now) dur)))

(define (be-playful-with-frequency)
  (play-note (random-choice (make-scale 22000.0 pentatonic-scale)) ;; PLAY
             (random-choice (list second half-second)))
  (schedule
   (+ (now)
      (random-choice
       (list
        (* 2.0 second)
        second
        second
        half-second
        half-second
        half-second
        half-second
        )))
   'be-playful-with-frequency))

(be-playful-with-frequency)

(sound:set-dsp! (live-signal 'inst))
(h!)
