(define start 0.0)
(define start~ (live-value 'start))
(define apex 1.0)
(define apex~ (live-value 'apex))
(define frequency 440.0)
(define frequency~ (live-value 'frequency))

(define my-dsp
  (*~
   (pan (osc:sine/// 5000.0)) ;; PLAY
   (make-overtone (map constant '(0.2 0.1 0.2 0.1 0.4)) ;; PLAY
                  (cut osc:pulse (amplitude->phase (osc:sine/// 200.0)) <>) ;; PLAY
                  frequency~
                  silence)))

(define inst (*~ (live-signal 'my-dsp) (env:impulse start~ apex~)))

(define (play-note freq dur)
  (set! frequency freq)
  (set! start (now))
  (set! apex (+ (now) dur)))

(play-note 440.0 1/2)
(inst 0.11123 0)

(define (be-playful-with-frequency)
  (play-note (random-choice (scale:make-scale 22000.0 scale:pentatonic-scale)) ;; PLAY
             (random-choice (list 1 1/2)))
  (schedule
   (+ (now)
      (random-choice
       (list
        2
        1
        1
        1/2
        1/2
        1/2
        1/2
        )))
   'be-playful-with-frequency))

(be-playful-with-frequency)

(play! (live-signal 'inst))
(h!)
