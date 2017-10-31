(define (make-kick-note frequency duration)
  (let-values ([(start end) (fix-duration (* 0.25 duration))])
    (*~
     (osc:sine/// frequency)
     (env:impulse start end))))

(define-values (kick~ play-kick) (inst:make-polyphony 4 make-kick-note))

(define (kick!) (play-kick 80.0 1/16))

(play! kick~)

(kick!)

(define (make-snare-note frequency duration)
  (let-values ([(start end) (fix-duration (* 0.25 duration))])
    (filter:biquad-hpf
     (~< 1.0)
     (~< frequency)
     (*~
      noise:white
      (env:impulse start end)))))

(define-values (snare~ play-snare) (inst:make-polyphony 4 make-snare-note))

(define (snare!) (play-snare 2000.0 1/32))

(play! (mix kick~ (*~ (~< 0.3) snare~)))

(kick!)
(snare!)

(define pat1 '(1 0 0 1 1))
(define pat2 '(0 0 1 0 1))

(define (test-perc)
  (play-pattern pat1 kick! (*beat*))
  (play-pattern pat2 snare! (*beat*))
  (*metro* 'test-perc))

(test-perc)

(set-bpm! 260.0)
(set! test-perc id)

(define frequencies (make-vector 16 0.0))
(define step (* 0.02 (+ 6 (random 3))))
(define frequencies~
  (map (λ (i) (~< (vector-ref frequencies i)))
       (iota (vector-length frequencies))))
(define sinusoids (map osc:sine/// frequencies~))
(define curve* (∑ sinusoids))
(define f 0.0)
(define~ curve
  (set! f (abs (<~ curve*)))
  0.0)

(define (change-pattern)
  (do-ec (: i (vector-length frequencies))
         (vector-set! frequencies i (random 8.0)))
  (set! step (* 0.03 (+ 6 (random 3))))
  (schedule (+ (now) 30.0) 'change-pattern))

(change-pattern)

(define (kick2!) (play-kick 110.0 1/32))
(define (snare2!) (play-snare 880.0 1/32))
(define (kick3!) (play-kick 220.0 1/8))
(define (snare3!) (play-snare 2000.0 1/32))
(define instruments
  (reverse (list kick! snare! kick2! snare2! kick! snare! kick3! snare3!)))

(define (play-percussion)
  (for-each
   (λ (instrument i)
     (when (and (> f (/ i 6.0))
                (< f (/ (+ i 1) 5.0)))
       (instrument)))
   instruments
   (iota (length instruments)))
  (schedule (+ (now) step) 'play-percussion))

(play-percussion)

(quick-test curve)

(play! (mono (mix kick~ snare~ curve)))

;;;;

(define (make-snare-note frequency duration)
  (let-values ([(start end) (fix-duration (* 0.25 duration))])
    (*~
     (osc:tri/// frequency)
     (env:impulse start end))))

(define-values (snare~ play-snare) (inst:make-polyphony 4 make-snare-note))

(define (kick!) (play-kick 55.0 1/16))
(define (snare!) (play-snare 220.0 1/16))
(define (kick2!) (play-kick 110.0 1/16))
(define (snare2!) (play-snare 440.0 1/16))
(define (kick3!) (play-kick 220.0 1/16))
(define (snare3!) (play-snare 880.0 1/16))
(define instruments
  (list kick! snare! kick2! snare2! kick! snare! kick3! snare3!))

(ctrl:define-control feedback 0.0)
(ctrl:define-control delay 0.0)

(define fb (env:linear-transition (~< 15.0) feedback~))
(define dl (filter:echo* (~< 0.03) (~< 0.1) (env:linear-transition (~< 15.0) delay~)))

;; (play! (*~ (~< 0.5) (mono (filter:echo* dl fb (mix kick~ snare~ curve)))))
(play! (mono (filter:echo* dl fb (mix kick~ snare~))))
(test-perc)
(set! test-perc id)

(set-bpm! 260.0)

(feedback-set! 0.9)
(feedback-set! 0.5)
(feedback-set! 0.1)

(delay-set! step)
(delay-set! (* 0.1 step))
(delay-set! (* 0.5 step))
(delay-set! (* 2.0 step))
(delay-set! (* 20.0 step))

(delay-set! 0.01)
(delay-set! 0.03)
(delay-set! 0.05)
(delay-set! 0.1)
(delay-set! 0.5)
(delay-set! 1.0)
(delay-set! 5.0)

(define (change-delay)
  (delay-set! (+ 0.01 (random 5.0)))
  (schedule (+ (now) 15.0) 'change-delay))

(change-delay)

(define change-delay 0)
(define test-perc 0)
(define play-percussion 0)
(define change-pattern 0)

(h!)
