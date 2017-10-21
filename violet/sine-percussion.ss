;;; perf sake

(define~ (simple-sine frequency)
  (if (zero? channel)
    (sin (* 2π frequency time))
    0.0))

(define~ (simple-sine* frequency)
  (if (zero? channel)
      (sin (* 2π (<~ frequency) time))
      0.0))

(define~ (simple-tri frequency)
  (if (zero? channel)
      (let ([phase (mod (* frequency time) 1.0)])
        (if (< phase 0.5)
            (- (* 4.0 phase) 1.0)
            (+ (* -4.0 phase) 3.0)))
      0.0))

(define (stereo signal)
  (let ([x 0.0])
    (~<
     (when (zero? channel)
       (set! x (<~ signal)))
     x)))

;;; /perf sake

(define (make-kick-note frequency duration)
  (let-values ([(start end) (fix-duration (* 0.25 duration))])
    (*~
     (simple-sine frequency)
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

(play! (mix kick~ snare~))

(kick!)
(snare!)

(define pat1 '(1 0 0 1))
(define pat2 '(0 0 1 0 1))

(define (test-perc)
  (play-pattern pat1 kick! (*beat*))
  (play-pattern pat2 snare! (*beat*))
  (*metro* 'test-perc))

(test-perc)

(set-bpm! 120.0)

(define frequencies (make-vector 16 0.0))
(define step (* 0.02 (+ 6 (random 3))))
(define frequencies~
  (map (λ (i) (~< (vector-ref frequencies i)))
       (iota (vector-length frequencies))))
(define sinusoids (map simple-sine* frequencies~))
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

(play! (stereo (mix kick~ snare~ curve)))

;;;;

(define (make-snare-note frequency duration)
  (let-values ([(start end) (fix-duration (* 0.25 duration))])
    (*~
     (simple-tri frequency)
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

(define-values (feedback set-feedback!) (ctrl:make-control 0.0))
(define-values (dly set-delay!) (ctrl:make-control 0.0))

(define fb (env:linear-transition (~< 15.0) feedback))
;; (define dl (env:linear-transition (~< 15.0) dly))
(define dl (filter:echo* (~< 0.03) (~< 0.1) (env:linear-transition (~< 15.0) dly)))

;; (play! (*~ (~< 0.5) (stereo (filter:echo* dl fb (mix kick~ snare~ curve)))))
(play! (*~ (stereo (filter:echo* dl fb (mix kick~ snare~)))))

(set-bpm! 260.0)

(set-feedback! 0.9)
(set-feedback! 0.5)
(set-feedback! 0.1)

(set-delay! step)
(set-delay! (* 0.1 step))
(set-delay! (* 0.5 step))
(set-delay! (* 2.0 step))
(set-delay! (* 20.0 step))

(set-delay! 0.01)
(set-delay! 0.03)
(set-delay! 0.05)
(set-delay! 0.1)
(set-delay! 0.5)
(set-delay! 1.0)
(set-delay! 5.0)

(define (change-delay)
  (set-delay! (+ 0.01 (random 5.0)))
  (schedule (+ (now) 15.0) 'change-delay))

(change-delay)

(define change-delay 0)
(define test-perc 0)
(define play-percussion 0)
(define change-pattern 0)

(h!)
