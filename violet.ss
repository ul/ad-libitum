(load "ad-libitum.ss")

(define (simplest-oscillator time channel)
  (if (> (mod time tuner-period) tuner-half-period)
      1.0
      -1.0))
(define second 1.0)
(define half-second 0.5)
(define quarter-second 0.25)
(define 1/8-second 0.125)
(define 1/16-second 0.0625)
(define 1/32-second 0.03125)

(define (random-choice list)
  (list-ref list (random (length list))))

(define chromatic-scale-half-step
  (expt 2 1/12))

(define second-interval (expt chromatic-scale-half-step 2))
(define third-interval (expt chromatic-scale-half-step 4))
(define perfect-fourth-interval (expt chromatic-scale-half-step 5))
(define perfect-fifth-interval (expt chromatic-scale-half-step 7))
(define major-sixth-interval (expt chromatic-scale-half-step 9))
(define major-seventh-interval (expt chromatic-scale-half-step 11))
(define perfect-octave-interval (expt chromatic-scale-half-step 12))
(define minor-second-interval (expt chromatic-scale-half-step 1))
(define minor-third-interval (expt chromatic-scale-half-step 3))
(define minor-sixth-interval (expt chromatic-scale-half-step 8))
(define minor-seventh-interval (expt chromatic-scale-half-step 11))
(define triton-interval (expt chromatic-scale-half-step 11))

;; TODO excercise: represent scales as whole/half steps

(define chromatic-scale '(1 2 3 4 5 6 7 8 9 10 11 12))
(define pentatonic-scale '(1 3 5 8 10))
(define major-scale '(1 3 5 6 8 10 12))
(define minor-scale '(1 3 4 6 8 9 11))

(define (make-scale base-frequency scale)
  (map (λ (x) (* base-frequency (expt chromatic-scale-half-step (- x 1)))) scale))

;;

(define (unroll signal period sample-rate)
  (let* ([n (exact (truncate (* period sample-rate)))]
         [table (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! table i (inexact (signal (/ i sample-rate) 0))))
    (cut sampler table <>)))

;; (define table-sine-wave (unroll (simple-osc 0.1) 10 96000))

(define (range n)
  (list-ec (: i n) i))

(define (make-overtone amplitudes wave frequency phase0)
  (∑ (map
      (λ (amplitude factor)
        (*~ amplitude
            (wave (osc:phasor (*~ (~< factor) frequency) phase0))))
      amplitudes
      (range (length amplitudes)))))

(define (fix-duration duration)
  (let* ([start (now)]
         [end (+ start duration)])
    (values (~< start) (~< end))))

(define~ (amp2phase s)
  (* 0.5 (+ 1.0 (<~ s))))

(define (make-polyphony n make-voice)
  (let ([voices (make-vector n ∅)]
        [cursor 0])
    (let ([signal
           (apply mix (list-ec (: i n) (~< (<~ (vector-ref voices i)))))]
          [play-note
           (λ args
             (let ([voice (apply make-voice args)])
               (vector-set! voices cursor voice)
               (set! cursor (mod (+ cursor 1) n))
               voice))])
      (values signal play-note))))
(define (simple-instrument start end freq a d s r)
  (let* ([start (live-value start)]
         [end (live-value end)]
         [freq (live-value freq)]
         [osc (sine-wave (phasor freq))]
         [env (adsr start end (~< a) (~< d) (~< s) (~< r))])
    (*~ env osc)))

(define (make-play-note start end frequency)
  (λ (freq dur)
    (set-top-level-value! frequency freq)
    (set-top-level-value! start (now))
    (set-top-level-value! end (+ (now) dur))))

;; (define start 0.0)
;; (define end 1.0)
;; (define frequency 440.0)

;; (define inst (simple-intrument 'start 'end 'frequency 0.3 0.5 0.8 1.0))
;; (define play-note (make-play-note 'start 'end 'frequency))

;; (sound:set-dsp! (live-signal 'inst))
;; (play-note 440.0 1.1)
