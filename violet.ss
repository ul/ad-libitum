(load "ad-libitum.ss")

(define (simplest-oscillator time channel)
  (if (> (mod time tuner-period) tuner-half-period)
      1.0
      -1.0))
(define (pan p)
  (λ (time channel)
    (let ([p (* 0.5 (+ 1.0 (@ p)))])
      (if (zero? channel)
          (- 1.0 p)
          p))))

;; normalizing +~
(define (mix . args)
  (*~ (apply +~ args) (constant (inexact (/ (length args))))))

(define (midi2frq pitch)
  (if (<= pitch 0.0) 0.0
      (* 440.0 (expt 2.0 (/ (- pitch 69.0) 12.0)))))

(define (frq2midi freq)
  (if (<= freq 0.0) 0.0
      (+ (* 12.0 (log (/ freq 440.0) 2.0)) 69.0)))

(define (bpm2hz bpm)
  (/ 60.0 bpm))

(define (hz2bpm hz)
  (* 60.0 hz))

(define (amp2db x)
  (* 20.0 (log10 x)))

(define (db2amp x)
  (expt 10.0 (/ x 20.0)))

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
  (map (lambda (x) (* base-frequency (expt chromatic-scale-half-step (- x 1)))) scale))

;;

(define (unroll signal period sample-rate)
  (let* ([n (exact (truncate (* period sample-rate)))]
         [table (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! table i (inexact (signal (/ i sample-rate) 0))))
    (λ (phase)
      (table-wave table phase))))

;; (define table-sine-wave (unroll (simple-osc 0.1) 10 96000))

(define (range n)
  (unfold (λ (x) (<= n x))
          (λ (x) x)
          (λ (x) (+ x 1))
          0))

(define (make-overtone amplitudes wave frequency phase0)
  (apply ∑
   (map
    (λ (amplitude factor)
      (*~ amplitude
          (wave (phase (*~ (constant factor) frequency) phase0))))
    amplitudes
    (range (length amplitudes)))))
(define (simple-instrument start end freq a d s r)
  (let* ([start (live-constant start)]
         [end (live-constant end)]
         [freq (live-constant freq)]
         [osc (sine-wave (phase* freq))]
         [env (adsr start
                    end
                    (constant a)
                    (constant d)
                    (constant s)
                    (constant r))])
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
