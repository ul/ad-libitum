(load "ad-libitum.ss")

(define (simplest-oscillator time channel)
  (if (> (mod time tuner-period) tuner-half-period)
      1.0
      -1.0))
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

(define (make-overtone amplitudes wave frequency phase0)
  (∑ (map
      (λ (amplitude factor)
        (*~ amplitude
            (wave (osc:phasor (*~ (~< factor) frequency) phase0))))
      amplitudes
      (iota (length amplitudes)))))

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

(define (make-static-polyphony n make-voice)
  ;; (make-voice) -> (list signal play-note)
  (let ([voices (list-ec (: i n) (make-voice))]
        [cursor 0])
    (let ([signal (apply mix (map first voices))]
          [play-note
           (λ args
             (apply (second (vector-ref voices cursor)) args)
             (set! cursor (mod (+ cursor 1) n)))])
      (values signal play-note))))

(define (time->beat time bpm)
  (-> time (* bpm) (/ 60) (round)))

(define (beat->time beat bpm)
  (-> beat (* 60) (/ bpm)))

(define (next-beat time bpm)
  (beat->time (+ 1 (time->beat time bpm)) bpm))

(define (metro bpm . args)
  (apply schedule (next-beat (now) bpm) args))

(define *bpm* 60.0)

(define (*metro* . args)
  (apply metro *bpm* args))

(define (play-pattern pattern sound beat)
  (let ([n (length pattern)])
    (when (positive? (list-ref pattern (mod (exact beat) n)))
      (sound))))

(define~ (delay t s)
  (let ([t (<~ t)])
    (s  (- time t) channel)))

(define *max-line-duration* 1)

(define (echo delay feedback signal)
  (let ([line-size (* *max-line-duration* *sample-rate*)]
        [lines (make-vector *channels*)]
        [cursor -1])
    (do ([i 0 (+ i 1)])
        ((= i *channels*) 0)
      (vector-set! lines i (make-vector line-size 0.0)))
    (~<
     (when(zero? channel)
       (set! cursor (mod (+ cursor 1) line-size)))
     (let ([line (vector-ref lines channel)]
           [x (<~ signal)]
           [delay (flonum->fixnum (round (* (<~ delay) *sample-rate*)))]
           [feedback (<~ feedback)])
       (let* ([i (mod (+ line-size (- cursor delay)) line-size)]
              [y (vector-ref line i)]
              [z (+ x (* feedback y))])
         (vector-set! line cursor z)
         z)))))

(define (lpf-frequency->α frequency)
  (let ([k (* frequency *sample-angular-period*)])
    (/ k (+ k 1))))

(define (hpf-frequency->α frequency)
  (let ([k (* frequency *sample-angular-period*)])
    (/ (+ k 1))))

(define (lpf frequency x)
  (let ([ys (make-vector *channels* 0.0)])
    (~<
     (let* ([y-1 (vector-ref ys channel)]
            [α (lpf-frequency->α (<~ frequency))])
       (let ([y (+ y-1 (* α (- (<~ x) y-1)))])
         (vector-set! ys channel y)
         y)))))

(define (hpf frequency x)
  (let ([xs (make-vector *channels* 0.0)]
        [ys (make-vector *channels* 0.0)])
    (~<
     (let ([x-1 (vector-ref xs channel)]
           [y-1 (vector-ref ys channel)]
           [x (<~ x)]
           [α (hpf-frequency->α (<~ frequency))])
       (let ([y (* α (+ y-1 (- x x-1)))])
         (vector-set! xs channel x)
         (vector-set! ys channel y)
         y)))))

(define (make-lpf-coefficients sin-ω cos-ω α)
  (let ([b0 (* 0.5 (- 1.0 cos-ω))])
    (values
     b0             ;; b0
     (- 1.0 cos-ω)  ;; b1
     b0             ;; b2
     (+ 1.0 α)      ;; a0
     (* -2.0 cos-ω) ;; a1
     (- 1.0 α)      ;; a2
     )))

(define (make-hpf-coefficients sin-ω cos-ω α)
  (let ([b0 (* 0.5 (+ 1.0 cos-ω))])
    (values
     b0             ;; b0
     (- -1.0 cos-ω) ;; b1
     b0             ;; b2
     (+ 1.0 α)      ;; a0
     (* -2.0 cos-ω) ;; a1
     (- 1.0 α)      ;; a2
     )))

(define (make-biquad-filter make-coefficients)
  (λ (Q frequency x)
    (let ([xs-1 (make-vector *channels* 0.0)]
          [xs-2 (make-vector *channels* 0.0)]
          [ys-1 (make-vector *channels* 0.0)]
          [ys-2 (make-vector *channels* 0.0)])
      (~<
       (let ([x-1 (vector-ref xs-1 channel)]
             [x-2 (vector-ref xs-2 channel)]
             [y-1 (vector-ref ys-1 channel)]
             [y-2 (vector-ref ys-2 channel)]
             [x (<~ x)]
             [Q (<~ Q)]
             [frequency (<~ frequency)])
         (let* ([ω (* frequency *sample-angular-period*)]
                [sin-ω (sin ω)]
                [cos-ω (cos ω)]
                [α (/ sin-ω (* 2.0 Q))])
           (let-values ([(b0 b1 b2 a0 a1 a2) (make-coefficients sin-ω cos-ω α)])
             (let ([y (-
                       (+
                        (* (/ b0 a0) x)
                        (* (/ b1 a0) x-1)
                        (* (/ b2 a0) x-2))
                       (* (/ a1 a0) y-1)
                       (* (/ a2 a0) y-2))])
               (vector-set! xs-1 channel x)
               (vector-set! xs-2 channel x-1)
               (vector-set! ys-1 channel y)
               (vector-set! ys-2 channel y-1)
               y))))))))
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
