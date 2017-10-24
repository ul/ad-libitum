(load "ad-libitum.ss")

;; <sandbox>
(define (make-overtone amplitudes wave frequency phase0)
  (∑ (map
      (λ (amplitude factor)
        (let ([factor (inexact factor)])
          (*~ amplitude
              (wave (osc:phasor (*~ (~< factor) frequency) phase0)))))
      amplitudes
      (iota (length amplitudes)))))

(define (fix-duration duration)
  (let* ([start (now)]
         [end (+ start duration)])
    (values (~< start) (~< end))))

(define~ (amplitude->phase s)
  (* 0.5 (+ 1.0 (<~ s))))
;; </sandbox>

;; <simplest-oscillator>
(define (simplest-oscillator time channel)
  (if (> (mod time tuner-period) tuner-half-period)
      1.0
      -1.0))
;; </simplest-oscillator>

;; <play-note>
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
;; </play-note>

