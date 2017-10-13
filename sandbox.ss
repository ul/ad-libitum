(define *bpm* 120.0)
(define my-bi-lpf (make-biquad-filter make-lpf-coefficients))
(define my-bi-hpf (make-biquad-filter make-hpf-coefficients))

(define my-lpf (cut lpf (*~ (~< 880.0) (amp2phase (osc:sine* 220.0))) <>))
(define my-lpf (∘ (cut hpf (~< 110.0) <>) (cut lpf (~< 440.0) <>)))
(define my-lpf (cut lpf (~< 110.0) <>))
(define my-lpf (cut my-bi-lpf (~< 1.0) (~< (* 4 880.0)) <>))

(begin
  (define (make-note1 frequency duration)
    (let ([frequency frequency])
      (let-values ([(start end) (fix-duration (* 0.25 duration))])
        (let ([s (*~
                  (~< 2.0)
                  ;; (make-overtone (map constant '(0.6 0.4)) ;; PLAY
                  ;;                ;; (cut osc:pulse (amp2phase (osc:sine* (~< 200.0))) <>) ;; PLAY
                  ;;                (λ (phase)
                  ;;                  (/~
                  ;;                   (osc:sine* #;frequency 880.0)
                  ;;                   (+~ (~< 2.0) (osc:sine phase))))
                  ;;                frequency
                  ;;                ∅)
                  (/~
                   (osc:sine* #;frequency 1760.0)
                   ;; noise:white
                   (+~ (~< 2.0) (osc:sine* frequency))
                   ;; (osc:sine* frequency)
                   ;; (+~ (~< 2.0) noise:white)
                   )
                  (env:impulse start end))])
          ;; (my-lpf (echo (*~ (~< 0.3) (amp2phase (osc:sine* 0.55))) (~< 0.9) s))
          (*~ s (pan (osc:sine* 440.0)))
          ;; (my-lpf (echo (~< 0.3) (~< 0.8) s))
          ;; (my-lpf (echo (~< 0.05) (~< 0.5) s))
          ;; s
          ))))

  (define-values (my-dsp1 play-note1) (make-polyphony 12 make-note1))

  (define my-dsp (my-lpf (echo (*~ (~< 0.1) (amp2phase (osc:sine* 0.001))) (~< 0.9) my-dsp1))))
;; (define my-dsp (my-lpf my-dsp1))


(begin
  (define my-pat1 '(1 0))
  (define my-pat2 '(0 1)))

(begin
  (define my-pat1 '(1 0 1 1 0 1 0))
  (define my-pat2 '(0 1 0 0))
  (define my-pat3 '(0 0))
  (define my-pat4 '(0 0)))

(define (play-my-perc)
  (let ([beat (time->beat (now) *bpm*)])
    (play-pattern my-pat1 (cut play-note1 220.0 (/ 32.0)) beat)
    (play-pattern my-pat2 (cut play-note1 110.0 (/ 32.0)) beat)
    ;; (play-pattern my-pat3 (cut play-note1 110.0 (/ 32.0)) beat)
    ;; (play-pattern my-pat4 (cut play-note2 220.0 (/ 32.0)) beat)
    )
  (*metro* 'play-my-perc))

(play-my-perc)

(sound:set-dsp! (live-signal 'my-dsp))
(h!)

;;;;
