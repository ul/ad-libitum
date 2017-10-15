(set-bpm! 120.0)

(define my-lpf (cut filter:lpf (*~ (~< 880.0) (amplitude->phase (osc:sine/// 220.0))) <>))
(define my-lpf (∘ (cut filter:hpf (~< 110.0) <>) (cut filter:lpf (~< 440.0) <>)))
(define my-lpf (cut filter:lpf (~< 110.0) <>))
(define my-lpf (cut filter:biquad-lpf (~< 1.0) (~< (* 4 880.0)) <>))

(begin
  (define (make-note1 frequency duration)
    (let ([frequency frequency])
      (let-values ([(start end) (fix-duration (* 0.25 duration))])
        (let ([s (*~
                  (~< 2.0)
                  ;; (make-overtone (map constant '(0.6 0.4)) ;; PLAY
                  ;;                ;; (cut osc:pulse (amplitude->phase (osc:sine/// (~< 200.0))) <>) ;; PLAY
                  ;;                (λ (phase)
                  ;;                  (/~
                  ;;                   (osc:sine/// #;frequency 880.0)
                  ;;                   (+~ (~< 2.0) (osc:sine phase))))
                  ;;                frequency
                  ;;                ∅)
                  (/~
                   (osc:sine/// #;frequency 1760.0)
                   ;; noise:white
                   (+~ (~< 2.0) (osc:sine/// frequency))
                   ;; (osc:sine/// frequency)
                   ;; (+~ (~< 2.0) noise:white)
                   )
                  (env:impulse start end))])
          ;; (*~ s (pan (osc:sine/// 440.0)))
          ;; (my-lpf (filter:echo (~< 0.3) (~< 0.8) s))
          (my-lpf (filter:echo (~< 0.05) (~< 0.5) s))
          ;; s
          ))))

  (define-values (my-dsp1 play-note1) (inst:make-polyphony 12 make-note1))

  (define my-dsp (my-lpf (filter:echo (*~ (~< 0.1) (amplitude->phase (osc:sine/// 0.001))) (~< 0.9) my-dsp1))))
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
  (let ([beat (*beat*)])
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
