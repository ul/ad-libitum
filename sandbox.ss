(define *bpm* 260.0)

(define lpf (∘ simple-lpf simple-lpf simple-lpf simple-lpf))

(begin
  (define (make-note1 frequency duration)
    (let ([frequency (~< frequency)])
      (let-values ([(start end) (fix-duration (* 0.25 duration))])
        (let ([s (*~
                  (osc:sine* frequency)
                  (env:impulse start end)
                  )])
          (lpf (feedback (*~ (~< 0.3) (amp2phase (osc:sine* 1.0))) (~< 0.8) s))
          ;; (lpf (feedback (~< 0.05) (~< 0.8) s))
          ))))

  (define-values (my-dsp1 play-note1) (make-polyphony 4 make-note1))

  (define my-dsp my-dsp1))

(begin
  (define my-pat1 '(1 0 0 0 1 0 1 0))
  (define my-pat2 '(0 1 0 0)))

(begin
  (define my-pat1 '(1 0 1 1 0 1 0))
  (define my-pat2 '(0 1 0 0))
  (define my-pat3 '(0 0))
  (define my-pat4 '(0 0)))

(define (play-my-perc)
  (let ([beat (time->beat (now) *bpm*)])
    (play-pattern my-pat1 (cut play-note1 220.0 (/ 32.0)) beat)
    ;; (play-pattern my-pat2 (cut play-note2 220.0 (/ 32.0)) beat)
    ;; (play-pattern my-pat3 (cut play-note1 110.0 (/ 32.0)) beat)
    ;; (play-pattern my-pat4 (cut play-note2 220.0 (/ 32.0)) beat)
    )
  (*metro* 'play-my-perc))

(play-my-perc)

(sound:set-dsp! (live-signal 'my-dsp))
(h!)
