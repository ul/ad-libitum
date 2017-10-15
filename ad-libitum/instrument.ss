(library (ad-libitum instrument (1))
  (export make-polyphony)
  (import (chezscheme)
          (only (srfi s1 lists) first second)
          (srfi s42 eager-comprehensions)
          (ad-libitum common)
          (ad-libitum signal))
  ;; <polyphony>
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
  ;; </polyphony>

  )
