(library (ad-libitum control (1))
  (export make-control window)
  (import (chezscheme)
          (srfi s42 eager-comprehensions)
          (ad-libitum common)
          (ad-libitum signal)
          )
  ;; <control-signal>
  (define make-control
    (case-lambda
      [(x)
       (if (number? x)
           (make-control id x)
           (make-control x 0.0))]
      [(f x)
       (values (~< x) (λ args (set! x (apply f args))))]))
  ;; </control-signal>

  ;; <window>
  (define (window width signal)
    (let ([windows (make-vector *channels*)]
          [N (-> width (* *sample-rate*) (ceiling) (exact))]
          [cursor -1])
      (do-ec (: i *channels*)
             (vector-set! windows i (make-vector N 0.0)))
      (values
       (~<
        (when (zero? channel)
          (set! cursor (mod (+ cursor 1) N)))
        (let ([sample (<~ signal)]
              [window (channel-ref windows)])
          (vector-set! window cursor sample)
          sample))
       (λ () windows))))
  ;; </window>

  )
