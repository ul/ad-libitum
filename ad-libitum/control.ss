(library (ad-libitum control (1))
  (export make-control)
  (import (chezscheme)
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

  )
