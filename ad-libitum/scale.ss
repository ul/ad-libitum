(library (ad-libitum scale (1))
  (export chromatic-scale-half-step
          second-interval
          third-interval
          perfect-fourth-interval
          perfect-fifth-interval
          major-sixth-interval
          major-seventh-interval
          perfect-octave-interval
          minor-second-interval
          minor-third-interval
          minor-second-interval
          triton-interval
          chromatic-scale
          pentatonic-scale
          major-scale
          minor-scale
          make-scale
          )
  (import (chezscheme)
          (ad-libitum common))
  ;; <intervals>
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
  ;; </intervals>
  ;; <scales>
  (define chromatic-scale '(1 2 3 4 5 6 7 8 9 10 11 12))
  (define pentatonic-scale '(1 3 5 8 10))
  (define major-scale '(1 3 5 6 8 10 12))
  (define minor-scale '(1 3 4 6 8 9 11))
  
  (define (make-scale base-frequency scale)
    (map (λ (x) (* base-frequency (expt chromatic-scale-half-step (- x 1)))) scale))
  ;; </scales>
  )
