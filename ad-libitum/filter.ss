(library (ad-libitum filter (1))
  (export delay
          echo
          lpf
          hpf
          biquad-lpf
          biquad-hpf)
  (import (except (chezscheme) delay)
          (ad-libitum common)
          (ad-libitum signal))
  ;; <delay>
  (define~ (delay Δt f)
    (f (- time (<~ Δt)) channel))
  ;; </delay>
  ;; <echo>
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
  ;; </echo>
  ;; <lpf>
  (define (lpf-frequency->α frequency)
    (let ([k (* frequency *sample-angular-period*)])
      (/ k (+ k 1))))
  
  (define (lpf frequency x)
    (let ([ys (make-vector *channels* 0.0)])
      (~<
       (let* ([y-1 (vector-ref ys channel)]
              [α (lpf-frequency->α (<~ frequency))])
         (let ([y (+ y-1 (* α (- (<~ x) y-1)))])
           (vector-set! ys channel y)
           y)))))
  ;; </lpf>
  ;; <hpf>
  (define (hpf-frequency->α frequency)
    (let ([k (* frequency *sample-angular-period*)])
      (/ (+ k 1))))
  
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
  ;; </hpf>
  ;; <make-biquad-filter>
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
  ;; </make-biquad-filter>
  ;; <biquad-lpf>
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
  
  (define biquad-lpf (make-biquad-filter make-lpf-coefficients))
  ;; </biquad-lpf>
  ;; <biquad-hpf>
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
  
  (define biquad-hpf (make-biquad-filter make-hpf-coefficients))
  ;; </biquad-hpf>
  )
