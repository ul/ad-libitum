(library (ad-libitum filter (1))
  (export delay
          echo echo*
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
  (define *max-line-duration-slow* 10)
  (define *max-line-duration-fast* 1)
  
  (define (make-echo max-line-duration)
    (λ (delay feedback signal)
      (let ([line-size (* max-line-duration *sample-rate*)]
            [lines (make-channel-vector)]
            [cursor -1])
        (do ([channel 0 (+ channel 1)])
            ((= channel *channels*) 0)
          (channel-set! lines (make-vector line-size 0.0)))
        (~<
         (when(zero? channel)
           (set! cursor (mod (+ cursor 1) line-size)))
         (let ([line (channel-ref lines)]
               [x (<~ signal)]
               [delay (flonum->fixnum (round (* (<~ delay) *sample-rate*)))]
               [feedback (<~ feedback)])
           (let* ([i (mod (+ line-size (- cursor delay)) line-size)]
                  [y (vector-ref line i)]
                  [z (+ x (* feedback y))])
             (vector-set! line cursor z)
             z))))))
  
  (define echo (make-echo *max-line-duration-fast*))
  (define echo* (make-echo *max-line-duration-slow*))
  ;; </echo>

  ;; <lpf>
  (define (lpf-frequency->α frequency)
    (let ([k (* frequency *sample-angular-period*)])
      (/ k (+ k 1))))
  
  (define (lpf frequency x)
    (let ([ys (make-channel-vector 0.0)])
      (~<
       (let* ([y-1 (channel-ref ys)]
              [α (lpf-frequency->α (<~ frequency))])
         (let ([y (+ y-1 (* α (- (<~ x) y-1)))])
           (channel-set! ys y)
           y)))))
  ;; </lpf>

  ;; <hpf>
  (define (hpf-frequency->α frequency)
    (let ([k (* frequency *sample-angular-period*)])
      (/ (+ k 1))))
  
  (define (hpf frequency x)
    (let ([xs (make-channel-vector 0.0)]
          [ys (make-channel-vector 0.0)])
      (~<
       (let ([x-1 (channel-ref xs)]
             [y-1 (channel-ref ys)]
             [x (<~ x)]
             [α (hpf-frequency->α (<~ frequency))])
         (let ([y (* α (+ y-1 (- x x-1)))])
           (channel-set! xs x)
           (channel-set! ys y)
           y)))))
  ;; </hpf>

  ;; <make-biquad-filter>
  (define (make-biquad-filter make-coefficients)
    (λ (Q frequency x)
      (let ([xs-1 (make-channel-vector 0.0)]
            [xs-2 (make-channel-vector 0.0)]
            [ys-1 (make-channel-vector 0.0)]
            [ys-2 (make-channel-vector 0.0)])
        (~<
         (let ([x-1 (channel-ref xs-1)]
               [x-2 (channel-ref xs-2)]
               [y-1 (channel-ref ys-1)]
               [y-2 (channel-ref ys-2)]
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
                 (channel-set! xs-1 x)
                 (channel-set! xs-2 x-1)
                 (channel-set! ys-1 y)
                 (channel-set! ys-2 y-1)
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
