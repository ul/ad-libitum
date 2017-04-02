(load "soundio.ss")

(define pi 3.1415926535)

(define two-pi (* 2 pi))

(define sine (lambda (time freq)
               (sin (* two-pi freq time))))

(define square (lambda (time freq)
                 (let ([ft (* two-pi freq time)])
                   (+ (- (* 2 (floor ft))
                         (floor (* 2 ft)))
                      1))))

(define write-callback (lambda (time channel)
                         (let ([k 100]
                               [sample 0.0])
                           (do ([i 0 (+ i 1)]
                                [sample 0.0 (+ sample (sine time (+ 440.0 i)))])
                               ((= i k) (/ sample k))))))

(define square-callback (lambda (time channel)
                          (let ([k 20]
                                [sample 0.0])
                            (do ([i 0 (+ i 1)]
                                 [sample 0.0 (+ sample (square time (+ 440.0 i)))])
                                ((= i k) (/ sample k 2))))))

(define my-out (open-default-out-stream write-callback))

(start-out-stream my-out)

;; try hot swap!
;; (sound-out-write-callback-set! my-out square-callback)
