(define (add-library-directory dir)
  (library-directories
   (cons dir (library-directories))))

(define (add-library-directories . dirs)
  (unless (null? dirs)
    (add-library-directory (car dirs))
    (apply add-library-directories (cdr dirs))))

(add-library-directories
 "./chez-soundio"
 "./chez-sockets")
(import (chezscheme)
        (prefix (sound) sound:)
        (prefix (scheduler) scheduler:)
        (prefix (repl) repl:))
(define now scheduler:now)
(define schedule scheduler:schedule)
(define callback schedule)

(sound:start)
(scheduler:start)
(repl:start-repl-server)
(define pi (* (asin 1.0) 2))
(define +pi   3.14159265358979323846264)
(define +pi/2 1.57079632679489661923132)
(define +pi/4  .78539816339744830961566)
(define -pi (- +pi))
(define -pi/2 (- +pi/2))
(define -pi/4 (- +pi/4))
(define two-pi (* 2 pi))
(define tuner-frequency 440.0)
(define tuner-period (/ 1.0 tuner-frequency))
(define tuner-half-period (/ tuner-period 2.0))
(define (sine time freq)
  (sin (* two-pi freq time)))

(define (tuner time channel)
  ;; inexact because otherwise exact 0 would crash soundio
  (inexact (sine time tuner-frequency)))

;; (sound:set-dsp! tuner)
(define (constant amplitude)
  (lambda (time channel)
    amplitude))
(define silence (constant 0.0))
(extend-syntax (@) [(@ signal) (signal time channel)])
(define (phase frequency phase0)
  (let ([previous-time 0.0]
        [previous-phase 0.0])
    (lambda (time channel)
      (let* ([time-delta (- time previous-time)]
             [phase-delta (* time-delta (@ frequency))]
             [next-phase (mod (+ previous-phase phase-delta (@ phase0)) 1.0)])
        (set! previous-time time)
        (set! previous-phase next-phase)
        next-phase))))

(define (phase* frequency)
  (phase frequency (constant 0.0)))
(define (sine-wave phase)
  (lambda (time channel)
    (inexact (sin (* two-pi (@ phase))))))

(define (cosine-wave phase)
  (lambda (time channel)
    (inexact (cos (* two-pi (@ phase))))))

(define (square-wave phase)
  (lambda (time channel)
    (if (< (@ phase) 0.5)
        1.0
        -1.0)))

(define (tri-wave phase)
  (lambda (time channel)
    (let ([phase (@ phase)])
      (if (< phase 0.5)
          (- (* 4.0 phase) 1.0)
          (+ (* -4.0 phase) 3.0)))))

(define (saw-wave phase)
  (lambda (time channel)
    (- (* 2.0 (@ phase)) 1.0)))

(define (table-wave table phase)
  (let ([n (vector-length table)])
    (lambda (time channel)
      (vector-ref table (exact (truncate (* (@ phase) n)))))))

(define (random-amplitude)
  (- (random 2.0) 1.0))

(define (random-wave time channel)
  (random-amplitude))
(define (live-signal symbol)
  (lambda (time channel)
    ((top-level-value symbol) time channel)))
(define (signal-sum . xs)
  (lambda (time channel)
    (fold-left (lambda (a x) (+ a (@ x))) 0.0 xs)))

(define (signal-mult . xs)
  (lambda (time channel)
    (fold-left (lambda (a x) (* a (@ x))) 1.0 xs)))

(define (signal-diff x . xs)
  (let ([y (apply signal-sum xs)])
    (lambda (time channel)
      (- (@ x) (@ y)))))

(define (signal-div x . xs)
  (let ([y (apply signal-mult xs)])
    (lambda (time channel)
      (/ (@ x) (@ y)))))

(define +~ signal-sum)
(define *~ signal-mult)
(define -~ signal-diff)
(define /~ signal-div)
