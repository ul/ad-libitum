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
(define (simplest-oscillator time channel)
  (if (> (mod time tuner-period) tuner-half-period)
      1.0
      -1.0))
