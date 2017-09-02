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

(define pi (* (asin 1.0) 2))
(define +pi   3.14159265358979323846264)
(define +pi/2 1.57079632679489661923132)
(define +pi/4  .78539816339744830961566)
(define -pi (- +pi))
(define -pi/2 (- +pi/2))
(define -pi/4 (- +pi/4))
(define two-pi (* 2 pi))

(define (sine time freq)
  (sin (* two-pi freq time)))

(define (tuner time channel)
  ;; inexact because otherwise exact 0 would crash soundio
  (inexact (sine time 440.0)))

(sound:start)
(scheduler:start)

(define schedule scheduler:schedule)
(define callback schedule)

(repl:start-repl-server)

;; try in remote repl
;; (sound:set-dsp! tuner)
