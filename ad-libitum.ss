(define (add-library-directory dir)
  (library-directories
   (cons dir (library-directories))))

(define (add-library-directories . dirs)
  (unless (null? dirs)
    (add-library-directory (car dirs))
    (apply add-library-directories (cdr dirs))))

(add-library-directories
 "./chez-soundio"
 "./chez-portmidi"
 "./chez-sockets")

(import (ad-libitum common))

(voodoo)

(import (chezscheme)
        (srfi s1 lists)
        (srfi s26 cut)
        (srfi s42 eager-comprehensions)
        (ad-libitum signal)
        (prefix (ad-libitum oscillator) osc:)
        (prefix (ad-libitum envelope) env:)
        (prefix (ad-libitum noise) noise:)
        (prefix (ad-libitum sound) sound:)
        (prefix (ad-libitum scheduler) scheduler:)
        (prefix (ad-libitum repl) repl:)
        (prefix (ad-libitum midi) midi:)
        )

(sound:start)

(define *sound-start-time* (current-time))

(define (now)
  (let ([Δt (time-difference (current-time) *sound-start-time*)])
    (inexact (+ (time-second Δt)
                (* 1e-9 (time-nanosecond Δt))))))

(alias schedule scheduler:*schedule*)
(alias callback schedule)

;; in case of emergency ☺
(alias h! sound:hush!)

(alias play! sound:set-dsp!)

(scheduler:init now)
(scheduler:start)
(repl:start-repl-server)
(define tuner-frequency 440.0)
(define tuner-period (/ 1.0 tuner-frequency))
(define tuner-half-period (/ tuner-period 2.0))
(define (tuner time channel)
  (sin (* 2π time tuner-frequency)))

;; (sound:set-dsp! tuner)
