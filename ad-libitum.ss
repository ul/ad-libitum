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
        (srfi s1 lists)
        (prefix (sound) sound:)
        (prefix (scheduler) scheduler:)
        (prefix (repl) repl:))
;; voodoo
(collect-maximum-generation 254)
(collect-generation-radix 2)
(optimize-level 3)

(alias λ lambda)

(alias now sound:now)
(alias schedule scheduler:schedule)
(alias callback schedule)

;; in case of emergency ☺
(alias h! sound:hush!)

;; TODO make actual
(define *channels* 2)

(sound:start)
(scheduler:init now)
(scheduler:start)
(repl:start-repl-server)
(define (compose . fns)
  (define (make-chain fn chain)
    (lambda args
      (call-with-values (lambda () (apply fn args)) chain)))
  (reduce make-chain values fns))

(define ∘ compose)
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
  (sine time tuner-frequency))

;; (sound:set-dsp! tuner)
(define (constant amplitude)
  (lambda (time channel)
    amplitude))
(define silence (constant 0.0))
(extend-syntax (@) [(@ signal) (signal time channel)])
(define (mod1 x)
  (mod x 1.0))

(define (phase frequency phase0)
  (let ([previous-time (make-vector *channels* 0.0)]
        [previous-phase (make-vector *channels* 0.0)])
    (lambda (time channel)
      (let* ([time-delta (- time (vector-ref previous-time channel))]
             [phase-delta (* time-delta (@ frequency))]
             [next-phase (mod1 (+ (vector-ref previous-phase channel) phase-delta))])
        (vector-set! previous-time channel time)
        (vector-set! previous-phase channel next-phase)
        (mod1 (+ next-phase (@ phase0)))))))

(define (phase* frequency)
  (phase frequency silence))
(define (sine-wave phase)
  (λ (time channel)
    (sin (* two-pi (@ phase)))))

(define (cosine-wave phase)
  (λ (time channel)
    (cos (* two-pi (@ phase)))))

(define (square-wave phase)
  (λ (time channel)
    (if (< (@ phase) 0.5)
        1.0
        -1.0)))

;; when `pulse-width' is `(constant 0.5)' it's identical to `square-wave'
(define (pulse-wave pulse-width phase)
  (λ (time channel)
    (if (< (@ phase) (@ pulse-width))
        1.0
        -1.0)))

(define (tri-wave phase)
  (λ (time channel)
    (let ([phase (@ phase)])
      (if (< phase 0.5)
          (- (* 4.0 phase) 1.0)
          (+ (* -4.0 phase) 3.0)))))

(define (saw-wave phase)
  (λ (time channel)
    (- (* 2.0 (@ phase)) 1.0)))

;; (define (table-wave table phase)
;;   (let ([n (vector-length table)])
;;     (lambda (time channel)
;;       (vector-ref table (exact (truncate (* (@ phase) n)))))))

(define (table-wave table phase)
  (let ([n (fixnum->flonum (vector-length table))])
    (λ (time channel)
      (vector-ref table (flonum->fixnum (fltruncate (fl* (@ phase) n)))))))

(define (random-amplitude)
  (- (random 2.0) 1.0))

(define (random-wave time channel)
  (random-amplitude))
(define (live-signal symbol)
  (lambda (time channel)
    ((top-level-value symbol) time channel)))
(define (live-constant symbol)
  (λ (time channel)
    (top-level-value symbol)))
(define (signal-sum* x y)
  (lambda (time channel)
    (+ (@ x) (@ y))))

(define (signal-sum x . xs)
  (fold-left signal-sum* x xs))

(define (signal-prod* x y)
  (lambda (time channel)
    (* (@ x) (@ y))))

(define (signal-prod x . xs)
  (fold-left signal-prod* x xs))

(define (signal-diff x . xs)
  (let ([y (apply signal-sum xs)])
    (lambda (time channel)
      (- (@ x) (@ y)))))

(define (signal-div x . xs)
  (let ([y (apply signal-prod xs)])
    (lambda (time channel)
      (/ (@ x) (@ y)))))

(alias +~ signal-sum)
(alias *~ signal-prod)
(alias -~ signal-diff)
(alias /~ signal-div)

(alias ∑ signal-sum)
(alias ∏ signal-prod)
(define simple-osc (∘ sine-wave phase* constant))
(define (adsr start end attack decay sustain release)
  (λ (time channel)
    (let ([end (@ end)])
      (if (<= end time)
          ;; NOTE OFF
          (let ([Δt (- time end)]
                [r (@ release)])
            (if (and (positive? r)
                     (<= Δt r))
                (* (- 1.0 (/ Δt r)) (@ sustain))
                0.0))
          ;; NOTE ON
          (let ([start (@ start)])
            (if (<= start time)
                (let ([Δt (- time start)]
                      [a (@ attack)])
                  (if (and (positive? a)
                           (<= Δt a))
                      (/ Δt a)
                      (let ([Δt (- Δt a)]
                            [d (@ decay)]
                            [s (@ sustain)])
                        (if (and (positive? d)
                                 (<= Δt d))
                            (- 1.0 (* (- 1.0 s) (/ Δt d)))
                            s))))
                0.0))))))
(define (impulse start apex)
  (λ (time channel)
    (let ([start (@ start)])
      (if (<= start time)
          (let ([h (/ (- time start)
                      (- (@ apex) start))])
            (* h (exp (- 1.0 h))))
          0.0))))
