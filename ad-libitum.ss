;; <add-library-directories>
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
;; </add-library-directories>


(import (ad-libitum common))

(voodoo)

(import (chezscheme)
        (srfi s1 lists)
        (srfi s26 cut)
        (srfi s42 eager-comprehensions)
        (ad-libitum signal)
        (prefix (ad-libitum control) ctrl:)
        (prefix (ad-libitum oscillator) osc:)
        (prefix (ad-libitum envelope) env:)
        (prefix (ad-libitum filter) filter:)
        (prefix (ad-libitum scale) scale:)
        (prefix (ad-libitum instrument) inst:)
        (ad-libitum metro)
        (prefix (ad-libitum noise) noise:)
        (prefix (ad-libitum sound) sound:)
        (prefix (ad-libitum scheduler) scheduler:)
        (prefix (ad-libitum repl) repl:)
        (prefix (ad-libitum midi) midi:)
        )

;; <ad-libitum-init>
(alias now sound:now)
(alias schedule scheduler:*schedule*)
(alias callback schedule)

;; in case of emergency ☺
(alias hush! sound:hush!)
(alias h! hush!)

(alias play! sound:set-dsp!)

(sound:start)
(scheduler:init now)
(scheduler:start)
(repl:start-repl-server)
;; </ad-libitum-init>

;; <tuner-constants>
(define tuner-frequency 440.0)
(define tuner-period (/ tuner-frequency))
(define tuner-half-period (* 0.5 tuner-period))
;; </tuner-constants>

;; <test-tuner>
(define (tuner time channel)
  (sin (* 2π time tuner-frequency)))

;; (sound:set-dsp! tuner)
;; </test-tuner>

