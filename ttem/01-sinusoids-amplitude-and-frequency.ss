;; NOTE This file is supposed to be evaluated by chunks in Ad Libitum repl, not as a solid program.

;;; 1. Sinusoids, amplitude and frequency

;; Ad Libitum deals with continous representation of time,
;; it's expressed as a float number of seconds passed from the stream start.
;; For any signal either continous or discrete feels more natural, and there is
;; no clear measure of their shares. Good news that conversion is relatively
;; easy in both directions.

;; ~< is a syntax sugar which wraps code into function of two parameters: `time'
;; and `channel', which are thrown in scope. Such kind of function is how Ad
;; Libitum represents audio signals. Think about time as a continuous
;; counterpart of sample number `n' in TTEM. `channel' will be discussed later.
;; Think about `~<' as the thing which animates your formula to produce signal.
(define (sinusoid a ω φ)
  (~< (* a (cos (+ (* ω time) φ)))))

;; `play!' connects your signal to audio output and you can hear it! Amplitude
;; range in Ad Libitum is [-1, 1], setting a=0.2 we are producing signal in 1/5
;; of it.
(play! (sinusoid 0.2 2000.0 0.0))

;; And the most important function in Ad Libitum — `h!'
;; hush!
(h!)

(define (ƒ→ω ƒ)
  (/ ω 2π))

(define (ω→ƒ ω)
  (* 2π ƒ))

(define (sinusoid a ƒ φ)
  (~< (* a (cos (+ (* 2π ƒ time) φ)))))

(play! (sinusoid 0.2 440.0 0.0))

;;; 1.1 Measures of Amplitude

;; Convert amplitude to decibels, with a0 = 1e-5 as suggested in TTEM
(define (amp->db x)
  (* 20.0 (log (* x 1e5) 10.0)))

(amp->db 1.0)
(amp->db 0.5)

;; But setting a0 to 1.0 is also very convenient — maximum amplitude is then 0dB
;; and any one below is just negative. All relations stays the same.
(define (amp->db x)
  (* 20.0 (log x 10.0)))

(amp->db 0.5)
(amp->db 1e-5)

;; This is not a blood pact, but many of Ad Libitum code relies on fact that
;; audiosignal is called sample by sample, without skips. Making that assumption
;; we are able to write RMS amplitude measurement even in continuous
;; representation of time.
(define (rms-amplitude window-width s)
  (let ([windows (make-vector *channels*)]
        [N (-> window-width (* *sample-rate*) (ceiling) (exact))]
        [cursor -1])
    (do-ec (: i *channels*)
           (vector-set! windows i (make-vector N 0.0)))
    (~<
     (when (zero? channel)
       (set! cursor (mod (+ cursor 1) N)))
     (let ([window (vector-ref windows channel)]
           [x 0.0])
       (vector-set! window cursor (<~ s))
       (vector-for-each (λ (y) (set! x (+ x (* y y)))) window)
       (/ x N)))))

;; Let's make sinusoid amplitude a signal, and set it to rms amplitude measured
;; from sinusoid with peak amplitude 1.0
;; Note `<~' syntax sugar to apply audiosignal function to time and channel.
;; `(<~ a)' is equivalent to `(a time channel)'
;; Also note how we make constant signal with `(~< 1.0)'
(define (sinusoid a ƒ φ)
  (~< (* (<~ a) (cos (+ (* 2π ƒ time) φ)))))

;; Note that computing rms in realtime is very expensive.
;; Try to increase window width until you get audio buffer underflow glitches.
(play! (sinusoid (rms-amplitude 2/440 (sinusoid (~< 1.0) 440.0 0.0)) 440.0 0.0))

;; Compare with 1.0 peak itself, it's louder!
(play! (sinusoid (~< 1.0) 440.0 0.0))

;; Let's throttle `rms-amplitude' to make it less hungry
(define (rms-amplitude window-width s)
  (let ([windows (make-vector *channels*)]
        [N (-> window-width (* *sample-rate*) (ceiling) (exact))]
        [cursor -1]
        [amplitudes (make-vector *channels* 0.0)])
    (do-ec (: i *channels*)
           (vector-set! windows i (make-vector N 0.0)))
    (~<
     (when (zero? channel)
       (set! cursor (mod (+ cursor 1) N)))
     (let ([window (vector-ref windows channel)])
       (vector-set! window cursor (<~ s))
       (when (zero? cursor)
         (vector-set!
          amplitudes
          channel
          (let ([x 0.0])
            (vector-for-each (λ (y) (set! x (+ x (* y y)))) window)
            (/ x N)))))
     (vector-ref amplitudes channel))))

(play! (sinusoid (rms-amplitude 1 (+~ (sinusoid (~< 1.0) 440.0 0.0))) 440.0 0.0))

;; Chez Scheme Gℂ is so cool that though we produce a lot of garbage every
;; second it doesn't interrupt sound! But we still have a noticeable lag on
;; window initialization. Take it into account if you are going to spawn
;; capacitive `rms-amplitude' signals frequently.

;; In future for sinusoid we will use built-in Ad Libitum oscillator available
;; as `osc:sine' (takes phasor signal as input, we'll cover it later) and
;; `osc:sine*' (takes frequency and optional initial phase).
(play! (osc:sine* 440.0 0.0))

;;; 1.2 Units of Amplitude
;;; 1.3 Controlling Amplitude
;;; 1.4 Frequency
;;; 1.5 Synthesizing a sinusoid
;;; 1.6 Superposing Signals
;;; 1.7 Periodic Signals
;;; 1.8 About the Software Examples
;;; 1.9 Examples
;;; Exercises
