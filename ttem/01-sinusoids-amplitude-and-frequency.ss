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
  (* 2π ƒ))

(define (ω→ƒ ω)
  (/ ω 2π))

(define (sinusoid a ƒ φ)
  (~< (* a (cos (+ (* (ƒ→ω ƒ) time) φ)))))

(play! (sinusoid 0.2 440.0 0.0))
(h!)

;;; 1.1 Measures of Amplitude

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
  (~< (* (<~ a) (cos (+ (* (ƒ→ω ƒ) time) φ)))))

;; Note that computing rms in realtime is very expensive.
;; Try to increase window width until you get audio buffer underflow glitches.
(play! (sinusoid (rms-amplitude 2/440 (sinusoid (~< 1.0) 440.0 0.0)) 440.0 0.0))
(h!)

;; Compare with 1.0 peak itself, it's louder!
(play! (sinusoid (~< 1.0) 440.0 0.0))
(h!)

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
(h!)

;; Chez Scheme Gℂ is so cool that though we produce a lot of garbage every
;; second it doesn't interrupt sound! But we still have a noticeable lag on
;; window initialization. Take it into account if you are going to spawn
;; capacitive `rms-amplitude' signals frequently.

;; In future for sinusoid we will use built-in Ad Libitum oscillator available
;; as `osc:sine' (takes phasor signal as input, we'll cover it later) and
;; `osc:sine*' (takes frequency and optional initial phase).
(play! (osc:sine* 440.0 0.0))
(h!)

;;; 1.2 Units of Amplitude

;; Convert amplitude to decibels, with a0 = 1e-5 as suggested in TTEM
(define (amp->dB x)
  (* 20.0 (log (* x 1e5) 10.0)))

(amp->dB 1.0)
(amp->dB 0.5)

;; But setting a0 to 1.0 is also very convenient — maximum amplitude is then 0dB
;; and any one below is just negative. All relations stays the same.
(define (amp->dB x)
  (* 20.0 (log x 10.0)))

(amp->dB 0.5)
(amp->dB 1e-5)

;; And convert decibels back to amplitude:
(define (dB->amp x)
  (expt 10.0 (/ x 20.0)))

(dB->amp -100.0)
(dB->amp -50.0)
(dB->amp -10.0)
(dB->amp 0.0)

;; Amplitude is related in an inexact way to the perceived loudness of a sound.
;; In general, two signals with the same peak or RMS amplitude won’t necessarily
;; have the same loudness at all. But amplifying a signal by 3 dB, say, will
;; fairly reliably make it sound about one “step” louder.
(define *volume-step-dB* 3.0)

;; Let's test it!
(define (sinusoid a ƒ φ)
  (~< (* a (cos (+ (* (ƒ→ω ƒ) time) φ)))))
(play! (sinusoid (dB->amp -10.0) 440.0 0.0))
(play! (sinusoid (dB->amp (- 10.0 *volume-step-dB*)) 440.0 0.0))
(h!)

;; Try to change step. For that wave personally I hear 2dB difference.

;;; 1.3 Controlling Amplitude

;; We already controlled amplitude by multiplying every sample by `a'
;; Let's do it by multiplying sinusoid by constant signal.
(play! (*~ (~< 0.5) (osc:sine* 440.0)))
(play! (*~ (~< 0.2) (osc:sine* 440.0)))
(h!)

;;; 1.4 Frequency

(define (midi-pitch->frequency m)
  (* 440.0 (expt 2.0 (/ (- m 69.0) 12.0))))

(define (frequency->midi-pitch f)
  (+ 69 (exact (round (* 12.0 (log (/ f 440.0) 2.0))))))

(play! (osc:sine* (midi-pitch->frequency 69)))
(play! (osc:sine* (midi-pitch->frequency 72)))
(h!)

;; Ad Libitum allows you to use MIDI controller. Support is still incomplete and
;; relies on many assumptions. Your MIDI input device should be connected and
;; identified as the first one.
;; `now' is Ad Libitum clock function. It's required for MIDI module to put
;; proper timestamps on events.
(midi:start now)

;; Let's defined so called control signal for our frequency. We'll speak about
;; control signals later, but putting it simply, control signal is an audio
;; signal which is updated in non-audio rate by calling its setter.
(define-values (frequency set-frequency!) (control-signal id 440.0 instant-transition))

;; Let's set callback which will be called for every control change MIDI event.
(midi:set-cc! (λ (t knob value channel)
                (set-frequency! (midi-pitch->frequency value))))

(play! (osc:sine* frequency))
(h!)

;; Notice that abrupt change of frequency cause "pops" discussed in TTEM 1.5

;;; 1.5 Synthesizing a sinusoid

;; To make transition smooth we could use built-in *linear-transition*
(define-values (frequency set-frequency!) (control-signal id 440.0 *linear-transition*))

;; Besides of using MIDI input we could make frequency change programmatically.
;; `schedule' allows you to call any function later at given point of time.
;; Any function could schedule itself. It is called temporal recursion.
(define (swap-frequency i)
  (if (zero? i)
      (set-frequency! 440.0)
      (set-frequency! 220.0))
  (schedule (+ (now) 1/4) 'swap-frequency (- 1 i)))

(swap-frequency 0)

(play! (osc:sine* frequency))
(h!)

;;; 1.6 Superposing Signals
;;; 1.7 Periodic Signals
;;; 1.8 About the Software Examples
;;; 1.9 Examples
;;; Exercises
