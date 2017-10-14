(import (ad-libitum oscillator))

(load "ad-libitum/test-runner.ss")

(test-group
 "phasor"

 (let ([phase0 (~< 0.25)]
       [antiphase0 (~< 0.75)])
   (do-ec
    (:real-range frequency 0.0 440.0 1.23456)
    (:real-range time 0.0 1.0 0.001)
    (: channel 2)
    (test-approximate "antiphase"
      0.0
      (+ ((sine (phasor (~< frequency) phase0)) time channel)
         ((sine (phasor (~< frequency) antiphase0)) time channel))
      1e-14))))
