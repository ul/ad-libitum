(library (ad-libitum sound (1))
  (export start set-dsp! hush! *sample-rate* *channels*)
  (import (chezscheme) (prefix (soundio) soundio:))
  (define (silence time channel) 0.0)
  
  (define *sound-out* (soundio:open-default-out-stream silence))
  (define *sample-rate* (soundio:sound-out-sample-rate *sound-out*))
  (define *channels* (soundio:sound-out-channel-count *sound-out*))
  
  (define (set-dsp! f)
    (soundio:sound-out-write-callback-set! *sound-out* f))
  
  (define (hush!) (set-dsp! silence))
  
  (define (start) (soundio:start-out-stream *sound-out*))
  )
