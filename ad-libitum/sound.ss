(library (ad-libitum sound (1))
  (export start set-dsp! hush! *sample-rate* *channels* now)
  (import (chezscheme) (prefix (soundio) soundio:))
  (define (silence time channel) 0.0)
  
  (define *dsp* silence)
  (define *time* 0.0)
  
  (define (write-callback time channel)
    (set! *time* time)
    (guard (_ [else 0.0])
      (*dsp* time channel)))
  
  (define (now) *time*)
  
  (define *sound-out* (soundio:open-default-out-stream write-callback))
  (define *sample-rate* (soundio:sample-rate *sound-out*))
  (define *channels* (soundio:channel-count *sound-out*))
  
  (define (set-dsp! f)
    (set! *dsp* f))
  
  (define (hush!) (set-dsp! silence))
  
  (define (start) (soundio:start-out-stream *sound-out*))
  )
