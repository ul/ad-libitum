(library (ad-libitum sound (1))
  (export start set-dsp! hush! now)
  (import (chezscheme) (prefix (soundio) soundio:))
  (define (try thunk default)
    (call/cc
     (lambda (k)
       (with-exception-handler
           (lambda (x) (k default))
         thunk))))
  (define (safe-function f)
    (lambda args
      (try (lambda () (apply f args)) 0.0)))
  
  (define (silence time channel) 0.0)
  
  (define *sound-out* (soundio:open-default-out-stream silence))
  
  (define (now) (soundio:sound-out-time *sound-out*))
  
  (define (set-dsp! f)
    (soundio:sound-out-write-callback-set! *sound-out* (safe-function f)))
  
  (define (hush!) (set-dsp! silence))
  
  (define (start) (soundio:start-out-stream *sound-out*))
  )
