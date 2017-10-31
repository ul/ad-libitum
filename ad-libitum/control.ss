#!chezscheme
(library (ad-libitum control (1))
  (export make-control define-control window)
  (import (chezscheme)
          (srfi s42 eager-comprehensions)
          (ad-libitum common)
          (ad-libitum signal)
          )
  ;; <control-signal>
  (define (make-control x)
    (let ([b (box x)])
      (values (~< (unbox b)) b)))
  
  (define-syntax (define-control stx)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax
         template-identifier
         (string->symbol
          (apply string-append
                 (map (lambda (x)
                        (if (string? x)
                            x
                            (symbol->string (syntax->datum x))))
                      args))))))
    (syntax-case stx ()
      [(_ name initial-value)
       (with-syntax ([s (construct-name #'name #'name '~)]
                     [ref (construct-name #'name #'name '-ref)]
                     [set (construct-name #'name #'name '-set!)])
         #'(begin
             (define-values (s name) (make-control initial-value))
             (define (ref) (unbox name))
             (define (set value) (set-box! name value))))]))
  ;; </control-signal>

  ;; <window>
  (define (window width signal)
    (let ([windows (make-vector *channels*)]
          [N (-> width (* *sample-rate*) (ceiling) (exact))]
          [cursor -1])
      (do-ec (: i *channels*)
             (vector-set! windows i (make-vector N 0.0)))
      (values
       (~<
        (when (zero? channel)
          (set! cursor (mod (+ cursor 1) N)))
        (let ([sample (<~ signal)]
              [window (channel-ref windows)])
          (vector-set! window cursor sample)
          sample))
       (λ () windows))))
  ;; </window>

  )
