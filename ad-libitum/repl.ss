(library (ad-libitum repl (1))
  (export start-repl-server)
  (import (chezscheme)
          (only (soundio) usleep)
          (prefix (bsd-sockets) sock:))
  ;; (define (try-display thunk default)
  ;;   (call/cc
  ;;    (lambda (k)
  ;;      (with-exception-handler
  ;;          (lambda (x)
  ;;            (display-condition x)
  ;;            (k default))
  ;;        thunk))))
  
  (define-syntax try-display
    (syntax-rules ()
      [(_ default e1 e2 ...)
       (guard (x [else (begin
                         (display-condition x)
                         default)])
         e1 e2 ...)]))
  (define (open-socket)
    (let ([socket (sock:create-socket
                   sock:socket-domain/internet
                   sock:socket-type/stream
                   sock:socket-protocol/auto)])
      (sock:bind-socket socket (sock:string->internet-address "127.0.0.1:37146"))
      (sock:listen-socket socket 1024)
      socket
      ))
  (define polling-usec 50000)
  (define max-chunk-length 65536)
  (define code-tx (make-transcoder (utf-8-codec) (eol-style lf) (error-handling-mode replace)))
  (define (spawn-remote-repl socket address)
    (fork-thread
     (lambda ()
       (let* (
              [call-with-send-port
               (lambda (f)
                 (let ([response (call-with-bytevector-output-port f code-tx)])
                   (sock:send-to-socket socket response address)))]
              [send-prompt
               (lambda ()
                 (call-with-send-port (lambda (p) (display "> " p))))]
              )
         (send-prompt)
         (let loop ()
           (usleep 0 polling-usec)
           (let-values ([(request address)
                         (sock:receive-from-socket socket max-chunk-length)])
             (if (and request (positive? (bytevector-length request)))
                 (call-with-port
                  (open-bytevector-input-port request code-tx)
                  (lambda (p)
                    (do ([x (read p) (read p)])
                        ((eof-object? x))
                      (printf "> ~s\r\n" x)
                      (call-with-send-port
                       (lambda (p)
                         (let* (
                                [result #f]
                                [output
                                 (with-output-to-string
                                   (lambda ()
                                     (set! result (try-display #f (eval x)))))]
                                )
                           (printf "| ~s\r\n" output)
                           (printf "< ~s\r\n" result)
                           (display output p)
                           (display result p)
                           (newline p)
                           )
                         )
                       ))
                    (send-prompt)
                    (loop))
                  )
                 (loop))))
         ))))
  (define (accept-connections repl-server-socket)
    (fork-thread
     (lambda ()
       (let loop ()
         (usleep 0 polling-usec)
         (let-values ([(socket address) (sock:accept-socket repl-server-socket)])
           (when socket
             (printf "New REPL @ ~s\r\n" (sock:internet-address->string address))
             (spawn-remote-repl socket address)))
         (loop)))))
  (define (start-repl-server)
    (accept-connections (open-socket)))
  )
