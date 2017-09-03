(library (repl (1))
  (export start-repl-server)
  (import (chezscheme)
          (prefix (bsd-sockets) sock:))
  (define (open-socket)
    (let ([socket (sock:create-socket
                   sock:socket-domain/internet
                   sock:socket-type/stream
                   sock:socket-protocol/auto)])
      (sock:bind-socket socket (sock:string->internet-address "127.0.0.1:37146"))
      (sock:listen-socket socket 1024)
      socket
      ))
  (define max-code-length 65536)
  (define code-tx (make-transcoder (utf-8-codec) (eol-style lf)
                                   (error-handling-mode replace)))
  (define polling-cycle (make-time 'time-duration 200000000 0))
  (define (spawn-remote-repl socket address)
    (fork-thread
     (lambda ()
       (let loop ()
         (sleep polling-cycle)
         (with-exception-handler
             ;; TODO report to client
             ;; TODO stop loop on severe errors (which?)
             (lambda (x)
               (display-condition x)
               (let ([response (call-with-bytevector-output-port newline code-tx)])
                 (sock:send-to-socket socket response address))
               (loop))
           (lambda ()
             (let-values ([(request address)
                           (sock:receive-from-socket socket max-code-length)])
               (if (and request (positive? (bytevector-length request)))
                 (call-with-port
                  (open-bytevector-input-port request code-tx)
                  (lambda (p)
                    (do ([x (read p) (read p)])
                        ((eof-object? x))
                        (printf "~s\r\n" x)
                      (let ([response (call-with-bytevector-output-port
                                       (lambda (p)
                                         ;; TODO `display` ???
                                         (write (eval x) p)
                                         (newline p))
                                       code-tx)])
                        (sock:send-to-socket socket response address)))
                    (loop)))
                 (loop)))))))))
  (define (accept-connections repl-server-socket)
    (fork-thread
     (lambda ()
       (let loop ()
         (sleep polling-cycle)
         (let-values ([(socket address) (sock:accept-socket repl-server-socket)])
           (when socket
             (printf "New REPL @ ~s\r\n" (sock:internet-address->string address))
             (spawn-remote-repl socket address)))
         (loop)))))
  (define (start-repl-server)
    (accept-connections (open-socket)))
  )
