(library (ad-libitum repl (1))
  (export start-repl-server)
  (import (chezscheme)
          (only (soundio) usleep)
          (prefix (bsd-sockets) sock:))
  ;; <open-socket>
  (define (open-socket)
    (let ([socket (sock:create-socket
                   sock:socket-domain/internet
                   sock:socket-type/stream
                   sock:socket-protocol/auto)])
      ;; <bind-socket>
      (sock:bind-socket socket (sock:string->internet-address "127.0.0.1:37146"))
      ;; </bind-socket>
      ;; <listen-socket>
      (sock:listen-socket socket 1024)
      ;; </listen-socket>
      socket
      ))
  ;; </open-socket>
  ;; <spawn-remote-repl>
  ;; <spawn-remote-repl-options>
  (define polling-microseconds 50000)
  (define max-chunk-length 65536)
  (define code-tx (make-transcoder (utf-8-codec) (eol-style lf) (error-handling-mode replace)))
  ;; </spawn-remote-repl-options>
  (define (spawn-remote-repl socket address)
    (fork-thread
     (lambda ()
       (let* (
              ;; <repl-send-helpers>
              [call-with-send-port
               (lambda (f)
                 (let ([response (call-with-bytevector-output-port f code-tx)])
                   (sock:send-to-socket socket response address)))]
              [send-prompt
               (lambda ()
                 (call-with-send-port (lambda (p) (display "> " p))))]
              ;; </repl-send-helpers>
              )
         (send-prompt)
         ;; <repl-loop>
         (let loop ()
           (usleep 0 polling-microseconds)
           (let-values ([(request address)
                         (sock:receive-from-socket socket max-chunk-length)])
             (if (and request (positive? (bytevector-length request)))
                 (call-with-port
                  (open-bytevector-input-port request code-tx)
                  ;; <repl-read-eval-print>
                  (lambda (p)
                    (do ([x (read p) (read p)])
                        ((eof-object? x))
                      (printf "> ~s\r\n" x)
                      (call-with-send-port
                       ;; <repl-eval-print>
                       (lambda (p)
                         (let* (
                                ;; <repl-eval>
                                [result #f]
                                [output
                                 (with-output-to-string
                                   (lambda ()
                                     (set! result (guard (x [else (display-condition x)]) (eval x)))))]
                                ;; </repl-eval>
                                )
                           ;; <repl-print>
                           (printf "| ~s\r\n" output)
                           (printf "< ~s\r\n" result)
                           (display output p)
                           (display result p)
                           (newline p)
                           ;; </repl-print>
                           )
                         )
                       ;; </repl-eval-print>
                       ))
                    (send-prompt)
                    (loop))
                  ;; </repl-read-eval-print>
                  )
                 (loop))))
         ;; </repl-loop>
         ))))
  ;; </spawn-remote-repl>
  ;; <accept-connections>
  (define (accept-connections repl-server-socket)
    (fork-thread
     (lambda ()
       (let loop ()
         (usleep 0 polling-microseconds)
         (let-values ([(socket address) (sock:accept-socket repl-server-socket)])
           (when socket
             (printf "New REPL @ ~s\r\n" (sock:internet-address->string address))
             (spawn-remote-repl socket address)))
         (loop)))))
  ;; </accept-connections>
  ;; <start-repl-server>
  (define (start-repl-server)
    (accept-connections (open-socket)))
  ;; </start-repl-server>
  )
