(library (ad-libitum midi (1))
  (export start stop set-note-on! set-note-off! set-cc!)
  (import (chezscheme)
          (prefix (ad-libitum scheduler) scheduler:)
          (prefix (portmidi) pm:))
  ;; <midi>
  (define (*on-note-on* timestamp data1 data2 channel)
    (printf "~s:~s:~s:~s\r\n" timestamp data1 data2 channel))
  
  (define (*on-note-off* timestamp data1 data2 channel)
    (printf "~s:~s:~s:~s\r\n" timestamp data1 data2 channel))
  
  (define (*on-cc* timestamp data1 data2 channel)
    (printf "~s:~s:~s:~s\r\n" timestamp data1 data2 channel))
  
  (define (set-note-on! f) (set! *on-note-on* f))
  (define (set-note-off! f) (set! *on-note-off* f))
  (define (set-cc! f) (set! *on-cc* f))
  
  (define *polling-cycle* 0.005)
  
  (define *stream* #f)
  (define *scheduler* #f)
  
  (define (process-event timestamp type data1 data2 channel)
    (cond
      [(= type pm:*midi-note-on*) (*on-note-on* timestamp data1 data2 channel)]
      [(= type pm:*midi-note-off*) (*on-note-off* timestamp data1 data2 channel)]
      [(= type pm:*midi-cc*) (*on-cc* timestamp data1 data2 channel)]
      [else (printf "Unsupported event type: ~s\r\n" type)]))
  
  (define (make-safe-process-event timestamp)
    (lambda args
      (guard (_ [else #f]) (apply process-event timestamp args))))
  
  (define (process-events)
    (let ([timestamp (scheduler:now *scheduler*)])
      (when (pm:poll *stream*)
        (pm:read *stream* (make-safe-process-event timestamp)))
      (scheduler:schedule *scheduler*
                          (+ timestamp *polling-cycle*)
                          process-events)))
  
  (define (start now)
    (unless *stream*
      (pm:init)
      (set! *stream* (pm:open-input 0))
      (set! *scheduler* (scheduler:simple-scheduler now))
      (scheduler:start-scheduler *scheduler*)
      (process-events)))
  
  (define (stop)
    (when *stream*
      (scheduler:stop-scheduler *scheduler*)
      (pm:close *stream*)
      (pm:terminate)
      (set! *stream* #f)
      (set! *scheduler* #f)))
  ;; </midi>
  )
