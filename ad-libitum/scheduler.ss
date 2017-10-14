(library (ad-libitum scheduler (1))
  (export start stop init
          start-scheduler stop-scheduler schedule now
          simple-scheduler
          *schedule* *now*)
  (import (chezscheme)
          (only (soundio) usleep))
  ;; <pairing-heap>
  ;; we do some #f-punning and don't throw on empty heaps
  
  (define heap/empty '())
  
  (define (heap/find-min heap)
    (if (null? heap)
        #f
        (car heap)))
  
  (define (heap/merge comparator h1 h2)
    (cond
     [(null? h1) h2]
     [(null? h2) h1]
     [(< (comparator (car h1)) (comparator (car h2)))
      (cons (car h1) (cons h2 (cdr h1)))]
     [else
      (cons (car h2) (cons h1 (cdr h2)))]))
  
  (define (heap/insert comparator elem heap)
    (heap/merge comparator (cons elem '()) heap))
  
  (define (heap/merge-pairs comparator subheaps)
    (cond
     [(null? subheaps) heap/empty]
     [(null? (cdr subheaps)) (car subheaps)]
     [else (heap/merge comparator
            (heap/merge comparator (car subheaps) (cadr subheaps))
            (heap/merge-pairs comparator (cddr subheaps)))]))
  
  (define (heap/delete-min comparator heap)
    (if (null? heap)
        heap/empty
        (heap/merge-pairs comparator (cdr heap))))
  ;; </pairing-heap>
  ;; <scheduler>
  ;; <scheduler-record>
  (define-record-type scheduler
    (fields now (mutable queue) resolution (mutable thread) mutex))
  
  (define (simple-scheduler now)
    (make-scheduler
     now           ; now
     heap/empty    ; queue
     250           ; resolution
     #f            ; thread
     (make-mutex)  ; mutex
     ))
  ;; </scheduler-record>
  ;; <event-record>
  (define-record-type event
    (fields time f args))
  ;; </event-record>
  ;; <scheduler-process-events>
  (define (process-events scheduler time)
    (with-mutex
     (scheduler-mutex scheduler)
     (let next-event ()
       (let ([event (heap/find-min (scheduler-queue scheduler))])
         (when (and event (<= (event-time event) time))
           (scheduler-queue-set!
            scheduler
            (heap/delete-min event-time (scheduler-queue scheduler)))
           (guard (_ [else #f])
             (let ([f (event-f event)])
               (apply (if (symbol? f)
                          (top-level-value f)
                          f)
                      (event-args event))))
           (next-event))))))
  ;; </scheduler-process-events>
  ;; <scheduler-interface>
  ;; <now>
  (define (now scheduler) ((scheduler-now scheduler)))
  ;; </now>
  ;; <schedule>
  (define schedule
    (case-lambda
      [(scheduler event)
       (with-mutex (scheduler-mutex scheduler)
                   (scheduler-queue-set! scheduler (heap/insert event-time event (scheduler-queue scheduler))))]
      [(scheduler t f . args)
       (schedule scheduler (make-event (inexact t) f args))]))
  ;; </schedule>
  ;; </scheduler-interface>
  ;; <scheduler-interface>
  ;; <start-scheduler>
  (define (start-scheduler scheduler)
    (fork-thread
     (lambda ()
       (scheduler-thread-set! scheduler (get-thread-id))
       (let* ([resolution (scheduler-resolution scheduler)]
              [expired-horizon (/ 0.5 resolution)]
              [microseconds-to-sleep (exact (floor (/ 1e6 resolution)))])
         (let loop ()
           (when (scheduler-thread scheduler)
             (process-events scheduler (+ (now scheduler) expired-horizon))
             (usleep 0 microseconds-to-sleep)
             (loop)))))))
  ;; </start-scheduler>
  ;; <stop-scheduler>
  (define (stop-scheduler scheduler)
    (scheduler-thread-set! scheduler #f))
  ;; </stop-scheduler>
  ;; </scheduler-interface>
  ;; </scheduler>
  (define *scheduler* #f)
  (define (init now) (set! *scheduler* (simple-scheduler now)))
  (define (start) (start-scheduler *scheduler*))
  (define (stop) (stop-scheduler *scheduler*))
  (define (*schedule* t f . args) (schedule *scheduler* (make-event t f args)))
  (define (*now*) (now *scheduler*))
  )
