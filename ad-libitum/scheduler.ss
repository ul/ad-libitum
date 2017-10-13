(library (ad-libitum scheduler (1))
  (export start stop init
          start-scheduler stop-scheduler schedule now
          simple-scheduler
          *schedule* *now*)
  (import (chezscheme)
          (only (soundio) usleep))
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
  (define-record-type scheduler
    (fields now (mutable queue) resolution (mutable thread) mutex))
  
  (define (simple-scheduler now)
    (make-scheduler
     now           ; now
     heap/empty    ; queue
     0.001         ; resolution
     #f            ; thread
     (make-mutex)  ; mutex
     ))
  (define-record-type event
    (fields time f args))
  (define (process-events scheduler t)
    (with-mutex
     (scheduler-mutex scheduler)
     (let next-event ()
       (let ([event (heap/find-min (scheduler-queue scheduler))])
         (when (and event (<= (event-time event) t))
           (scheduler-queue-set! scheduler (heap/delete-min event-time (scheduler-queue scheduler)))
           (guard (_ [else #f])
             (let ([f (event-f event)])
               (apply (if (symbol? f)
                          (top-level-value f)
                          f)
                      (event-args event))))
           (next-event))))))
  (define (now scheduler) ((scheduler-now scheduler)))
  (define schedule
    (case-lambda
      [(scheduler event)
       (with-mutex (scheduler-mutex scheduler)
                   (scheduler-queue-set! scheduler (heap/insert event-time event (scheduler-queue scheduler))))]
      [(scheduler t f . args)
       (schedule scheduler (make-event t f args))]))
  (define (start-scheduler scheduler)
    (fork-thread
     (lambda ()
       (scheduler-thread-set! scheduler (get-thread-id))
       (let* ([resolution (scheduler-resolution scheduler)]
              [polling-usec (exact (floor (* resolution 1e6)))])
         (let loop ()
           (when (scheduler-thread scheduler)
             (process-events scheduler (+ (now scheduler) resolution))
             (usleep 0 polling-usec)
             (loop)))))))
  (define (stop-scheduler scheduler)
    (scheduler-thread-set! scheduler #f))
  (define *scheduler* #f)
  (define (init now) (set! *scheduler* (simple-scheduler now)))
  (define (start) (start-scheduler *scheduler*))
  (define (stop) (stop-scheduler *scheduler*))
  (define (*schedule* t f . args) (schedule *scheduler* (make-event t f args)))
  (define (*now*) (now *scheduler*))
  )
