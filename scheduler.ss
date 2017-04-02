(library (scheduler)
  (export start stop
          (rename (*schedule* schedule) (*now* now)))
  (import (chezscheme))
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
     [(positive? (comparator (car h1) (car h2)))
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
  
  (define (simple-scheduler)
    (make-scheduler
     current-time                         ; now
     heap/empty                           ; queue
     (make-time 'time-duration 1000000 0) ; resolution
     #f                                   ; thread
     (make-mutex)                         ; mutex
     ))
  (define-record-type event
    (fields time f args))
  
  (define (event-comparator e1 e2)
    (let ([t1 (event-time e1)]
          [t2 (event-time e2)])
      (cond
       [(time<? t1 t2) 1]
       [(time>? t1 t2) -1]
       [else 0])))
  (define (now scheduler) ((scheduler-now scheduler)))
  (define (schedule scheduler event)
    (with-mutex (scheduler-mutex scheduler)
                (scheduler-queue-set! scheduler (heap/insert event-comparator event (scheduler-queue scheduler)))))
  (define (process-events scheduler t)
    (with-mutex
     (scheduler-mutex scheduler)
     (let next-event ()
       (let ([event (heap/find-min (scheduler-queue scheduler))])
         (when (and event (time<=? (event-time event) t))
           (scheduler-queue-set! scheduler (heap/delete-min event-comparator (scheduler-queue scheduler)))
           (apply (event-f event) (event-args event))
           (next-event))))))
  
  (define (start-scheduler scheduler)
    (fork-thread
     (lambda ()
       (scheduler-thread-set! scheduler (get-thread-id))
       (let ([zero-duration (make-time 'time-duration 0 0)]
             [resolution (scheduler-resolution scheduler)])
         (let loop ()
           (when (scheduler-thread scheduler)
             (let ([clock (current-time)]
                   [t (add-duration (now scheduler) resolution)])
               (process-events scheduler t)
               (let* ([day (time-difference (current-time) clock)]
                      [night (time-difference resolution day)])
                 (when (time<? zero-duration night)
                   (sleep night))
                 (loop)))))))))
  (define (stop-scheduler scheduler)
    (scheduler-thread-set! scheduler #f))
  (define *scheduler* (simple-scheduler))
  (define (start) (start-scheduler *scheduler*))
  (define (stop) (stop-scheduler *scheduler*))
  (define (*schedule* t f . args) (schedule *scheduler* (make-event t f args)))
  (define (*now*) (now *scheduler*))
  )
