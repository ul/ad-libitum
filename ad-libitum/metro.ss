(library (ad-libitum metro (1))
  (export metro *metro* *beat* play-pattern time->beat beat->time set-bpm!)
  (import (chezscheme)
          (ad-libitum common)
          (only (ad-libitum sound) now)
          (rename (only (ad-libitum scheduler) *schedule*)
                  (*schedule* schedule)))
  ;; <beat>
  (define (time->beat time bpm)
    (-> time (* bpm) (/ 60) (round)))
  
  (define (beat->time beat bpm)
    (-> beat (* 60) (/ bpm)))
  
  (define (next-beat time bpm)
    (beat->time (+ 1 (time->beat time bpm)) bpm))
  
  (define (metro bpm . args)
    (apply schedule (next-beat (now) bpm) args))
  
  (define *bpm* 60.0)
  
  (define (set-bpm! bpm)
    (set! *bpm* bpm))
  
  (define (*beat*)
    (time->beat (now) *bpm*))
  
  (define (*metro* . args)
    (apply metro *bpm* args))
  ;; </beat>
  ;; <pattern>
  (define (play-pattern pattern sound beat)
    (let ([n (length pattern)])
      (when (positive? (choice pattern (exact beat)))
        (sound))))
  ;; </pattern>
  )
