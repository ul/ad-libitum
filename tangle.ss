#!/usr/bin/env scheme --script

;;;; Quick & Dirty tangler for org mode files.
;;;; Because org mode itself tangles too slow.

(import (chezscheme))

(load "irregex.ss")

(define utf-tx (make-transcoder (utf-8-codec)))

(define block-name-irx (irregex '(: "#+NAME:" (* space) ($ (+ any)) (* space))))

(define begin-tangle-block-irx
  (irregex
   '(: "#+BEGIN_SRC"
       (+ space)
       (* any)
       (: ":tangle"
          (+ space)
          ($ (+ (~ space)))
          )
       (* any)
       )))

(define end-block-irx (irregex '(: "#+END_SRC")))

(define macro-irx (irregex '(: (=> prefix (* any)) "<<" (=> name (+ any)) ">>" (=> suffix (* any)))))

(define (match-macro s)
  (let ([m (irregex-match macro-irx s)])
    (and m
         (list (irregex-match-substring m 'prefix)
               (irregex-match-substring m 'name)
               (irregex-match-substring m 'suffix)))))

(irregex-match-substring (irregex-match macro-irx "    ;;; <<test>> )") 'suffix)

(define (get-block-name s)
  (let ([m (irregex-match block-name-irx s)])
    (and m (irregex-match-substring m 1))))

(define (get-tangle-path s)
  (let ([m (irregex-match begin-tangle-block-irx s)])
    (and m (irregex-match-substring m 1))))

(define (is-block-end? s)
  (irregex-match end-block-irx s))

(define block-name->code (make-hashtable string-hash string=?))

(define tangle-path->block-name (make-hashtable string-hash string=?))

(define (start-block block-name tangle-path)
  (when tangle-path
    (hashtable-set! tangle-path->block-name tangle-path block-name)))

(define (process-text line loop)
  (let ([block-name (get-block-name line)])
    (if block-name
        (loop 'named-block block-name #f)
        (let ([tangle-path (get-tangle-path line)])
          (if tangle-path
              (let ([block-name (gensym->unique-string (gensym))])
                (start-block block-name tangle-path)
                (loop 'code block-name tangle-path))
              (loop 'text #f #f))))))

(define (process-named-block line loop block-name)
  (let ([tangle-path (get-tangle-path line)])
    (start-block block-name tangle-path)
    (loop 'code block-name tangle-path)))

(define (process-code line loop block-name tangle-path)
  (if (is-block-end? line)
      (loop 'text #f #f)
      (begin
        (hashtable-update! block-name->code block-name
                           (lambda (block) (cons line block))
                           '())
        (loop 'code block-name tangle-path))))

(define (get-path path)
  (let ([components (string-split-char path #\/)])
    (let loop ([components components]
               [path ""])
      (if (null? (cdr components))
          path
          (loop (cdr components) (string-append path (car components) "/"))))))

(define paddle-irx (irregex '(: ($ (* space)) (* any))))

(define (string-paddle-length s)
  (string-length (irregex-match-substring (irregex-match paddle-irx s) 1)))

(define (string-drop s n)
  (let ([end (string-length s)])
    (if (< n end)
      (substring s n end)
      s)))

(define (tangle block-prefix block-name port)
  (let* ([code (reverse (hashtable-ref block-name->code block-name '()))]
         [paddle (string-paddle-length (car code))])
    (for-each
     (lambda (line)
       (let* ([line (string-drop line paddle)]
              [macro (match-macro line)])
         (if macro
             (let ([prefix (car macro)]
                   [block-name (cadr macro)]
                   [suffix (caddr macro)])
               (tangle (string-append block-prefix prefix) block-name port)
               (display suffix port))
             (begin
               (display block-prefix port)
               (display line port)))
         (newline port)))
     code)))

;;;

(define input-path (cadr (command-line)))

(define input-port
  (open-file-input-port input-path (file-options) (buffer-mode line) utf-tx))

(let loop ([parser-state 'text]
           [block-name #f]
           [tangle-path #f])
  (let ([line (get-line input-port)])
    (unless (eof-object? line)
      (case parser-state
        [text (process-text line loop)]
        [named-block (process-named-block line loop block-name)]
        [code (process-code line loop block-name tangle-path)]))))

(vector-for-each
 (lambda (tangle-path)
   (let* ([block-name (hashtable-ref tangle-path->block-name tangle-path #f)]
          [path (get-path tangle-path)])
     (unless (or (string=? path "") (file-exists? path))
       (mkdir path))
     (let ([output-port (open-file-output-port tangle-path (file-options no-fail) (buffer-mode line) utf-tx)])
       (tangle "" block-name output-port)
       (close-port output-port))))
 (hashtable-keys tangle-path->block-name))

(close-port input-port)
