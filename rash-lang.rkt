#lang racket/base

(require syntax/readerr
         (prefix-in rash: "rash.rkt"))

(define (rash-read in)
  (syntax->datum (rash-read-syntax #f in)))

(define (rash-read-syntax src in)
  (define-values (start-line start-col start-pos) (port-next-location in))
  (define (list->rash-syntax l span)
    (datum->syntax #f
                   (cons 'start l)
                   (vector src start-line start-col start-pos span)))

  (let loop ([words '()]
             [delta 0])
    (define-values (line col pos) (port-next-location in))
    (cond
     [(regexp-try-match #px"^[[:alnum:]]+" in) =>
      (λ (r)
         (define m (bytes->string/utf-8 (car r)))
         (define len (string-length m))
         (define word (datum->syntax #f m (vector src line col pos len)))
         (loop (cons word words) len))]
     [(regexp-try-match #px"^\n" in) => (λ (m) (list->rash-syntax
                                                (reverse words)
                                                delta))]
     [(regexp-try-match #px"^[ \t]+" in) => (λ (r)
                                               (define m
                                                 (bytes->string/utf-8 (car r)))
                                               (displayln m)
                                               (loop words (string-length m)))]
     [(eof-object? (peek-char in)) (list->rash-syntax (reverse words) delta)]
     [else (raise-read-error
            (string-append "Unknown character " (read-char in))
            src line col pos 1)])))

(module+ test
  (require rackunit)

  (define (2port str)
    (open-input-string str))

  (test-case
   "Read alphanumeric words with EOF"
   (check equal? '(start "echo" "hello1") (rash-read (2port "echo hello1"))))

  (test-case
   "Read string with newline"
   (check equal? '(start "echo") (rash-read (2port "echo\n")))))
