#lang racket/base

(require syntax/readerr
         (prefix-in rash: "language.rkt"))

(provide (rename-out [rash-read read]
                     [rash-read-syntax read-syntax]))

(define (rash-read in)
  (syntax->datum (rash-read-syntax #f in)))

(define (rash-read-syntax src in)
  (define-values (start-line start-col start-pos) (port-next-location in))
  (define (list->rash-syntax l span)
    (datum->syntax #f
                   l
                   (vector src start-line start-col start-pos span)))

  (let loop ([words '()]
             [delta 0])
    (define-values (line col pos) (port-next-location in))
    (define peeked-char (peek-char in))
    (cond
     [(eof-object? peeked-char)
      (read-char in)
      (if (null? words)
          eof
          (list->rash-syntax (reverse words) delta))]
     [(regexp-try-match #px"^[[:alnum:]~>]+" in) =>
      (λ (r)
         (define m (bytes->string/utf-8 (car r)))
         (define len (string-length m))
         (define word (datum->syntax #f m (vector src line col pos len)))
         (loop (cons word words) (+ delta len)))]
     [(regexp-try-match #px"^\n" in) => (λ (m)
                                           (if (null? words)
                                               (loop words (add1 delta))
                                               (list->rash-syntax
                                                (reverse words)
                                                delta)))]
     [(regexp-try-match #px"^[ \t]+" in) =>
      (λ (r)
         (define m (bytes->string/utf-8 (car r)))
         (loop words (+ delta (string-length m))))]
     [(char=? #\( peeked-char)
      (define lst (read in))
      (define stx (datum->syntax #f lst))
      (loop (cons stx words) delta)]
     [(char=? #\" peeked-char)
      (let* ([str (read in)]
             [len (+ 2 (string-length str))]
             [stx (datum->syntax #f str (vector src line col pos len))])
        (loop (cons stx words) (+ delta len)))]
     [else (raise-read-error
            (string-append "Unknown character " (read-char in))
            src line col pos 1)])))

(module+ test
  (require rackunit)

  (define (2port str)
    (open-input-string str))

  (test-case
   "Read alphanumeric words with EOF"
   (check equal? '("echo" "hello1") (rash-read (2port "echo hello1"))))

  (test-case
   "Read string with newline"
   (check equal? '("echo") (rash-read (2port "echo\n"))))

  (test-case
   "Read string"
   (check equal?
          '("echo" "hello world")
          (rash-read (2port "echo \"hello world\""))))

  (test-case
   "Read an s-exp"
   (check equal? '((~> (echo) (wc))) (rash-read (2port "(~> (echo) (wc))")))))
