#lang racket/base

(require racket/contract)
(require racket/port)
(require racket/string)

(define (executable? bin)
  (find-executable-path bin))

(define (executable-list? command)
  (and (find-executable-path (car command))
       (foldl (λ (i j) (and i j)) #t (map string? (cdr command)))))

(provide (contract-out
          [start (->* (executable? (listof string?))
                      (#:stdin (or/c #f file-stream-port?))
                      void?)]
          [pipe (-> (listof string?) (listof string?)
                    void?)]))

(define (start command args #:stdin [stdin #f])
  (define cmd (find-executable-path command))
  (define-values (proc out in err)
    (apply subprocess
           (current-output-port)
           stdin
           (current-error-port)
           cmd
           args))
  (subprocess-wait proc))

(define (pipe command1 command2)
  (define-values (proc-1 out-1 in-1 err-1)
    (apply subprocess
           #f
           #f
           (current-error-port)
           (find-executable-path (car command1))
           (cdr command1)))
  (define-values (proc-2 out-2 in-2 err-2)
    (apply subprocess
           (current-output-port)
           out-1
           (current-error-port)
           (find-executable-path (car command2))
           (cdr command2)))
  (subprocess-wait proc-2))

(module+ test
  (require rackunit)
  (require racket/match)
  (require racket/system)

  (define (launch-racket command stdin)
    (define stdout (open-output-string))
    (match-let ([(list _ _ pid stderr state-fn)
                 (process*/ports stdout
                                 stdin
                                 (current-error-port)
                                 (find-executable-path "racket")
                                 "-e"
                                 command)])
      (state-fn 'wait))
    stdout)

  (test-case
   "Spawn a sub process with current-output-port for stdout and
    current-error-port for stderr. stdin is not used."
   (define stdout (launch-racket
                   "(require \"rash.rkt\") (start \"echo\" '(\"hello\"))"
                   #f))
   (check string=? "hello\n" (get-output-string stdout)))

  (test-case
   "Spawn a sub process with piping in stdin."
   (define stdout (launch-racket
                   "(require \"rash.rkt\") (start \"wc\" '(\"-c\") #:stdin (current-input-port))"
                   (open-input-string "hello")))
   (check string=? "5" (string-trim (get-output-string stdout))))

  (test-case
   "Pipe stdout of one process to the other"
   (define stdout (launch-racket
                   "(require \"rash.rkt\") (pipe '(\"echo\" \"-n\" \"hello\") '(\"wc\" \"-c\"))"
                   #f))
   (check string=? "5" (string-trim (get-output-string stdout)))))
