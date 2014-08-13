#lang racket/base

(require racket/port)
(require racket/string)

(provide start)

(define (start command args #:stdin [stdin #f])
  (define cmd (find-executable-path command))
  (unless cmd
    (raise (string-append command ": command not found")))
  (define reverse-args (list cmd
                             (current-error-port)
                             stdin
                             (current-output-port)))
  (define-values (proc out in err)
    (if (null? args)
        (apply subprocess (reverse reverse-args))
        (apply subprocess (reverse (cons (string-join args)
                                         reverse-args)))))
  (subprocess-wait proc))

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
   (check string=? "5" (string-trim (get-output-string stdout)))))
