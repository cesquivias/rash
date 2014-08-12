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

  (test-case
   "Spawn a sub process with current-output-port for stdout and
    current-error-port for stderr. stdin is not used."
   (let ([stdout (open-output-string)])
     (match-let ([(list _ stdin pid stderr state-fn)
                  (process*/ports
                   stdout
                   #f
                   (current-error-port)
                   (find-executable-path "racket")
                   "-e"
                   "(require \"rash.rkt\") (start \"echo\" '(\"hello\"))")])
       (state-fn 'wait)
       (check string=? "hello\n" (get-output-string stdout)))))

  (test-case
   "Spawn a sub process with piping in stdin."
   (let ([stdout (open-output-string)])
     (match-let ([(list _ _ pid stderr state-fn)
                  (process*/ports
                   stdout
                   (open-input-string "hello")
                   (current-error-port)
                   (find-executable-path "racket")
                   "-e"
                   "(require \"rash.rkt\") (start \"wc\" '(\"-c\") #:stdin (current-input-port))")])
       (state-fn 'wait)
       (check string=? "5" (string-trim (get-output-string stdout))))))
)
