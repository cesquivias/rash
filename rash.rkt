#lang racket/base

(require readline/readline)
(require racket/string)

(define (do command . args)
  (let ([cmd (find-executable-path command)])
    (if cmd
        (let-values ([(subproc in out err)
                      (subprocess (current-output-port)
                                  (current-input-port)
                                  (current-error-port)
                                  cmd
                                  (string-join args " "))])
          (subprocess-wait subproc))
        (raise (exn:fail:filesystem command)))))
