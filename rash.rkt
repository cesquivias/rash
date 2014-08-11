#lang racket/base

(require racket/string)

(provide run)

(define (run command . args)
  (let ([cmd (find-executable-path command)]
        [output-port (file-port? (current-output-port))]
        [input-port (file-port? (current-input-port))]
        [error-port (file-port? (current-error-port))])
    (if cmd
        (let-values ([(subproc in out err)
                      (subprocess output-port
                                  input-port
                                  error-port
                                  cmd
                                  (string-join args " "))])
          (subprocess-wait subproc)
          (when (input-port? in)
            (close-input-port in))
          (when (output-port? out)
            (close-output-port out))
          (when (output-port? err)
            (close-output-port err)))
        (raise (string-append command ": command not found")))))

(define (file-port? port)
  (if (file-stream-port? port)
      port
      #f))
