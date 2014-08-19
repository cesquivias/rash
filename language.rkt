#lang racket/base

(require "rash.rkt")

(provide start
         pipe
         (except-out (all-from-out racket/base)
                     #%app
                     #%top)
         (rename-out [my-app #%app]
                     [my-top #%top]))

(define-syntax-rule (my-top . id)
  'id)

(define-syntax-rule (my-app proc-expr arg ...)
  (if (string? proc-expr)
      (start proc-expr arg ...)
      (proc-expr arg ...)))
