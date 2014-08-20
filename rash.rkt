#lang racket/base

(require racket/contract)
(require racket/port)
(require racket/string)

(require (for-syntax racket/base))

(define (executable? bin)
  (find-executable-path bin))

(define (executable-list? command)
  (and (find-executable-path (car command))
       (foldl (λ (i j) (and i j)) #t (map string? (cdr command)))))

(provide (contract-out
          ;; TODO: Accept any/c for args. Convert to string in function.
          [start (->* (executable?)
                      ()
                      #:rest (listof string?)
                      void?)]
          [start* (->* (executable? (listof string?))
                      (#:stdin (or/c #f file-stream-port?))
                      void?)]
          ;; TODO: validate pipe's commands are lists of strings
          [pipe (->* () () #:rest (listof list?) void?)])
         run
         ~>)

(define (start command . args)
  (start* command args #:stdin (current-input-port)))

(define (start* command args #:stdin [stdin #f])
  (define cmd (find-executable-path command))
  (define-values (proc out in err)
    (apply subprocess
           (current-output-port)
           stdin
           (current-error-port)
           cmd
           args))
  (subprocess-wait proc))

(define-syntax (run stx)
  ;; TODO: Allow string to already be in use
  (syntax-case stx ()
    [(run command arg ...)
     (let ([datum (syntax->datum stx)])
       (datum->syntax stx (apply list
                                 'start
                                 (symbol->string (cadr datum)) ; command
                                 (map symbol->string (cddr datum)) ; args
                                 )))]))

(define-syntax-rule (apply-values fn body ...)
  (call-with-values (λ () body ...) fn))

(struct process (job std-out std-in std-err))

(define (pipe . commands)
  (let loop ([std-in-arg #f]
             [procs '()]
             [commands commands])
    (define command (car commands))
    (define last-process? (= (length commands) 1))
    (define std-out-arg (if last-process?
                            (current-output-port)
                            #f))
    (define proc (apply-values process
                               (apply subprocess std-out-arg
                                      std-in-arg
                                      (current-output-port)
                                      (find-executable-path
                                       (car command))
                                      (cdr command))))
    (if last-process?
        (subprocess-wait (process-job proc))
        (loop (process-std-out proc) (cons proc procs) (cdr commands)))))
  
(define-syntax (~> stx)
  ;; TODO: Add ability to have just a symbol for argument-less commands
  ;; TODO: Allow strings to already be in use for args
  (syntax-case stx ()
    [(~> command ...)
     (datum->syntax stx
                    (cons 'pipe
                          (map (λ (e) (list 'quote (map symbol->string e)))
                               (cdr (syntax->datum stx)))))]))


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
                   "(require \"rash.rkt\") (start \"echo\" \"hello\")"
                   #f))
   (check string=? "hello\n" (get-output-string stdout)))

  (test-case
   "Spawn a sub process with piping in stdin."
   (define stdout (launch-racket
                   "(require \"rash.rkt\") \
                    (start* \"wc\" '(\"-c\") #:stdin (current-input-port))"
                   (open-input-string "hello")))
   (check string=? "5" (string-trim (get-output-string stdout))))

  (test-case
   "Pipe stdout of one process to another"
   (define stdout (launch-racket
                   "(require \"rash.rkt\") \
                    (pipe '(\"echo\" \"-n\" \"hello\") \
                          '(\"cut\" \"-c\" \"3-\") \
                          '(\"wc\" \"-c\"))"
                   #f))
   (check string=? "4" (string-trim (get-output-string stdout))))

  (test-case
   "Using run macro to spawn process"
   (define stdout (launch-racket
                   "(require \"rash.rkt\") (run echo -n hello world)"
                   #f))
   (check string=? "hello world" (get-output-string stdout)))

  (test-case
   "Using ~> macro to pipe processes"
   (define stdout (launch-racket
                   "(require \"rash.rkt\") \
                    (~> (echo hello) (cut -c 3-) (wc -c))"
                   #f))
   (check string=? "4" (string-trim (get-output-string stdout)))))
