(load "~/projects/emacs_lisp/parallel.scm")
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
x

(define s (make-serializer))


(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(list false)


(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))


(define (make-semaphore n)
  (let ((mutex (make-mutex)) (locked 0))
    (define (thi-semafor m)
      (cond ((eq? m 'acquire)
             (if (< locked n)
                 (begin (set! locked (+ locked 1)) (mutex 'release))
                 (mutex 'acquire)))
            ((eq? m 'release)
             (begin
               (set! locked (- locked 1))
               (mutex 'release)
               ))))
    thi-semafor))



(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (taken 0))
    (define (semaphore command)
      (cond ((eq? command 'acquire)
             (if (< taken n)
                 (begin (set! taken (+ 1 taken)) (mutex 'release))
                 (begin (mutex 'acquire))))
            ((eq? command 'release)
             (mutex 'acquire)
             (when (> taken 1)
               (set! taken (- taken 1)))
             (mutex 'release))))
    semaphore))
(define s (make-semaphore 2))
(s 'acquire)
(s 'release)


;;
