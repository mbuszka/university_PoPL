(module scheduler (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "queues.scm")
  (require "data-structures.scm")       ; for continuation?
  (require "lang.scm")                  ; for expval?
  
  (provide
    initialize-scheduler!
    set-final-answer! 
    
    time-expired?
    decrement-timer!

    place-on-ready-queue!
    yield-to-queue!
    run-next-thread

    )
  
  ;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;
  
  ;; components of the scheduler state:
  
  (define the-ready-queue   'uninitialized)         
  (define the-final-answer  'uninitialized)
  
  (define the-max-time-slice    'uninitialized)
  (define the-time-remaining    'uninitialized)

  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice) 
      ))
  
  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

  ;; place-on-ready-queue! : Thread -> Unspecified
  ;; Page: 184  
  (define place-on-ready-queue!
    (lambda (th)
      (set! the-ready-queue
        (enqueue the-ready-queue
                 (cons th the-max-time-slice)))))

  (define (yield-to-queue! th)
    (set! the-ready-queue
          (enqueue the-ready-queue
                   (cons th the-time-remaining))))

  ;; run-next-thread : () -> FinalAnswer
  ;; Page: 184    
  (define run-next-thread
    (lambda ()
      (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
          (lambda (th other-ready-threads)
            (set! the-ready-queue other-ready-threads)            
            (set! the-time-remaining (cdr th)) 
            ((car th))
            )))))

  ;; set-final-answer! : ExpVal -> Unspecified
  ;; Page: 184    
  (define set-final-answer!
    (lambda (val)
      (set! the-final-answer val)))

  ;; time-expired? : () -> Bool
  ;; Page: 184    
  (define time-expired?
    (lambda ()
      (zero? the-time-remaining)))

  ;; decrement-timer! : () -> Unspecified
  ;; Page: 184    
  (define decrement-timer!
    (lambda ()
      (set! the-time-remaining (- the-time-remaining 1))))

  )
